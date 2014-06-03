package fr.proline.module.parser.omssa

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.codehaus.staxmate.SMInputFactory
import org.codehaus.staxmate.in.SMHierarchicCursor
import javax.xml.stream.XMLInputFactory
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.core.om.model.msi.PtmNames
import fr.proline.core.om.model.msi.PtmEvidence
import fr.proline.core.om.model.msi.IonTypes
import fr.proline.core.om.model.msi.Enzyme
import fr.proline.core.om.provider.msi.IResultFileVerifier
import com.typesafe.scalalogging.slf4j.Logging
import scala.io.Source
import java.net.URL

class OmssaResultFileVerifier extends IResultFileVerifier with Logging {

  // returns PtmDefinitions referenced by the specified file
  def getPtmDefinitions(fileLocation: File, importProperties: Map[String, Any]): Seq[PtmDefinition] = {
    //    getPtmDefinitionsByInternalId(getUsermodFileLocation(importProperties)).getValues
    getPtmDefinitionsByInternalId(getUsermodFileLocation(importProperties), parsePtmCompositions(getCompositionsFileLocation(importProperties))).getValues
  }
  // can be used to verify that the provider handle this kind of file (ex: MSMS search, error tolerant search, n15 search, PMF, ...)  
  def isValid(fileLocation: File, importProperties: Map[String, Any]): Boolean = {
    // parser should raise an exception if more than one enzyme is used (impossible with omssa)
    // parser should try to store the enzyme if it is not in the database
    return true
  }
  // TODO ABU
  def getEnzyme(fileLocation: File, importProperties: Map[String, Any]): Array[Enzyme] = {
    // put enzyme omssa ids into a map
    val enzymeIdToName = Map[Int, String](0 -> "trypsin", 1 -> "argc", 2 -> "cnbr", 3 -> "chymotrypsin", 4 -> "formicacid", 5 -> "lysc", 6 -> "lysc-p", 7 -> "pepsin-a", 8 -> "tryp-cnbr",
		9 -> "tryp-chymo", 10 -> "trypsin-p", 11 -> "whole-protein", 12 -> "aspn", 13 -> "gluc", 14 -> "aspngluc", 15 -> "top-down", 16 -> "semi-tryptic", 17 -> "no-enzyme", 18 -> "chymotrypsin-p",
		19 -> "aspn-de", 20 -> "gluc-de", 21 -> "lysn", 22 -> "thermolysin-p", 23 -> "max", 255 -> "none")
	// read omssa file and extract the enzyme id
	val omssaPreloader = new OmssaFilePreloader(fileLocation)
	// create a fake Enzyme with just its name and return it
    val enzyme = new Enzyme(id = -1,
          name = enzymeIdToName.get(omssaPreloader.enzymeId).getOrElse("Unknown"),
          enzymeCleavages = Array.empty,
          cleavageRegexp = None,
          isIndependant = false,
          isSemiSpecific = false,
          properties = None)
    Array(enzyme)
  }

  // get the usermods file from the import properties
  // if missing, take the default usermods file
  private def getUsermodFileLocation(importProperties: Map[String, Any]): java.net.URL = {
    val parseProperties: Map[OmssaParseParams.OmssaParseParam, Any] = importProperties.map(entry => OmssaParseParams.withName(entry._1) -> entry._2)
    if (parseProperties.get(OmssaParseParams.USERMOD_XML_FILE) == None) {
      new File(this.getClass().getClassLoader().getResource("omssa_config/usermods.xml").getPath()).toURL()
    } else {
      new File(parseProperties.get(OmssaParseParams.USERMOD_XML_FILE).get.toString).toURL()
    }
  }

  /**
   * This function reads the xml file using StaxMate
   * It creates the PtmDefinition objects and puts them into the corresponding array (known and unknown)
   * returns a hashmap containing each definition, and the corresponding omssa id
   * -> the omssa Id is not the key of the hashmap because there can be more than one residue per ptm in the omssa file
   */
  //  def getPtmDefinitionsByInternalId(fileLocation: java.net.URL): TwoDimensionsMap[Long, Char, PtmDefinition] = {
  def getPtmDefinitionsByInternalId(fileLocation: java.net.URL, ptmCompositions: Map[String, String]): TwoDimensionsMap[Long, Char, PtmDefinition] = {
    val ptms = new TwoDimensionsMap[Long, Char, PtmDefinition]
    val invalidCompositionString = ""
    if (!fileLocation.getFile().endsWith(".xml")) throw new IllegalArgumentException("Incorrect PTM file type")
    logger.debug("Reading file " + fileLocation.getFile())
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())

    val MSModSpecSet: SMHierarchicCursor = inf.rootElementCursor(fileLocation)
    MSModSpecSet.advance
    // for each mod
    var MSModSpec = MSModSpecSet.childElementCursor().advance()
    while (MSModSpec.getCurrEvent != null) {
      // declare ptm variables
      var id: Option[Long] = None
      var shortName: Option[String] = None
      var omssaType: Option[PtmLocation.Location] = None
      var fullName: Option[String] = None
      var monoMass: Option[Double] = None
      var avgMass: Option[Double] = None
      val residues = new ArrayBuffer[Char]
      var hasNeutralLoss = false
      var unimodId: Option[Long] = None
      var psimsName: Option[String] = None
      var child = MSModSpec.childElementCursor().advance()
      while (child.getCurrEvent() != null) {
        child.getPrefixedName() match {
          case "MSModSpec_mod" =>
            val MSMod = child.childElementCursor().advance()
            shortName = Some(MSMod.getAttrValue("value"))
            id = Some(MSMod.collectDescendantText(false).toLong)
          case "MSModSpec_type"        => omssaType = Some(omssaTypeToPtmLocation(child.childElementCursor().advance().collectDescendantText(false).toInt))
          case "MSModSpec_name"        => fullName = Some(child.collectDescendantText(false))
          case "MSModSpec_monomass"    => monoMass = Some(child.collectDescendantText(false).toDouble)
          case "MSModSpec_averagemass" => avgMass = Some(child.collectDescendantText(false).toDouble)
          case "MSModSpec_residues" =>
            var residue = child.childElementCursor().advance()
            while (residue.getCurrEvent() != null) {
              residues += residue.collectDescendantText(false).charAt(0)
              residue = residue.advance()
            }
          case "MSModSpec_unimod" => unimodId = Some(child.collectDescendantText(false).toLong)
          case "MSModSpec_psi-ms" => psimsName = Some(child.collectDescendantText(false).toString())
          case "MSModSpec_neutralloss" =>
            val neutralloss = child.childElementCursor().advance()
            if (neutralloss.getCurrEvent() != null) {
              var mass = neutralloss.childElementCursor().advance()
              while (mass.getCurrEvent() != null) {
                mass.getPrefixedName() match {
                  case "MSMassSet_monomass"    => monoMass = Some(mass.collectDescendantText(false).toDouble)
                  case "MSMassSet_averagemass" => avgMass = Some(mass.collectDescendantText(false).toDouble)
                  case _                       =>
                }
                mass = mass.advance()
              }
            }
          case _ =>
        }
        child.advance()
      }

      // the short name for usermods is fixed, but the content may change for different users
      if(shortName.isDefined && shortName.get.startsWith("usermod")) {
        shortName = fullName
      }
      if (residues.length == 0) residues += '\0';
      residues.foreach(residue => {
        ptms.update(id.get, residue, new PtmDefinition(
          id = PtmDefinition.generateNewId(),
          ptmId = unimodId.getOrElse(id.getOrElse(0)),
          location = omssaType.get.toString(),
          residue = residue,
          classification = "Post-translational",
          names = new PtmNames(
//            shortName = (if (psimsName.isDefined && psimsName.get.trim() != "") psimsName.get else fullName.getOrElse("")), // psimsName should always be filled, omssa shortname should never be used (especially for usermods)
//            fullName = fullName.getOrElse(psimsName.getOrElse(""))),
            shortName = (if (psimsName.isDefined && psimsName.get.trim() != "") psimsName.get else shortName.getOrElse("-")), // psimsName should always be filled
            fullName = fullName.getOrElse(shortName.getOrElse(""))),
          ptmEvidences = Array(new PtmEvidence(
            ionType = (if (hasNeutralLoss) IonTypes.NeutralLoss else IonTypes.Precursor),
            //            composition = invalidCompositionString,
            composition = ptmCompositions.getOrElse(shortName.getOrElse(""), invalidCompositionString),
            monoMass = monoMass.getOrElse(0),
            averageMass = avgMass.getOrElse(0),
            isRequired = false
          ))
        ))
      })
      MSModSpec = MSModSpec.advance()
    }

    // closing the file
    MSModSpecSet.getStreamReader().closeCompletely()
    ptms
  }

  def getPtmSpecificityById(fileLocation: java.net.URL): Map[Long, Boolean] = {
    val ptmIds = new HashMap[Long, Boolean]()
    if (!fileLocation.getFile().endsWith(".xml")) throw new IllegalArgumentException("Incorrect PTM file type")
    logger.debug("Reading file " + fileLocation.getFile())
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())

    val MSModSpecSet: SMHierarchicCursor = inf.rootElementCursor(fileLocation)
    MSModSpecSet.advance
    // for each mod
    var MSModSpec = MSModSpecSet.childElementCursor().advance()
    while (MSModSpec.getCurrEvent != null) {
      // declare ptm variables
      var id: Long = 0
      var location: String = ""
      var child = MSModSpec.childElementCursor().advance()
      while (child.getCurrEvent() != null) {
        child.getPrefixedName() match {
          case "MSModSpec_mod"         => id = child.childElementCursor().advance().collectDescendantText(false).toLong
          case "MSModSpec_type"        =>
            location = child.childElementCursor().advance().getAttrValue("value")
          case _ =>
        }
        if(location == "modaa" || location == "modnaa" || location == "modcaa" || location == "modnpaa" || location == "modcpaa") {
        	ptmIds.put(id, true)
        } else {
          ptmIds.put(id, false)
        }
        child.advance()
      }
      MSModSpec = MSModSpec.advance()
    }

    // closing the file
    MSModSpecSet.getStreamReader().closeCompletely()
    ptmIds.toMap
  }

  /**
   * PtmLocation is stored as a numeric id in the xml file
   * The name corresponding to the id is given in the OMSSA.xsd file
   * @param typeId the omssa location id given in MSModType
   * @return the PtmLocation corresponding to the id
   */
  private def omssaTypeToPtmLocation(typeId: Int): PtmLocation.Location = {
    typeId match {
      case 0 => PtmLocation.ANYWHERE // modaa : at particular amino acids
      case 1 => PtmLocation.PROT_N_TERM // modn : at the N terminus of a protein 
      case 2 => PtmLocation.PROT_N_TERM // modnaa : at the N terminus of a protein at particular amino acids
      case 3 => PtmLocation.PROT_C_TERM // modc : at the C terminus of a protein
      case 4 => PtmLocation.PROT_C_TERM // modcaa : at the C terminus of a protein at particular amino acids
      case 5 => PtmLocation.ANY_N_TERM // modnp : at the N terminus of a peptide
      case 6 => PtmLocation.ANY_N_TERM // modnpaa : at the N terminus of a peptide at particular amino acids
      case 7 => PtmLocation.ANY_C_TERM // modcp : at the C terminus of a peptide
      case 8 => PtmLocation.ANY_C_TERM // modcpaa : at the C terminus of a peptide at particular amino acids
      case 9 => PtmLocation.ANYWHERE // modmax : the max number of modification types
    }
  }

  /**
   * Temporary method !!
   * This method extracts the URL of the composition file
   * @param importProperties The properties of the parser
   * @return URL of the composition file
   */
  def getCompositionsFileLocation(importProperties: Map[String, Any]): java.net.URL = {
    val parseProperties: Map[OmssaParseParams.OmssaParseParam, Any] = importProperties.map(entry => OmssaParseParams.withName(entry._1) -> entry._2)
    if (parseProperties.get(OmssaParseParams.PTM_COMPOSITION_FILE) != None) {
      new File(parseProperties.get(OmssaParseParams.PTM_COMPOSITION_FILE).get.toString).toURL()
    } else {
      null
    }
  }
  /**
   * Temporary method !!
   * This method extracts the compositions for each ptm in the file
   * @param fileLocation URL of the composition file
   * @return A map of compositions per ptm
   */
  def parsePtmCompositions(fileLocation: java.net.URL): Map[String, String] = {
    val ptmCompositions = new HashMap[String, String]
    try {
      for (line <- Source.fromURL(fileLocation).getLines()) {
        if (line.matches("^.+=.+")) {
          val items = line.split("=", 2)
          ptmCompositions.put(items(0), items(1))
        }
      }
    } catch {
      case e: Exception => logger.error(e.getMessage(), e)
    }
    //    ptmCompositions.foreach { case (k, v) => logger.debug("ABUTEST: "+k+" => "+v) }
    ptmCompositions.toMap
  }
}
