package fr.proline.module.parser.omssa

import java.io.File
import java.io.FileNotFoundException
import scala.collection.mutable.HashMap
import org.codehaus.staxmate.in.SMHierarchicCursor
import org.codehaus.staxmate.in.SMInputCursor
import org.codehaus.staxmate.SMInputFactory
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.om.model.msi.IonTypes
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmEvidence
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.core.om.model.msi.PtmNames
import javax.xml.stream.XMLInputFactory
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import java.io.InputStream

//object OmssaMandatoryFilesLoader {
//  val classLoader = OmssaMandatoryFilesLoader.getClass().getClassLoader()
//  private val omssaConfigFolder = "omssa_config"
//  println("###ABU### "+classLoader.getResource(omssaConfigFolder+"/OMSSA.xsd").toURI())
//  def getOmssaXsd(): File = new File(classLoader.getResource(omssaConfigFolder + "/OMSSA.xsd").toURI())
//  def getModsXml(): File = new File(classLoader.getResource(omssaConfigFolder + "/mods.xml").toURI())
//  def getUsermodsXml(): File = new File(classLoader.getResource(omssaConfigFolder + "/usermods.xml").toURI())
//}
object OmssaMandatoryFilesLoader {
  val classLoader = OmssaMandatoryFilesLoader.getClass().getClassLoader()
  private val omssaConfigFolder = "omssa_config"
  def getOmssaXsdFileName(): String = "OMSSA.xsd"
  def getOmssaXsd(): InputStream = classLoader.getResourceAsStream(omssaConfigFolder + "/" + getOmssaXsdFileName())
  def getModsXmlFileName(): String = "mods.xml"
  def getModsXml(): InputStream = classLoader.getResourceAsStream(omssaConfigFolder + "/" + getModsXmlFileName())
  def getUsermodsXmlFileName(): String = "usermods.xml"
  def getUsermodsXml(): InputStream = classLoader.getResourceAsStream(omssaConfigFolder + "/" + getUsermodsXmlFileName())
}

class OmssaMandatoryFilesLoader(val _userptmFilePath: String, val parserContext: ProviderDecoratedExecutionContext) extends Logging {

  private val xsdFile = OmssaMandatoryFilesLoader.getOmssaXsd()
  private val ptmFile = OmssaMandatoryFilesLoader.getModsXml()
  private var userptmFile: InputStream = null
  if (_userptmFilePath != "") {
    logger.info("Usermod file has not been given by the user, default file will be used instead")
    userptmFile = OmssaMandatoryFilesLoader.getUsermodsXml()
  }

  //  private var ptmProvider = entityProviders.ptmProvider
  private val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
  private var _enzymes: HashMap[Int, String] = null
  def enzymes = _enzymes
  private var _modificationNames: HashMap[Int, String] = null
  //  def modificationNames = _modificationNames
  private var _ptmDefinitions: HashMap[Int, PtmDefinition] = new HashMap[Int, PtmDefinition]()
  def ptmDefinitions = _ptmDefinitions
  private var _modTypes: HashMap[Int, String] = null
  //  def modTypes = _modTypes
  private var _searchTypes: HashMap[Int, String] = null
  //  def searchTypes = _searchTypes
  private var _serialDataFormat: HashMap[Int, String] = null
  //  def serialDataFormat = _serialDataFormat
  private var _spectrumFileTypes: HashMap[Int, String] = null
  def spectrumFileTypes = _spectrumFileTypes
  private var _ionTypes: HashMap[Int, String] = null
  def ionTypes = _ionTypes
  private var _ionNeutralLosses: HashMap[Int, String] = null
  //  def ionNeutralLosses = _ionNeutralLosses
  private var _ionIsotopicTypes: HashMap[Int, String] = null
  //  def ionIsotopicTypes = _ionIsotopicTypes
  private var _hitErrors: HashMap[Int, String] = null
  //  def hitErrors = _hitErrors
  private var _userAnnots: HashMap[Int, String] = null
  //  def userAnnots = _userAnnots
  private var _responseErrors: HashMap[Int, String] = null
  //  def responseErrors = _responseErrors

  parseXsd // parse the xsd file as soon as possible
  parseMods(OmssaMandatoryFilesLoader.getModsXmlFileName(), ptmFile, false) // parse the ptm file as soon as possible
  parseMods(OmssaMandatoryFilesLoader.getUsermodsXmlFileName(), userptmFile, false) // parse the user's ptms file as soon as possible

  /**
   * Reads an OMSSA.xsd file that is the reference of the OMSSA omx file
   * The OMSSA.xsd file is bound to a specific version of OMSSA
   * @param xsdFile the file to read
   * @return always true
   */
  private def parseXsd {
    if (xsdFile == null) throw new FileNotFoundException("Invalid specified file")
    logger.info("reading xsd file '" + OmssaMandatoryFilesLoader.getOmssaXsdFileName() + "'")
    // open an input factory
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
    // get the root cursor
    val schema: SMHierarchicCursor = inf.rootElementCursor(xsdFile)
    schema.advance
    // for each child
    var element = schema.childElementCursor().advance()
    while (element.getCurrEvent() != null) {
      if (element.getAttrCount() > 0) {
        element.getAttrValue("name") match {
          case "MSEnzymes"          => _enzymes = _convertEnzymeNames(readEnumerations(element))
          case "MSMod"              => _modificationNames = readEnumerations(element)
          case "MSModType"          => _modTypes = readEnumerations(element)
          case "MSSearchType"       => _searchTypes = readEnumerations(element)
          case "MSSerialDataFormat" => _serialDataFormat = readEnumerations(element)
          case "MSSpectrumFileType" => _spectrumFileTypes = readEnumerations(element)
          case "MSIonType"          => _ionTypes = readEnumerations(element)
          case "MSIonNeutralLoss"   => _ionNeutralLosses = readEnumerations(element)
          case "MSIonIsotopicType"  => _ionIsotopicTypes = readEnumerations(element)
          case "MSHitError"         => _hitErrors = readEnumerations(element)
          case "MSUserAnnot"        => _userAnnots = readEnumerations(element)
          case "MSResponseError"    => _responseErrors = readEnumerations(element)
          case _                    =>
        }
      }
      element.advance()
    }
    schema.getStreamReader().closeCompletely()
  }
  private def readEnumerations(element: SMInputCursor): HashMap[Int, String] = {
    var hash = new HashMap[Int, String]
    var enumeration = element.childElementCursor().advance()
    while (enumeration.getPrefixedName() != "xs:enumeration") enumeration = enumeration.childElementCursor().advance()
    while (enumeration.getCurrEvent() != null) {
      hash.put(enumeration.getAttrValue(1).toInt, enumeration.getAttrValue(0))
      enumeration = enumeration.advance()
    }
    return hash
  }
  
/**
 * This function is made to fit with the enzyme names in the UDS_db
 * It is certainly not the best way, but it's a quick solution to the problem without touching the database
 * TODO A better solution would be to put these names in a file
 * @param omssaEnzymes the hash containing for each enzyme its id and its name read in OMSSA.xsd
 * @return the same hash with correct enzyme names
 */
private def _convertEnzymeNames(omssaEnzymes: HashMap[Int, String]): HashMap[Int, String] = {
    var correctedEnzymes: HashMap[Int, String] = new HashMap[Int, String]
    for (enzyme <- omssaEnzymes) {
      correctedEnzymes.put(
        enzyme._1,
        enzyme._2 match {
          case "trypsin"       => "Trypsin"
          case "trypsin-p"     => "Trypsin/P"
          case "argc"          => "Arg-C"
          case "aspn"          => "Asp-N"
          case "chymotrypsin"  => "Chymotrypsin"
          case "cnbr"          => "CNBr"
          case "tryp-cnbr"     => "CNBr+Trypsin"
          case "formicacid"    => "Formic_acid"
          case "lysc"          => "Lys-C"
          case "lysc-p"        => "Lys-C/P"
          case "lysn"          => "Lys-N"
          case "pepsin-a"      => "PepsinA"
          case "semi-tryptic"  => "semiTrypsin"
          case "tryp-chymo"    => "TrypChymo"
          case "no-enzyme"     => "None"
          case "thermolysin-p" => "thermolysin"
          case _               => enzyme._2
        })
    }
    return correctedEnzymes
  }

  /**
   * Reads a ptm file
   * @param modFile the file that contains ptms
   * @param isUserPtm OMSSA needs mods and usermods in two separate files
   * @return an array of PtmDefinition objects
   */
  //  private def parseMods(modFile: File, isUserPtm: Boolean = false) {
  private def parseMods(modFileName: String, modFile: InputStream, isUserPtm: Boolean = false) {
    //    var ptms = new ArrayBuffer[PtmDefinition]()
    //    if (modFile == null || !modFile.exists()) throw new FileNotFoundException("Invalid specified file")
    //    logger.info("reading ptm file '" + modFile.getName() + "'")
    if (modFile == null) throw new FileNotFoundException("Invalid specified file")
    logger.info("reading ptm file '" + modFileName + "'")
    // open an input factory
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
    // get the root cursor
    val MSModSpecSet: SMHierarchicCursor = inf.rootElementCursor(modFile)
    MSModSpecSet.advance
    // for each mod
    var MSModSpec = MSModSpecSet.childElementCursor().advance()
    while (MSModSpec.getCurrEvent != null) {
      // declare ptm variables
      var modShortName: String = ""; var id: Int = -1; var typeName: String = ""; var typeId: Int = -1
      var monoMass: Double = -1; var avgMass: Double = -1; var n15Mass: Double = -1
      var modFullName: String = ""; var residues: String = ""; var unimod: Int = -1; var psims: String = ""
      var nlMonoMass: Double = -1; var nlAvgMass: Double = -1; var nlN15Mass: Double = -1
      var child = MSModSpec.childElementCursor().advance()
      while (child.getCurrEvent() != null) {
        child.getPrefixedName() match {
          case "MSModSpec_mod" =>
            val MSMod = child.childElementCursor().advance()
            modShortName = MSMod.getAttrValue("value")
            id = MSMod.collectDescendantText(false).toInt
          case "MSModSpec_type" =>
            val MSModType = child.childElementCursor().advance()
            typeName = MSModType.getAttrValue("value")
            typeId = MSModType.collectDescendantText(false).toInt
          case "MSModSpec_name"        => modFullName = child.collectDescendantText(false)
          case "MSModSpec_monomass"    => monoMass = child.collectDescendantText(false).toDouble
          case "MSModSpec_averagemass" => avgMass = child.collectDescendantText(false).toDouble
          case "MSModSpec_n15mass"     => n15Mass = child.collectDescendantText(false).toDouble
          case "MSModSpec_residues" =>
            var residue = child.childElementCursor().advance()
            while (residue.getCurrEvent() != null) {
              residues += residue.collectDescendantText(false)
              residue = residue.advance()
            }
          case "MSModSpec_unimod" => unimod = child.collectDescendantText(false).toInt
          case "MSModSpec_psi-ms" => psims = child.collectDescendantText(false)
          case "MSModSpec_neutralloss" =>
            val neutralloss = child.childElementCursor().advance()
            if (neutralloss.getCurrEvent() != null) {
              var mass = neutralloss.childElementCursor().advance()
              while (mass.getCurrEvent() != null) {
                mass.getPrefixedName() match {
                  case "MSMassSet_monomass"    => nlMonoMass = mass.collectDescendantText(false).toDouble
                  case "MSMassSet_averagemass" => nlAvgMass = mass.collectDescendantText(false).toDouble
                  case "MSMassSet_n15mass"     => nlN15Mass = mass.collectDescendantText(false).toDouble
                }
                mass = mass.advance()
              }
            }
        }
        child.advance()
      }

      // use explicit name if possible
      if (psims != "") modShortName = psims
      // use the parsed information to create PtmEvidence and PtmDefinition objects
      var location: PtmLocation.Location = null
      try {
        location = PtmLocation.withName(ptmTypeExplicitName(typeName))
      } catch {
        case e: Exception => throw new Exception("can't parse OMSSA PTM position constraint")
      }
      // try to get the PTM from the database
      var ptmDefinition = ptmProvider.getPtmDefinition(
        modShortName,
        (if (residues != null && residues != "") residues.charAt(0) else '\0'),
        location)
        // FIXME only use PTMs from the database (for the moment import is not working, and there is not enough information in omssa ptm files)
        // FIXME: the PTMs should already be in the database before importing omssa files
        // There is not enough information in the mod.xml/usermods.xml files to import the PTMs from here !
        // The fix consists in throwing an exception when a peptide matches with an unknown PTM
//      if (ptmDefinition == None) {
//        // create the PTM object if it does not exist in the database
//        if (child.getCurrEvent == null) {
//          val ptmEvidence = new PtmEvidence(
//            ionType = (if (nlMonoMass >= 0) IonTypes.NeutralLoss else IonTypes.Precursor), // default value is precursor
//            composition = "UNVALID", // this information is missing in the ptm file
//            monoMass = monoMass,
//            averageMass = avgMass,
//            isRequired = true)
//          // Get PtmName if exist
//          var ptmIdOpt = ptmProvider.getPtmId(modShortName)
//          var ptmId = (if (ptmIdOpt == Some) ptmIdOpt.get else PtmNames.generateNewId)
//          ptmDefinition = Some(new PtmDefinition(
//            id = PtmDefinition.generateNewId,
//            location = ptmTypeExplicitName(typeName),
//            names = PtmNames(shortName = modShortName, fullName = modFullName),
//            ptmEvidences = Array(ptmEvidence),
//            residue = (if (residues != null && residues != "") residues.charAt(0) else '\0'),
//            classification = "Post-translational",
//            ptmId = ptmId))
//        }
//      }
      if (ptmDefinition != None) {
    	  _ptmDefinitions.put(id, ptmDefinition.get)
      }
      MSModSpec = MSModSpec.advance()
    }

    MSModSpecSet.getStreamReader().closeCompletely()
  }

  /**
   * The type name gives the location of the ptm
   * @param ptmType the OMSSA short name (string identifier)
   * @return the location in Mascot format corresponding to the OMSSA short name
   */
  private def ptmTypeExplicitName(ptmType: String): String = {
    val name: String = ptmType match {
      case "modaa"   => PtmLocation.ANYWHERE.toString() // at particular amino acids
      case "modn"    => PtmLocation.PROT_N_TERM.toString() // at the N terminus of a protein 
      case "modnaa"  => PtmLocation.PROT_N_TERM.toString() // at the N terminus of a protein at particular amino acids
      case "modc"    => PtmLocation.PROT_C_TERM.toString() // at the C terminus of a protein
      case "modcaa"  => PtmLocation.PROT_C_TERM.toString() // at the C terminus of a protein at particular amino acids
      case "modnp"   => PtmLocation.ANY_N_TERM.toString() // at the N terminus of a peptide
      case "modnpaa" => PtmLocation.N_TERM.toString() // at the N terminus of a peptide at particular amino acids
      case "modcp"   => PtmLocation.ANY_C_TERM.toString() // at the C terminus of a peptide
      case "modcpaa" => PtmLocation.C_TERM.toString() // at the C terminus of a peptide at particular amino acids
      case "modmax"  => "" // the max number of modification types
    }
    return name
  }

  /**
   * Indicates whether the tolerance type is in dalton or ppm
   * Default is dalton, ppm is only available since OMSSA 2.1.9
   * @param tolerance true for ppm, dalton otherwise
   * @return ppm or dalton
   */
  def toleranceUnit(tolerance: String): String = {
    val unit: String = tolerance match {
      case "true"  => "Ppm"
      case "false" => "Da"
      case _       => "Da"
    }
    return unit
  }
}
