package fr.proline.module.parser.omssa

import java.io.File
import java.io.FileNotFoundException
import java.io.InputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import scala.collection.mutable.HashMap
import org.codehaus.staxmate.SMInputFactory
import org.codehaus.staxmate.in.SMHierarchicCursor
import org.codehaus.staxmate.in.SMInputCursor
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import javax.xml.stream.XMLInputFactory
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.PtmLocation

object OmssaMandatoryFilesLoader {
  val classLoader = OmssaMandatoryFilesLoader.getClass().getClassLoader()
  private val omssaConfigFolder = "omssa_config"
  lazy val xsdFile = new File(classLoader.getResource(omssaConfigFolder + "/OMSSA.xsd").getPath())
  lazy val modFile = new File(classLoader.getResource(omssaConfigFolder + "/mods.xml").getPath())
  lazy val usermodsFile = new File(classLoader.getResource(omssaConfigFolder + "/usermods.xml").getPath())
}

class OmssaMandatoryFilesLoader(val _userptmFilePath: String, val parserContext: ProviderDecoratedExecutionContext) extends Logging {

  private val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
  private var _enzymes: HashMap[Int, String] = null
  def enzymes = _enzymes.toMap
  private var _modificationNames: HashMap[Int, String] = null
  //  private var _ptmDefinitions: HashMap[Long, PtmDefinition] = new HashMap[Long, PtmDefinition]
  //  def ptmDefinitions = _ptmDefinitions.toMap
  private var _ptmDefinitions = new HashMap[PtmDefinition, Long]
  private var _modTypes: HashMap[Int, String] = null
  private var _searchTypes: HashMap[Int, String] = null
  private var _serialDataFormat: HashMap[Int, String] = null
  private var _spectrumFileTypes: HashMap[Int, String] = null
  def spectrumFileTypes = _spectrumFileTypes.toMap
  private var _ionTypes: HashMap[Int, String] = null
  def ionTypes = _ionTypes.toMap
  private var _ionNeutralLosses: HashMap[Int, String] = null
  private var _ionIsotopicTypes: HashMap[Int, String] = null
  private var _hitErrors: HashMap[Int, String] = null
  private var _userAnnots: HashMap[Int, String] = null
  private var _responseErrors: HashMap[Int, String] = null

  def getPtmDefinitions(id: Long): Array[PtmDefinition] = {
    //    _ptmDefinitions.get(id).getOrElse(new ArrayBuffer).toArray
    //    val ptms = new ArrayBuffer[PtmDefinition]
    //    _ptmDefinitions.foreach{ case (_ptm, _id) => 
    //      if(_id == id) ptms += _ptm
    //    }
    //    ptms.toArray
    _ptmDefinitions.filter(_._2 == id).keys.toArray
  }
  def getPtmDefinition(id: Long, site: Char): Option[PtmDefinition] = {
    logger.debug("Looking for PTM " + id + " with residue '" + site + "'")
    val ptm = _ptmDefinitions.filter(_._2 == id).keys.filter(_.residue == site).toArray
    //    val ptm = _ptmDefinitions.get(id).getOrElse(new ArrayBuffer).filter(_.residue == site)
    if (ptm.size != 0) Some(ptm(0)) else None
  }

  parseXsd // parse the xsd file as soon as possible
  parseMods(OmssaMandatoryFilesLoader.modFile)
  if (_userptmFilePath != "") parseMods(new File(_userptmFilePath)) // parse the user's ptms file as soon as possible
  else parseMods(OmssaMandatoryFilesLoader.usermodsFile)

  /**
   * Reads an OMSSA.xsd file that is the reference of the OMSSA omx file
   * The OMSSA.xsd file is bound to a specific version of OMSSA
   * @param xsdFile the file to read
   * @return always true
   */
  private def parseXsd {
    val xsdFile = OmssaMandatoryFilesLoader.xsdFile
    logger.info("reading xsd file '" + xsdFile.getName() + "'")

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
   * @param modFile the ptm file as a parameter for omssa
   * at this point, the ptm file has already been read and all ptms should exist
   * notes about the mods.xml file:
   * - the original file given with omssa2.1.9 has been modified to add unimod identiers
   * - the ptms 110, 195 and 207 has been removed as they do not exist in unimod
   */
  private def parseMods(modFile: File) {
    // this method returns a HashMap[PtmDefinition, Long] object
    (new OmssaResultFileVerifier).getPtmDefinitionsByInternalId(modFile).foreach {
      case (ptm, id) =>
        // add the ptm from the PSdb in the hash, instead of the incomplete ptm from the file
        val _ptm = ptmProvider.getPtmDefinition(ptm.names.shortName, ptm.residue, PtmLocation.withName(ptm.location))
        if (_ptm.isDefined) _ptmDefinitions.put(_ptm.get, id)
        else logger.error("Unknown ptm, will not be considered: " + ptm.toString)
    }
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
