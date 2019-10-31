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
import com.typesafe.scalalogging.LazyLogging
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import javax.xml.stream.XMLInputFactory
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.PtmLocation


//class OmssaMandatoryFilesLoader(val _userptmFilePath: String, val parserContext: ProviderDecoratedExecutionContext) extends Logging {
class OmssaMandatoryFilesLoader(val _userptmFilePath: String, val ptmCompositionFilePath: String, val parserContext: ProviderDecoratedExecutionContext) extends LazyLogging {

  private val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
  private var _enzymes: HashMap[Int, String] = null
  def enzymes = _enzymes.toMap
  private var _modificationNames: HashMap[Int, String] = null
  private var _ptmDefinitions = new TwoDimensionsMap[Long, Char, PtmDefinition]
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
  private val _ptmSpecificity = new HashMap[Long, Boolean]

  def getPtmDefinitions(id: Long): Array[PtmDefinition] = {
    _ptmDefinitions.wrapped.filter(_._1._1 == id).values.toArray
  }
  def getPtmDefinition(id: Long, site: Char): Option[PtmDefinition] = {
    if(ptmExpectsResidue(id)) {
      _ptmDefinitions.getOption(id, site)
    } else {
      _ptmDefinitions.getOption(id, '\0')
    }
  }
  private def ptmExpectsResidue(id: Long): Boolean = _ptmSpecificity.get(id).get

  parseXsd // parse the xsd file as soon as possible
  parseMods(this.getClass().getClassLoader().getResource("omssa_config/mods.xml"))
  if (_userptmFilePath != "") parseMods((new File(_userptmFilePath)).toURL()) // parse the user's ptms file as soon as possible
  else parseMods(this.getClass().getClassLoader().getResource("omssa_config/usermods.xml"))
  
  /**
   * Reads an OMSSA.xsd file that is the reference of the OMSSA omx file
   * The OMSSA.xsd file is bound to a specific version of OMSSA
   * @return always true
   */
  private def parseXsd {
    logger.info("reading xsd file 'OMSSA.xsd'")

    // open an input factory
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
    // get the root cursor
    val schema: SMHierarchicCursor = inf.rootElementCursor(this.getClass().getClassLoader().getResource("omssa_config/OMSSA.xsd"))
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
          case "chymotrypsin-p"=> "Chymotrypsin"
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
          case _               => enzyme._2 // unknown so far : whole-protein, gluc, aspngluc, top-down, chymotrypsin, aspn-de, gluc-de, thermolysin-p
        })
    }
    return correctedEnzymes
  }

  /**
   * @param modFile the ptm file as a parameter for omssa
   * at this point, the ptm file has already been read and all ptms should exist
   * notes about the mods.xml file:
   * - the original file given with omssa2.1.9 has been modified to add unimod identifiers (when missing)
   * - the ptms 110, 195 and 207 has been removed as they do not exist in unimod
   */
  private def parseMods(modFile: java.net.URL) {
    val verifier = new OmssaResultFileVerifier
    verifier.getPtmDefinitionsByInternalId(modFile, verifier.parsePtmCompositions(new File(ptmCompositionFilePath).toURL())).foreach((key, ptm) => {
      val _ptm = ptmProvider.getPtmDefinition(ptm.names.shortName, ptm.residue, PtmLocation.withName(ptm.location))
      if (_ptm.isDefined) _ptmDefinitions.update(key._1, key._2, _ptm.get)
      else logger.error("Unknown ptm, will not be considered: " + ptm.toString)
    })
    verifier.getPtmSpecificityById(modFile).foreach(s => {
      _ptmSpecificity.put(s._1, s._2)
    })
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
