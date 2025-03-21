package fr.proline.module.parser.mascot

import java.io.File
import java.io.FileInputStream


import scala.io.Source
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.io.RichBufferedSource
import fr.profi.util.regex.RegexUtils.RichString
import fr.proline.core.om.model.msi.ChargeConstraint
import fr.proline.core.om.model.msi.FragmentIonRequirement
import fr.proline.core.om.model.msi.FragmentIonSeries
import fr.proline.core.om.model.msi.Fragmentation
import fr.proline.core.om.model.msi.FragmentationRule
import fr.proline.core.om.model.msi.FragmentationRuleSet
import fr.proline.core.om.model.msi.Instrument
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.model.msi.RequiredSeries
import fr.proline.core.om.model.msi.RequiredSeriesQualityLevel._
import fr.proline.module.parser.mascot.MascotDataParser.LATIN_1_CHARSET

import scala.collection.mutable

/**
 * @author David Bouyssie
 *
 */
object MascotFragmentationRuleParser extends LazyLogging  {

  val mascotInfoPerFile = new mutable.HashMap[File, (Array[FragmentationRuleSet],Array[InstrumentConfig])]()

  def getFragmentationRuleSets(fileLocation: File): Array[FragmentationRuleSet] = {
    require((fileLocation != null) && fileLocation.isFile, "Invalid fileLocation")
    if(!mascotInfoPerFile.contains(fileLocation))
      loadInfoFromFile(fileLocation)
    mascotInfoPerFile(fileLocation)._1
  }

  def getInstrumentConfigs(fileLocation: File): Array[InstrumentConfig] = {
    require((fileLocation != null) && fileLocation.isFile, "Invalid fileLocation")
    if(!mascotInfoPerFile.contains(fileLocation))
      loadInfoFromFile(fileLocation)
    mascotInfoPerFile(fileLocation)._2
  }

  def loadInfoFromFile(fileLocation: File)  {

    val absolutePathname = fileLocation.getAbsolutePath

    val is = new FileInputStream(fileLocation)
    val fragRuleSets = Array.newBuilder[FragmentationRuleSet]
    val instConfigs = Array.newBuilder[InstrumentConfig]
    try {
           // Force ANSI ISO-8859-1 to read Mascot .dat files
      Source.fromInputStream(is, LATIN_1_CHARSET).eachLine("*", block => {

        val lines = block.split("\r\n")
        val titleLine = lines.find(_ =~ "^title:.+")

        if (titleLine.isDefined) {
          val instrumentType = titleLine.get.split(":")(1)

        val ruleNumbers: Array[Int] = lines.filter { _ =~ """^\d+.+""" }.map{ l => (l =# """^(\d+).+""").get.group(1).toInt }

          val (fragmentationRuleSet, instConfig) = this._buildFragmentationRuleSetAndInstConfig(instrumentType, ruleNumbers)
          if (fragmentationRuleSet.isDefined) fragRuleSets += fragmentationRuleSet.get
          if (instConfig.isDefined) instConfigs += instConfig.get
        }

        ()
      })

    } finally {
      mascotInfoPerFile += (fileLocation -> (fragRuleSets.result(),instConfigs.result()))
      try {
        is.close()
      } catch {
        case exClose: Exception => logger.error("Error closing [" + absolutePathname + ']', exClose)
      }

    }

  }

  private def _buildFragmentationRuleSetAndInstConfig(
    instrumentType: String,
    ruleNumbers: Array[Int]
  ): (Option[FragmentationRuleSet], Option[InstrumentConfig]) = {

        // Define some vars
    var analyzers = ("", "")
    var (instSource, activationType) = ("ESI", "CID")
//
    // Retrieve source
    val instrumentParts = instrumentType.split("-")
    if (instrumentParts.length == 1) return (None, None) // skip Default and All
    else {

      if (instrumentParts(0) == "ESI" || instrumentParts(0) == "MALDI") instSource = instrumentParts(0)
      else if (instrumentParts(0) == "ETD") activationType = "ETD"

      // Retrieve activation type
      if (instrumentParts(1) == "ECD") activationType = "ECD"
      else if (instrumentParts.length == 3 && instrumentParts(2) == "PSD") activationType = "PSD"

      if (instrumentType endsWith "TOF-TOF") { analyzers = ("TOF", "TOF") }
      else if (instrumentType endsWith "QUAD-TOF") { analyzers = ("QUAD", "TOF") }
      else if (instrumentType endsWith "QUIT-TOF") { analyzers = ("QUIT", "TOF") }
      else if (instrumentType == "FTMS-ECD") { analyzers = ("FTMS", "FTMS") }
      else { analyzers = (instrumentParts(1), instrumentParts(1)) }
    }

    // Create new instrument
    val instrument = new Instrument(
      id = Instrument.generateNewId(),
      name = instrumentType,
      source = instSource
    )

    // Create new instrument config
    val instrumentConfig = new InstrumentConfig(
      id = InstrumentConfig.generateNewId(),
      name = instrumentType,
      instrument = instrument,
      ms1Analyzer = analyzers._1,
      msnAnalyzer = analyzers._2,
      activationType = activationType
    )

    // Retrieve fragmentation rules of the instrument configuration
    val fragRules = MascotFragmentation.rules
    val instFragRules = ruleNumbers.map(i => fragRules(i - 1))

    // Create new instrument config
    val fragRuleSet = new FragmentationRuleSet(
      id = FragmentationRuleSet.generateNewId(),
      name = instrumentType,
      fragmentationRules = instFragRules
    )

    ( Some(fragRuleSet), Some(instrumentConfig))
  }

}

object MascotFragmentation {
  
  

  val rules: Array[FragmentationRule] = {

    val ionTypes = Fragmentation.defaultIonTypes
    val ionTypeMap = Map() ++ ionTypes.map { ionType => ionType.toString -> ionType }
    val ionSeries = FragmentIonSeries

    // Create fragmentation rules
    Array(
      ChargeConstraint(
        description = "singly charged",
        fragmentCharge = 1
      ),
      ChargeConstraint(
        description = "doubly charged if precursor 2+ or higher (not internal or immonium)",
        fragmentCharge = 2,
        precursorMinCharge = Some(2)
      ),
      ChargeConstraint(
        description = "doubly charged if precursor 3+ or higher (not internal or immonium)",
        fragmentCharge = 2,
        precursorMinCharge = Some(3)
      ),
      FragmentIonRequirement(
        description = "immonium",
        ionType = ionTypeMap("immonium")
      ),
      FragmentIonRequirement(
        description = "a series",
        ionType = ionTypeMap("a")
      ),
      FragmentIonRequirement(
        description = "a - NH3 if a significant and fragment includes RKNQ",
        ionType = ionTypeMap("a-NH3"),
        requiredSeries = ionSeries.a,
        requiredSeriesQualityLevel = SIGNIFICANT,
        residueConstraint = Some("RKNQ")
      ),
      FragmentIonRequirement(
        description = "a - H2O if a significant and fragment includes STED",
        ionType = ionTypeMap("a-H2O"),
        requiredSeries = ionSeries.a,
        requiredSeriesQualityLevel = SIGNIFICANT,
        residueConstraint = Some("STED")
      ),
      FragmentIonRequirement(
        description = "b series",
        ionType = ionTypeMap("b")
      ),
      FragmentIonRequirement(
        description = "b - NH3 if b significant and fragment includes RKNQ",
        ionType = ionTypeMap("b-NH3"),
        requiredSeries = ionSeries.b,
        requiredSeriesQualityLevel = SIGNIFICANT,
        residueConstraint = Some("RKNQ")
      ),
      FragmentIonRequirement(
        description = "b - H2O if b significant and fragment includes STED",
        ionType = ionTypeMap("b-H2O"),
        requiredSeries = ionSeries.b,
        requiredSeriesQualityLevel = SIGNIFICANT,
        residueConstraint = Some("STED")
      ),
      FragmentIonRequirement(
        description = "c series",
        ionType = ionTypeMap("c")
      ),
      FragmentIonRequirement(
        description = "x series",
        ionType = ionTypeMap("x")
      ),
      FragmentIonRequirement(
        description = "y series",
        ionType = ionTypeMap("y")
      ),
      FragmentIonRequirement(
        description = "y - NH3 if y significant and fragment includes RKNQ",
        ionType = ionTypeMap("y-NH3"),
        requiredSeries = ionSeries.y,
        requiredSeriesQualityLevel = SIGNIFICANT,
        residueConstraint = Some("RKNQ")
      ),
      FragmentIonRequirement(
        description = "y - H2O if y significant and fragment includes STED",
        ionType = ionTypeMap("y-H2O"),
        requiredSeries = ionSeries.y,
        requiredSeriesQualityLevel = SIGNIFICANT,
        residueConstraint = Some("STED")
      ),
      FragmentIonRequirement(
        description = "z series",
        ionType = ionTypeMap("z")
      ),
      FragmentIonRequirement(
        description = "internal yb < 700 Da",
        ionType = ionTypeMap("yb"),
        fragmentMaxMoz = Some(700f)
      ),
      FragmentIonRequirement(
        description = "internal ya < 700 Da",
        ionType = ionTypeMap("ya"),
        fragmentMaxMoz = Some(700f)
      ),
      RequiredSeries(
        description = "y or y++ must be significant",
        requiredSeries = ionSeries.y,
        requiredSeriesQualityLevel = SIGNIFICANT
      ),
      RequiredSeries(
        description = "y or y++ must be highest scoring series",
        requiredSeries = ionSeries.y,
        requiredSeriesQualityLevel = HIGHEST_SCORING
      ),
      FragmentIonRequirement(
        description = "z+1 series",
        ionType = ionTypeMap("z+1")
      ),
      FragmentIonRequirement(
        description = "d and d' series",
        ionType = ionTypeMap("d")
      ),
      FragmentIonRequirement(
        description = "v series",
        ionType = ionTypeMap("v")
      ),
      FragmentIonRequirement(
        description = "w and w' series",
        ionType = ionTypeMap("w")
      ),
      FragmentIonRequirement(
        description = "z+2 series",
        ionType = ionTypeMap("z+2")
      )
    )
  }

}