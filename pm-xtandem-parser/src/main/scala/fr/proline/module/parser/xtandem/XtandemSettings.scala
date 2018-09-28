package fr.proline.module.parser.xtandem

import java.io.File
import java.util.Date

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.LazyLogging
import fr.profi.chemistry.model.Enzyme
import fr.proline.core.om.model.msi.FragmentationRuleSet
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.MSMSSearchSettings
import fr.proline.core.om.model.msi.Peaklist
import fr.proline.core.om.model.msi.PeaklistSoftware
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.ResultSetProperties
import fr.proline.core.om.model.msi.SearchSettings
import fr.proline.core.om.model.msi.SeqDatabase
import fr.proline.core.om.model.msi.XTandemImportProperties
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.SeqDbEmptyFakeProvider

object XtandemSettings {
  def getFastaFile(value: String): File = new File(value.replaceAll(".pro$", "")) // xtandem may append ".pro" to a fasta file
}

case class XtandemSettings(resultBioml: XTBioml, parserContext: ProviderDecoratedExecutionContext, 
    instrumentConfig: Option[InstrumentConfig] = None,  fragmentationRuleSetOpt : Option[FragmentationRuleSet] = None, peaklistSoftware: Option[PeaklistSoftware] = None) extends LazyLogging {

  //TODO : Verify if specified FragmentationRuleSet corresponds to XTandem parameters !

  private lazy val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
  private lazy val seqDbProvider = if(parserContext.hasProvider(classOf[ISeqDatabaseProvider])) parserContext.getProvider(classOf[ISeqDatabaseProvider]) else SeqDbEmptyFakeProvider
  
  private lazy val settings: Map[String, String] = {
    val _settings = new HashMap[String, String]
    resultBioml.groupParametersList.foreach(group => {
      group.noteList.foreach(note => {
        _settings.put(note.label, note.info)
      })
    })
    _settings.toMap
  }
  
//  private lazy val ionSeries: Array[String] = {
//    val _ionSeries = new ArrayBuffer[String]
//    if(settings.isDefinedAt("scoring, a ions")) _ionSeries += "a"
//    if(settings.isDefinedAt("scoring, b ions")) _ionSeries += "b"
//    if(settings.isDefinedAt("scoring, c ions")) _ionSeries += "c"
//    if(settings.isDefinedAt("scoring, x ions")) _ionSeries += "x"
//    if(settings.isDefinedAt("scoring, y ions")) _ionSeries += "y"
//    if(settings.isDefinedAt("scoring, z ions")) _ionSeries += "z"
//    _ionSeries.toArray
//  }
  
  lazy val resultSetProperties: ResultSetProperties = {
    val rsProps = new ResultSetProperties
    val rsImportProperties = new XTandemImportProperties
    rsImportProperties.setRawSettings(Some(settings.toMap))
    rsProps.setXtandemImportProperties(Some(rsImportProperties))
    rsProps
  }
  
  private def getSearchStartTime: Date = {
    // date is like "2015:04:27:15:41:28"
    if(settings.isDefinedAt("process, start time")) {
      val items = settings("process, start time").toString.split(":")
      new Date(items(0)+"/"+items(1)+"/"+items(2)+" "+items(3)+":"+items(4)+":"+items(5))
    } else new Date
  }

  private lazy val fastaFiles: Array[SeqDatabase] = {
    val _fastaFiles = new ArrayBuffer[SeqDatabase]
    val nbProteinUsed = settings.getOrElse("modelling, total proteins used", "0").toString.toInt
    
    settings.foreach { case (key, value) => {
      if(key.startsWith("list path, sequence source #")) {
        val fastaFile = XtandemSettings.getFastaFile(value.toString)
        val seqDatabase = seqDbProvider.getSeqDatabase(fastaFile.getName, fastaFile.getPath)
        if(seqDatabase.isDefined)
          _fastaFiles += seqDatabase.get
        else
          _fastaFiles += new SeqDatabase(
              id = SeqDatabase.generateNewId,
              name = fastaFile.getName,
              filePath = fastaFile.getPath,
              sequencesCount = nbProteinUsed, // Problem, this is the total number of proteins in all fasta files
              searchedSequencesCount = nbProteinUsed, // Problem, this is the total number of proteins in all fasta files
              version = "",
              releaseDate = new java.util.Date,
              properties = None,
              searchProperties = None)
      }
    }}
    
    _fastaFiles.toArray
  }
  
  private lazy val usedEnzymes: Array[Enzyme] = {
    val isSemiSpecific = settings.getOrElse("protein, cleavage semi", "no").toString.equals("yes")
    val _enzymes = XTandemEnzymeVerifier.extractEnzymesFromXTandemString(parserContext, settings.getOrElse("protein, cleavage site", "").toString.toUpperCase.replaceAll("[X]", "_"), isSemiSpecific)
    logger.debug("Enzymes: "+_enzymes.map(_.name).mkString(", "))
    _enzymes
  }
  
  private lazy val refinedResults = settings.getOrElse("refine", "no").equals("yes")
  
  private lazy val fixedPtmDefs: Array[PtmDefinition] = {
    XtandemPtmVerifier.getFixedPtms(
        ptmProvider, 
        settings.getOrElse("protein, C-terminal residue modification mass", "").toString, 
        settings.getOrElse("protein, N-terminal residue modification mass", "").toString, 
        settings.getOrElse("residue, modification mass", "").toString, 
        settings.getOrElse("refine, modification mass", "").toString, 
        refinedResults)
  }
  
  private lazy val variablePtmDefs: Array[PtmDefinition] = {
    XtandemPtmVerifier.getVariablePtms(
        ptmProvider, 
        settings.getOrElse("residue, potential modification mass", "").toString, 
        settings.getOrElse("refine, potential modification mass", "").toString, 
        settings.getOrElse("refine, potential C-terminus modifications", "").toString, 
        settings.getOrElse("refine, potential N-terminus modifications", "").toString, 
        settings.getOrElse("protein, quick acetyl", "yes").toString.equals("yes"), 
        settings.getOrElse("protein, quick pyrolidone", "yes").toString.equals("yes"), 
        refinedResults)
  }
  
  lazy val searchSettingsOpt: Option[SearchSettings] = {
    val ms1ChargeStates = List.range(1, settings.getOrElse("spectrum, maximum parent charge", "4").toInt + 1).map(_+"+").mkString(", ")
    val ms1ErrorTolMinusOrPlus = settings.getOrElse("spectrum, parent monoisotopic mass error minus", settings.getOrElse("spectrum, parent monoisotopic mass error plus", 0.0))
    val ms1ToleranceUnit = settings.getOrElse("spectrum, parent monoisotopic mass error units", "Daltons")
    val ms2ToleranceUnit = settings.getOrElse("spectrum, fragment monoisotopic mass error units", "Daltons")
    
    Some(new SearchSettings(
        id = SearchSettings.generateNewId(),
        softwareName = "XTandem",
        softwareVersion = settings.getOrElse("process, version", "Unknown software version").toString,
        taxonomy = settings.getOrElse("protein, taxon", "").toString,
        maxMissedCleavages = settings.getOrElse("scoring, maximum missed cleavage sites", 0).toString.toInt,
        ms1ChargeStates = ms1ChargeStates,
        ms1ErrorTol = ms1ErrorTolMinusOrPlus.toString.toDouble,
        ms1ErrorTolUnit = if(ms1ToleranceUnit.equals("Daltons")) "Da" else "ppm",
        isDecoy = settings.getOrElse("scoring, include reverse", "no").equals("yes"), // Warning, it may be equal to "only" which means there's only decoy matches
        usedEnzymes = usedEnzymes,
        variablePtmDefs = variablePtmDefs,
        fixedPtmDefs = fixedPtmDefs,
        seqDatabases = fastaFiles.toArray,
        instrumentConfig = instrumentConfig.getOrElse(null), // specified by caller  - not available at this moment
        fragmentationRuleSet = fragmentationRuleSetOpt, // specified by caller. TODO read it from parameters ?
        msmsSearchSettings = Some(
          new MSMSSearchSettings(
            ms2ChargeStates = ms1ChargeStates, // not an actual setting, using MS1 value instead
            ms2ErrorTol = settings.getOrElse("spectrum, fragment monoisotopic mass error", 0).toString.toDouble,
            ms2ErrorTolUnit = if(ms2ToleranceUnit.equals("Daltons")) "Da" else "ppm"))))
  }
  
  lazy val msiSearchOpt: Option[MSISearch] = {
    
    val inputFile = new File(settings.getOrElse("spectrum, path", "").toString) // cannot be empty !!
    val outputFile = new File(settings.getOrElse("output, path", "output.xml").toString) // cannot be empty !!
    val nbProteinUsed = settings.getOrElse("modelling, total proteins used", "0").toString.toInt
    
    val msiSearch = new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = outputFile.getName,
      searchSettings = searchSettingsOpt.get,
      peakList = new Peaklist(
          id = Peaklist.generateNewId(),
          fileType = {
            val extension = inputFile.getName.split('.').lastOption.getOrElse("")
            extension.toLowerCase match {
              case "mgf" => "Mascot generic"
              case "dta" => "Sequest"
              case "pkl" => "Micromass"
              case "xml" => "mzML"
              case _ => extension
            }
          },
          path = inputFile.getPath,
          rawFileIdentifier = inputFile.getName.split('.').headOption.getOrElse(inputFile.getName),
          msLevel = 2,
          peaklistSoftware = this.peaklistSoftware.getOrElse(null)),
      date = getSearchStartTime,
      title = outputFile.getName,
      resultFileDirectory = outputFile.getParent,
      jobNumber = 0,
      userName = "",
      userEmail = "",
      queriesCount = settings.getOrElse("modelling, total spectra used", "0").toString.toInt,
      searchedSequencesCount = nbProteinUsed)
    Some(msiSearch)
  }
  
  lazy val hasDecoyResultSet = searchSettingsOpt.get.isDecoy
  
  lazy val hasMs2Peaklist = settings.getOrElse("output, spectra", "yes").equals("yes")

}