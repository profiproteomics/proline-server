package fr.proline.module.parser.mascot

import java.io.{ FileNotFoundException, File }
import java.lang.{ UnsatisfiedLinkError, System }
import java.net.URLDecoder
import scala.Array.canBuildFrom
import scala.collection.mutable.{ HashMap, ArrayBuffer }

import com.typesafe.scalalogging.slf4j.Logging

import matrix_science.msparser.{ ms_searchparams, ms_peptidesummary, ms_mascotresults, ms_mascotresfile, ms_inputquery }

import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.msi.{ ISeqDatabaseProvider, IProteinProvider, IPeptideProvider, IPTMProvider }
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.algo.msi.validation.MascotValidationHelper
import fr.profi.util.primitives._

import matrixscience.NativeLibrariesLoader

object MascotScores extends Enumeration {
  type ScoreType = Value
  val MudPIT = Value("mudpit")
  val Standard = Value("standard")
}

object MascotParseParams extends Enumeration {
  type MascotParseParam = Value
  val ION_SCORE_CUTOFF = Value("ion.score.cutoff")
  val SUBSET_THRESHOLD = Value("subset.threshold")
  val PROTEIN_CUTOFF_PVALUE = Value("protein.cutoff.pvalue")
  val MASCOT_VERSION = Value("mascot.version")
  val MASCOT_SERVER_URL = Value("mascot.server.url")
}

/**
 * Create a MascotResultFile to parse specified Mascot identification result file.
 * mascot parse parameters are specified in parseProperties
 *
 * @param fileLocation :  Mascot identification result file to parse
 * @param importProperties : parameters to use for parsing. Allowed values are those specified by MascotParseParams. Should not be null.
 * Empty Map could be specified if default value should be used.
 */
class MascotResultFile(
  val fileLocation: File,
  val importProperties: Map[String, Any], // TODO: use the MascotImportProperties class instead ???
  val parserContext: ProviderDecoratedExecutionContext) extends IResultFile with Logging {

  val LOG_SPECTRA_COUNT = 4000 // Print a log for each created spectrum

  // ---- For Java developers ;) : --- Constructor Code 

  // Requirements
  require(fileLocation != null, "FileLocation is null")

  val fileAbsolutePath = fileLocation.getAbsolutePath

  require(fileLocation.isFile, "Invalid fileLocation : " + fileAbsolutePath)

  logger.info("Opening Mascot result file : " + fileAbsolutePath)

  require(importProperties != null)

  private val m_closeLock = new Object()

  private var _isClosed = false // Mutable field guarded by m_closeLock

  val parseProperties: Map[MascotParseParams.MascotParseParam, Any] = importProperties.map(entry => MascotParseParams.withName(entry._1) -> entry._2)
  val rsImportProperties = new MascotImportProperties()
  rsImportProperties.setIonsScoreCutoff(parseProperties.get(MascotParseParams.ION_SCORE_CUTOFF).map(toFloat(_)))
  rsImportProperties.setProteinsPvalueCutoff(parseProperties.get(MascotParseParams.PROTEIN_CUTOFF_PVALUE).map(toFloat(_)))
  rsImportProperties.setSubsetsThreshold(parseProperties.get(MascotParseParams.SUBSET_THRESHOLD).map(toFloat(_)))

  /* Load Matrix Science Mascot Parser native libraries */
  val loaded = NativeLibrariesLoader.loadNativeLibraries
  if (!loaded) {
    val message = "Mascot Parser native libraries not loaded"
    logger.error(message)

    throw new RuntimeException(message)
  }

  // Create Mascot ms_mascotresfile and ms_peptidesummary from specified file name 
  private val mascotResFile: ms_mascotresfile = new ms_mascotresfile(fileAbsolutePath, 0, "")
  if (!mascotResFile.isValid()) throw new Exception("Invalid Mascot result file " + fileLocation.getAbsolutePath() + " specified")

  private val mascotSearchParams: ms_searchparams = mascotResFile.params()

  /** IResultFile values **/
  val msLevel: Int = 2 // Disable support for PMF data  
  val hasMs2Peaklist: Boolean = true // A Mascot dat file has always a peaklist
  val hasDecoyResultSet = if (mascotSearchParams.getDECOY == 1) true else false

  // ---- For Java developers ;) : --- END Constructor Code 

  //--- Lazy attributes --- //
  lazy val ptmHelper: MascotPTMHelper = {
    val helper = new MascotPTMHelper(parserContext)

    // Retrieve PTMs, fixed and variable, specified for Mascot Search
    val mascotVarModsAsStr = mascotSearchParams.getIT_MODS()
    logger.debug("Search variable Ptms using Mascot string : " + mascotVarModsAsStr)
    val varPtmDefsByModName = helper.createVarPtmDefs(mascotVarModsAsStr)
    logger.debug("PTMProvider found " + varPtmDefsByModName.size + " variables PTMs")

    val mascotFixedModsAsStr = mascotSearchParams.getMODS()
    logger.debug("Search fixed Ptms using Mascot string : " + mascotFixedModsAsStr)
    val fixedPtmDefsByModName = helper.createFixedPtmDefs(mascotFixedModsAsStr)
    logger.debug("PTMProvider found " + fixedPtmDefsByModName.size + " fixed PTMs")

    helper
  }

  /**
   * Target PepSummary read from Mascot ResultFile
   */
  private var _isTargetSummaryLoaded = false
  private lazy val targetPepSummary = {
    _isTargetSummaryLoaded = true
    _buildPepSummary(false)
  }

  /**
   * Decoy PepSummary read from Mascot ResultFile
   */
  private var _isDecoySummaryLoaded = false
  private lazy val decoyPepSummary = {
    _isDecoySummaryLoaded = true
    _buildPepSummary(true)
  }

  /**
   *  Map Query Id -> MsQuery for all queries of Mascot result file
   *  Load the MS queries here when requested
   *
   */
  lazy val msQueryByInitialId: Map[Int, MsQuery] = {

    import java.net.URLDecoder

    val msQueryMapBuilder = scala.collection.immutable.Map.newBuilder[Int, MsQuery]
    val nbrQueries: Int = mascotResFile.getNumQueries()

    def getMsQueryDbSearchProps(pepSummaryOpt: Option[ms_peptidesummary], msQueryNum: Int) = {
      if (pepSummaryOpt == None) None
      else {
        val pepSum = pepSummaryOpt.get
        var ht: Float = mascotResFile.getSectionValueDouble(ms_mascotresfile.SEC_SUMMARY, "qplughole" + msQueryNum).toFloat
        val candidatePepCount = pepSum.getQmatch(msQueryNum)
        //val it = pepSum.getPeptideIdentityThreshold(msQueryNum,20)
        val it = if (candidatePepCount > 0) Some(MascotValidationHelper.calcIdentityThreshold(candidatePepCount, 0.05)) else None

        Some(
          MsQueryDbSearchProperties(
            candidatePeptidesCount = pepSum.getQmatch(msQueryNum),
            mascotIdentityThreshold = it,
            mascotHomologyThreshold = Some(ht)
          )
        )
      }
    }

    var fileComesFromPklInput = false
    for (q <- 1 to nbrQueries) { // Go through each Query
      //  AW/ABU: ticket #10344 fix
//      var specTitle = s"Cmpd ${q}, +MSn(${mascotResFile.getObservedMass(q)}), ? min" 
      var specTitle = "Cmpd "+q+", +MSn("+mascotResFile.getObservedMass(q)+"), ? min" // back to a simple format because query number was always at '0'
      try {
        val tmp = URLDecoder.decode(mascotResFile.getQuerySectionValueStr(q, "title"), "UTF-8").replace('\\', '/') //WorkAround for \ char in spectrum storer  !
        if(!tmp.trim().equals("")) {
          specTitle = tmp
        }
      } catch {
        case ex: Exception => fileComesFromPklInput = true
      }
      // ----------------- 

      val msQueryProps = new MsQueryProperties(
        targetDbSearch = getMsQueryDbSearchProps(targetPepSummary, q),
        decoyDbSearch = getMsQueryDbSearchProps(decoyPepSummary, q)
      )

      var query = new Ms2Query(
        id = Ms2Query.generateNewId,
        initialId = q,
        moz = mascotResFile.getObservedMass(q),
        charge = mascotResFile.getObservedCharge(q),
        spectrumTitle = specTitle,
        properties = Some(msQueryProps)
      )

      msQueryMapBuilder += (q -> query)
    }
    if(fileComesFromPklInput) {
      logger.info("Peaklist format looks like PKL, spectrum fake titles have been generated")
    }

    msQueryMapBuilder.result()

  }

  lazy val peaklist: Peaklist = {
    new Peaklist(
      id = Peaklist.generateNewId,
      fileType = mascotSearchParams.getFORMAT(), // TODO: check file extension first (ex: .raw)
      path = mascotSearchParams.getFILENAME(),
      rawFileName = "", // TODO: retrieve this
      msLevel = 2
    )
  }

  /**
   * Create from mascot parser ms_mascotresfile, a MSISearch with all associated information :
   * Peaklist, Enzyme, SeqDatabase and SearchSettings
   */
  lazy val msiSearch: MSISearch = {

    // Get MSISearch information : Retrieve peak list, search parameters...
    logger.info("Parse Marcot Search Settings information ...")

    val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
    val searchParams = mascotSearchParams

    val nbrSearchedQueries = mascotResFile.getNumSeqsAfterTax(0) //Get num of seq in all dbs after taxonomy filter

    // Verify used SeqDatabases specified for Mascot Search exist, else Stop and throw an exception
    var nbrDBs = searchParams.getNumberOfDatabases()
    logger.debug("Search for " + nbrDBs + " Sequences database(s) used in Mascot result file")

    var seqDbs = new Array[SeqDatabase](nbrDBs)
    for (dbIndex <- 1 until (nbrDBs + 1)) {
      var dbName = searchParams.getDB(dbIndex)
      var filePath = mascotResFile.getFastaPath(dbIndex)

      val seqDbProvider = parserContext.getProvider(classOf[ISeqDatabaseProvider])
      var usedSeqSDb = seqDbProvider.getSeqDatabase(dbName, filePath)

      logger.debug("Sequence database " + dbIndex + ": " + dbName + ", " + filePath + " found ? = " + usedSeqSDb)
      if (usedSeqSDb != None)
        seqDbs.update(dbIndex - 1, usedSeqSDb.get)
      else {
        val msg = "Sequence database used for identification does not exist in Proline datastore. Please load this database in proline datastore"
        logger.warn(msg)
        seqDbs.update(
          dbIndex - 1,
          new SeqDatabase(
            id = SeqDatabase.generateNewId,
            name = dbName,
            filePath = filePath,
            sequencesCount = mascotResFile.getNumSeqs(dbIndex),
            releaseDate = new java.util.Date
          )
        )
      }
    }

    // Create Peaklist & Enzymes
    val enzymes = Array(new Enzyme(searchParams.getCLE()))

    //Create MSISearch regrouping all these information   
    var sSettings: SearchSettings = new SearchSettings(
      id = SearchSettings.generateNewId(),
      softwareName = "Mascot",
      softwareVersion = mascotResFile.getMascotVer(),
      taxonomy = searchParams.getTAXONOMY(),
      maxMissedCleavages = searchParams.getPFA(),
      ms1ChargeStates = searchParams.getCHARGE(),
      ms1ErrorTol = searchParams.getTOL(),
      ms1ErrorTolUnit = searchParams.getTOLU(),
      isDecoy = false,
      usedEnzymes = enzymes,
      variablePtmDefs = ptmHelper.varPtmDefsByModName.values.flatMap { p => p } toArray,
      fixedPtmDefs = ptmHelper.fixedPtmDefsByModName.values.flatMap { p => p } toArray,
      seqDatabases = seqDbs,
      instrumentConfig = instrumentConfig.getOrElse(null),
      quantitation = ""
    )

    if (msLevel == 2) {
      sSettings.msmsSearchSettings = Some(
        new MSMSSearchSettings(
          ms2ChargeStates = searchParams.getCHARGE(), // TODO: check this param
          ms2ErrorTol = searchParams.getITOL(),
          ms2ErrorTolUnit = searchParams.getITOLU())
      )
    }

    val nbQueries = mascotResFile.getNumQueries()
    new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = fileLocation.getName(), //mascotResFile.getFileName(),
      submittedQueriesCount = nbQueries,
      searchSettings = sSettings,
      peakList = peaklist,
      title = searchParams.getCOM(),
      date = new java.util.Date(mascotResFile.getDate().toLong * 1000),
      resultFileDirectory = fileLocation.getParentFile().getAbsolutePath(),
      queriesCount = nbQueries,
      jobNumber = mascotResFile.getJobNumber(),
      userName = searchParams.getUSERNAME(),
      userEmail = searchParams.getUSEREMAIL,
      searchedSequencesCount = nbrSearchedQueries
    )
  }

  // TODO: replace by getMascotResults( wantDecoy: Boolean ): Option[ms_mascotresults]
  private def _buildPepSummary(wantDecoy: Boolean): Option[ms_peptidesummary] = {

    if (!hasDecoyResultSet && wantDecoy) return None

    case object MascotResFlags {
      val Group = ms_mascotresults.MSRES_GROUP_PROTEINS
      val Subsets = ms_mascotresults.MSRES_SHOW_SUBSETS
      val MudPIT = ms_mascotresults.MSRES_MUDPIT_PROTEIN_SCORE
      val Decoy = ms_mascotresults.MSRES_DECOY
    }

    var flagCombination = MascotResFlags.Group | MascotResFlags.Subsets

    val scoreType = MascotScores.MudPIT // TODO : retrieve from input parameters
    if (scoreType == MascotScores.MudPIT) { flagCombination = flagCombination | MascotResFlags.MudPIT }
    if (wantDecoy) { flagCombination = flagCombination | MascotResFlags.Decoy }

    val (minProtProb, maxHitsToReport) = (0, 0) // Report All proteins
    val ignoreIonsScoreBelow = rsImportProperties.ionsScoreCutoff.getOrElse(0f).toDouble // Specify ion score cut off
    val (unigeneIndexFile, minPepLenInPepSummary, singleHit, flags2) = (null, 0, null, ms_peptidesummary.MSPEPSUM_NO_PROTEIN_GROUPING)

    val peptideSummary = new ms_peptidesummary(
      mascotResFile,
      flagCombination,
      minProtProb,
      maxHitsToReport,
      unigeneIndexFile,
      ignoreIonsScoreBelow,
      minPepLenInPepSummary,
      singleHit,
      flags2
    )
    // subSetThreshold - is the fractional score required for a protein to be counted as a subset. 
    // Its score must be greater than or equal to main_protein_score * (1-scoreFraction)
    // The default value is 1.0  
    peptideSummary.setSubsetsThreshold(rsImportProperties.getSubsetsThreshold().getOrElse(1.0f).toDouble)

    Some(peptideSummary)

    //*** To test different creation modes : No Protein Group ?!   
    //new ms_peptidesummary(mascotResFile, ms_mascotresults.MSRES_GROUP_PROTEINS,
    //  0, 0, null, 0, 0, null, ms_peptidesummary.MSPEPSUM_NO_PROTEIN_GROUPING)

    //    pepSummary = new ms_peptidesummary(mascotResFile, ms_mascotresults.MSRES_GROUP_PROTEINS | ms_mascotresults.MSRES_SHOW_SUBSETS,
    //      0.05, Integer.MAX_VALUE, null, 0, 0, null)
  }

  private def _getPepSummary(wantDecoy: Boolean): ms_peptidesummary = {

    // Retrieve peptide summary corresponding to the wanted dataset
    val pepSummary = if (wantDecoy) decoyPepSummary else targetPepSummary
    if (pepSummary == None) {
      throw new Exception("can't load the peptide summary")
    }

    pepSummary.get
  }

  /**
   * Free all ms_parser attached objects
   */
  def close() {
    doClose(false)
  }

  override def finalize() {

    try {

      try {
        doClose(true)
      } catch {
        case ex: Exception => logger.error("Error closing MascotResultFile", ex)
      }

    } finally {
      super.finalize()
    }

  }

  private def doClose(fromFinalize: Boolean) {

    m_closeLock.synchronized {

      if (!_isClosed) {
        _isClosed = true

        if (fromFinalize) {
          logger.warn("Closing MascotResultFile resources from finalize")
        } else {
          logger.debug("Closing MascotResultFile resources ...")
        }

        /* Free memory (reverse order) */

        if (_isDecoySummaryLoaded && decoyPepSummary.isDefined) {
          val decoyPepSumValue = decoyPepSummary.get

          try {
            decoyPepSumValue.delete()
          } catch {
            case t: Throwable => logger.error("Error deleting decoyPepSumValue value", t)
          }

        }

        if (_isTargetSummaryLoaded && targetPepSummary.isDefined) {
          val targetPepSumValue = targetPepSummary.get

          try {
            targetPepSumValue.delete()
          } catch {
            case t: Throwable => logger.error("Error deleting targetPepSumValue value", t)
          }

        }

        try {
          mascotSearchParams.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting mascotSearchParams", t)
        }

        try {
          mascotResFile.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting mascotResFile", t)
        }

      }
      // Do nothing if already closed

    } // End of synchronized block on m_closeLock

  }

  /**
   * Parse specified mascot dat file and return corresponding ResultSet (target or decoy depending on wantDecoy parameter)
   *
   *
   */
  def getResultSet(wantDecoy: Boolean): ResultSet = {

    m_closeLock.synchronized {

      // Check if the result file is closed
      if (_isClosed) {
        throw new IllegalStateException("MascotResultFile is closed")
      }

    } // End of synchronized block on m_closeLock

    val start = System.currentTimeMillis;

    //---- DEBUG Purpose : to be removed
    // Get Matches information : Read Peptide, Proteins and associated matches

    //-------  For test / debug purpose only !    
    //    var totalNbrMatches =0
    //    val nbrQueryies: Int = mascotResFile.getNumQueries()
    //    val maxRankPerQuery: Int = pepSummary.getMaxRankValue()           
    //    
    //    for (q <- 1 until nbrQueryies + 1) {
    //      for (k <- 1 until maxRankPerQuery + 1) {
    //    	totalNbrMatches += printPeptide(q,k)        
    //      }
    //    }
    //    logger.debug("nbrQueryies " + nbrQueryies + " using maxRankPerQuery " + maxRankPerQuery+ " --> generated "+totalNbrMatches+" peptide matches")

    //---- END DEBUG Purpose : to be removed

    if (wantDecoy) logger.info("Start loading DECOY identification results...")
    else logger.info("Start loading TARGET identification results...")

    val pepSummary = _getPepSummary(wantDecoy)

    var dataParser: MascotDataParser = new MascotDataParser(
      pepSummary,
      mascotResFile,
      msiSearch.searchSettings,
      msQueryByInitialId,
      parserContext,
      wantDecoy
    )

    /*
     * Get all identified peptides and retrieve related information :
     *  - Query 
     *  - PeptideMatch
     *  - Identified proteins
     */
    val rsId = ResultSet.generateNewId
    dataParser.parseMatches(rsId)

    // --- Create final ResultSet issued from Mascot Result File
    val pepMatchesByPep = dataParser.getPeptideMatchesByPeptide
    var allPepMatches = pepMatchesByPep.values.flatMap { p => p } toArray

    val protMatches = dataParser.getProteinMatches
    logger.debug("Parser has gone through " + pepMatchesByPep.size + " Peptides creating " + allPepMatches.length + " PeptideMatches")
    logger.info("Create ResultSet with " + allPepMatches.length + " PeptideMatches identifying " + protMatches.size + " Proteins")

    val rsProps = new ResultSetProperties()
    rsProps.setMascotImportProperties(Some(rsImportProperties))
    logger.info("Loading identification results done in " + (System.currentTimeMillis - start) + " ms");
    new ResultSet(
      id = rsId,
      name = msiSearch.title,
      peptides = pepMatchesByPep.keySet.toArray,
      peptideMatches = allPepMatches,
      proteinMatches = protMatches,
      isDecoy = wantDecoy,
      isNative = true,
      msiSearch = Some(msiSearch),
      properties = Some(rsProps)
    )
  }

  /**
   * Creates for each MsQuery the corresponding Spectrum and
   * execute the specified onEachSpectrum function on it.
   *
   */
  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {

    val querybyInitialId = msQueryByInitialId
    logger.info("Start iterate over MSQueries")

    var count = 0

    for ((initialId, msq) <- querybyInitialId) { // Go through each Query

      val mozList = new ArrayBuffer[Double]
      val intensityList = new ArrayBuffer[Float]

      // Extract peak data
      val mascotQ = new ms_inputquery(mascotResFile, initialId)

      try {

        /*for( j <- 1 to mascotQ.getNumberOfPeaks(1) ) {
        mozList += mascotQ.getPeakMass(1,j)
        intensityList += mascotQ.getPeakIntensity(1,j).toFloat
      }*/

        // --- Parse peaks --- //
        val msqIonsString = mascotQ.getStringIons1
        if (msqIonsString.isEmpty == false) {

          val peaksAsStr = msqIonsString.split(",")
          val peaks = new ArrayBuffer[Tuple2[Double, Float]](peaksAsStr.length)

          peaksAsStr.foreach { peakAsStr =>
            val values = peakAsStr.split(":")
            peaks += Tuple2(values(0).toDouble, values(1).toFloat)
          }

          // Sort peaks by moz and build moz and intensity lists
          peaks.sortBy(_._1).foreach { peak =>
            mozList += peak._1
            intensityList += peak._2
          }

        } else {
          logger.debug("Spectrum of query#" + initialId + " is empty")
        }

        // Retrieve spectrum title and instrument config id
        val spectrumTitle = msq.asInstanceOf[Ms2Query].spectrumTitle
        val instConfigId = if (instrumentConfig.isDefined) instrumentConfig.get.id else 0

        val specTitleFieldMapOpt = if (peaklistSoftware.isDefined) peaklistSoftware.get.specTitleParsingRule.map(_.parseTitle(spectrumTitle)) else None
        val specTitleFieldMap = specTitleFieldMapOpt.getOrElse(Map.empty[SpectrumTitleFields.Value, String])

        // TODO: put in primitives utils
        def toIntOrZero(v: Any): Int = try { toInt(v) } catch { case e: Throwable => 0 }
        def toFloatOrZero(v: Any): Float = try { toFloat(v) } catch { case e: Throwable => 0f }

        val titleFields = SpectrumTitleFields

        val spec = new Spectrum(
          id = Spectrum.generateNewId,
          title = spectrumTitle,
          precursorMoz = msq.moz,
          precursorCharge = msq.charge,
          firstCycle = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_CYCLE, 0)),
          lastCycle = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_CYCLE, 0)),
          firstScan = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_SCAN, 0)),
          lastScan = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_SCAN, 0)),
          firstTime = toFloatOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_TIME, 0f)),
          lastTime = toFloatOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_TIME, 0f)),
          mozList = Some(mozList.toArray),
          intensityList = Some(intensityList.toArray),
          peaksCount = mozList.length,
          instrumentConfigId = instConfigId,
          peaklistId = peaklist.id
        )

        count += 1

        if ((count % LOG_SPECTRA_COUNT) == 0) {
          logger.debug("Created Spectra: " + count)
        }

        onEachSpectrum(spec)

      } finally {
        /* Free memory in finally block */

        try {
          mascotQ.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting mascotQ", t)
        }

      } // End of try - finally block

    }

    ()
  }

  /**
   * Creates for each MsQuery the corresponding Spectrum and
   * execute the specified onEachSpectrum fonction on it.
   *
   */

  def eachSpectrumMatch(wantDecoy: Boolean,
                        onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {

    val mascotVersion = importProperties.getOrElse(
      MascotParseParams.MASCOT_VERSION.toString,
      msiSearch.searchSettings.softwareVersion
    //throw new Exception("mascot version must be provided in the import properties")
    ).toString

    val mascotServerURLAsStr = importProperties.getOrElse(
      MascotParseParams.MASCOT_SERVER_URL.toString,
      "http://www.matrixscience.com/cgi/"
    //throw new Exception("mascot server url must be provided in the import properties")
    ).toString

    val mascotServerCGIURLAsStr = if (mascotServerURLAsStr.endsWith("/cgi/")) { mascotServerURLAsStr } else {
      if (mascotServerURLAsStr.endsWith("/cgi"))
        mascotServerURLAsStr ++ "/"
      else
        mascotServerURLAsStr ++ "/cgi/"
    }

    logger.debug("Iterating over spectrum matches of result file '%s' (mascot version=%s ; server URL =%s)".format(
      fileLocation.getName, mascotVersion, mascotServerCGIURLAsStr
    ))

    val mascotConfig = new MascotRemoteConfig(mascotVersion.asInstanceOf[String], mascotServerCGIURLAsStr.asInstanceOf[String])
    val spectrumMatcher = new MascotSpectrumMatcher(mascotResFile, mascotConfig, ptmHelper)

    try {

      // Retrieve peptide summary corresponding to the wanted dataset
      val pepSummary = _getPepSummary(wantDecoy)
      val nbrQueries = mascotResFile.getNumQueries()

      val maxRankPerQuery = pepSummary.getMaxRankValue()

      for (q <- 1 to nbrQueries; k <- 1 to maxRankPerQuery) { // Go through each Query        
        var currentMSPep = pepSummary.getPeptide(q, k)

        try {

          // Check that the peptide is not empty
          if (currentMSPep.getAnyMatch) {
            onEachSpectrumMatch(spectrumMatcher.getSpectrumMatch(currentMSPep))
          }

        } finally {
          /* Free memory in finally block */

          if (currentMSPep != null) {
            try {
              currentMSPep.delete()
            } catch {
              case t: Throwable => logger.error("Error deleting currentMSPep", t)
            }
          }

        } // End of try - finally block

      }

    } finally {

      try {
        spectrumMatcher.clear()
      } catch {
        case t: Throwable => logger.error("Error clearing spectrumMatcher", t)
      }

    }

  }

}
