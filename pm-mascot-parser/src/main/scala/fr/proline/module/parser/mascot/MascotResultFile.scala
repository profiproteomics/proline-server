package fr.proline.module.parser.mascot

import java.io._

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.concurrent._
import com.typesafe.scalalogging.StrictLogging
import fr.profi.chemistry.model.Enzyme
import fr.profi.util.primitives.toFloat
import fr.profi.util.primitives.toFloatOrMinusOne
import fr.profi.util.primitives.toIntOrZero
import fr.proline.core.algo.msi.validation.MascotValidationHelper
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import matrix_science.msparser.ms_mascotresfile
import matrix_science.msparser.ms_mascotresults
import matrix_science.msparser.ms_peptidesummary
import matrix_science.msparser.ms_searchparams
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
//  val MASCOT_SERVER_URL = Value("mascot.server.url")
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
  val parserContext: ProviderDecoratedExecutionContext
) extends IResultFile with StrictLogging {

  val LOG_SPECTRA_COUNT = 4000 // Print a log for each created spectrum

  //Cache for value to be keep between load and get methods
  var _targetResultSetOp : Option[ResultSet] = None
  var _decoyResultSetOp : Option[ResultSet] = None

  // ---- For Java developers ;) : --- Constructor Code 

  // Requirements
  val fileAbsolutePath = fileLocation.getAbsolutePath
  require(fileLocation.isFile, "Invalid fileLocation : " + fileAbsolutePath)

  logger.info("Opening Mascot result file : " + fileAbsolutePath)

  require(importProperties != null, "No Mascot Import Properties specified." )

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
  if (!mascotResFile.isValid()) throw new Exception("Invalid Mascot result file " + fileLocation.getAbsolutePath() + " specified : "+mascotResFile.getErrorString(1))

  private val mascotSearchParams: ms_searchparams = mascotResFile.params( )

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

  def msQueries: Array[MsQuery] = {
    msQueryByInitialId.values.toArray
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
      if (pepSummaryOpt.isEmpty) None
      else {
        val pepSum = pepSummaryOpt.get
        val ht = mascotResFile.getSectionValueDouble(ms_mascotresfile.SEC_SUMMARY, "qplughole" + msQueryNum).toFloat
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
      var specTitle = ""
      try {
        val tmp = URLDecoder.decode(mascotResFile.getQuerySectionValueStr(q, "title"), "UTF-8").replace('\\', '/') //WorkAround for \ char in spectrum storer  !
        if(!tmp.trim().equals("")) {
          specTitle = tmp
        }
      } catch {
        case ex: Exception => fileComesFromPklInput = true
      }
      
      if (specTitle.isEmpty()) {
        // AW/ABU: ticket #10344 fix
        val qMass = mascotResFile.getObservedMass(q)
        specTitle = s"Cmpd $q, +MSn($qMass), ? min" 
        
        // TODO: remove this assertion if we have no error
        assert( specTitle == "Cmpd "+q+", +MSn("+qMass+"), ? min", "something wrong with string interpolation" )

        specTitle
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
      logger.warn("Peaklist format looks like PKL, spectrum fake titles have been generated")
    }

    msQueryMapBuilder.result()
  }

  lazy val peaklist: Peaklist = {
    
    var fileNameStr = mascotSearchParams.getFILENAME()

    // Split string if it correspond to a Proteome Discoverer template
    if (fileNameStr.startsWith("File Name: ")) {
      logger.debug(s"Proteome Discoverer template detected, we will keep only the 'File Name' part of: $fileNameStr")
      val FileNamePattern = "File Name: (.+?);?".r
      val FileNamePattern(pklPath) = fileNameStr
      fileNameStr = pklPath
    }
    logger.info(s"Parsed peaklist file path is: $fileNameStr")
    
    val peaklistFile = new File(fileNameStr)
    val peaklistFileName = peaklistFile.getName
    val rawFileIdentifier = peaklistFileName.split('.').headOption.getOrElse(peaklistFileName)
    
    new Peaklist(
      id = Peaklist.generateNewId,
      fileType = mascotSearchParams.getFORMAT(), // TODO: check file extension first (ex: .raw)
      path = fileNameStr,
      rawFileIdentifier = rawFileIdentifier,
      msLevel = 2
    )
  }

  /**
   * Create from mascot parser ms_mascotresfile, a MSISearch with all associated information :
   * Peaklist, Enzyme, SeqDatabase and SearchSettings
   */
  lazy val msiSearch: MSISearch = {

    //******  Get MSISearch information : Retrieve peak list, search parameters...
    logger.info("Parse Marcot Search Settings information ...")

    val searchParams = mascotSearchParams

    val nbrSearchedQueries = mascotResFile.getNumSeqsAfterTax(0) //Get num of seq in all dbs after taxonomy filter

    //--- 1. Verify used SeqDatabases specified for Mascot Search exist, else Stop and throw an exception
    val nbrDBs = searchParams.getNumberOfDatabases()
    logger.debug("Search for " + nbrDBs + " Sequences database(s) used in Mascot result file")

    val seqDbs = new Array[SeqDatabase](nbrDBs)
    for (dbIndex <- 1 until (nbrDBs + 1)) {
      val dbName = searchParams.getDB(dbIndex)
      val filePath = mascotResFile.getFastaPath(dbIndex)
      
      seqDbs.update(
        dbIndex - 1,
        new SeqDatabase(
          id = SeqDatabase.generateNewId,
          name = dbName,
          filePath = filePath,
          sequencesCount = mascotResFile.getNumSeqs(dbIndex),
          releaseDate = new java.util.Date
//            version = version
        )
      )
//VDS TODO : Is this needed ?
      /*val seqDbProvider = parserContext.getProvider(classOf[ISeqDatabaseProvider])
      var usedSeqSDb = seqDbProvider.getSeqDatabase(dbName, filePath)

      logger.debug("Sequence database " + dbIndex + ": " + dbName + ", " + filePath + " found ? = " + usedSeqSDb)
      if (usedSeqSDb != None)
        seqDbs.update(dbIndex - 1, usedSeqSDb.get)
      else {
        val msg = "Sequence database used for identification does not exist in Proline datastore...."
        logger.warn(msg)*/
        
//        val version = if(filePath.lastIndexOf(dbName) != -1) {         
//        	var versionBuilder = filePath.substring(filePath.lastIndexOf(dbName)+dbName.length())
//			if(versionBuilder.startsWith("_"))
//				versionBuilder = versionBuilder.substring(1)
//			if(versionBuilder.lastIndexOf('.') != -1)
//				versionBuilder = versionBuilder.substring(0,versionBuilder.lastIndexOf('.'))
//                 
//			versionBuilder
//        } else ""
    }

    //--- 2. Create Enzymes & Search Settings
    val enzymes = Array(new Enzyme(searchParams.getCLE()))

    // Get C13 properties
    val isotopeOffRange: Array[Int] = Array(0,searchParams.getPEP_ISOTOPE_ERROR)
    val ssProperties = new SearchSettingsProperties( isotopeOffsetRange = isotopeOffRange)

    //Create MSISearch regrouping all these information
    val sSettings: SearchSettings = new SearchSettings(
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
      variablePtmDefs = ptmHelper.varPtmDefsByModName.values.toArray.flatten,
      fixedPtmDefs = ptmHelper.fixedPtmDefsByModName.values.toArray.flatten,
      seqDatabases = seqDbs,
      instrumentConfig = instrumentConfig.orNull,
      fragmentationRuleSet = fragmentationRuleSet,
      properties = Some(ssProperties)
    )

    if (msLevel == 2) {
      sSettings.msmsSearchSettings = Some(
        new MSMSSearchSettings(
          ms2ChargeStates = searchParams.getCHARGE(), // TODO: check this param
          ms2ErrorTol = searchParams.getITOL(),
          ms2ErrorTolUnit = searchParams.getITOLU())
      )
    }

    ////--- 3. Create peaklist (lazy) & MsiSearch
    val nbQueries = mascotResFile.getNumQueries()
    new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = fileLocation.getName(),
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

  private def _buildPepSummary(wantDecoy: Boolean): Option[ms_peptidesummary] = {

    if (!hasDecoyResultSet && wantDecoy) return None

    case object MascotResFlags {
      val NoGroup = ms_mascotresults.MSRES_NOFLAG
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
        _decoyResultSetOp = None
        _targetResultSetOp = None


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
  override def parseResultSet(wantDecoy: Boolean)  {
    _loadResultSet(wantDecoy)
  }

  override def getResultSet(wantDecoy: Boolean): ResultSet = {
    if (wantDecoy) {
      if (!_decoyResultSetOp.isDefined)
        _loadResultSet(true)

      _decoyResultSetOp.get

    } else {
      if (!_targetResultSetOp.isDefined)
        _loadResultSet(false)
      _targetResultSetOp.get
    }
  }

  private def _loadResultSet(wantDecoy: Boolean)  {

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

    val dataParser = new MascotDataParser(
      pepSummary,
      mascotResFile,
      msiSearch.searchSettings,
      mascotSearchParams,
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
    val allPepMatches = pepMatchesByPep.values.flatten.toArray

    val protMatches = dataParser.getProteinMatches
    logger.debug("Parser has gone through " + pepMatchesByPep.size + " Peptides creating " + allPepMatches.length + " PeptideMatches")
    logger.info("Create ResultSet with " + allPepMatches.length + " PeptideMatches identifying " + protMatches.size + " Proteins")

    val rsProps = new ResultSetProperties()
    rsProps.setMascotImportProperties(Some(rsImportProperties))
    logger.info("Loading identification results done in " + (System.currentTimeMillis - start) + " ms");
    val loadedRS =  new ResultSet(
      id = rsId,
      name = msiSearch.title,
      peptides = pepMatchesByPep.keySet.toArray,
      peptideMatches = allPepMatches,
      proteinMatches = protMatches,
      isDecoy = wantDecoy,
      isSearchResult = true,
      isValidatedContent = false,
      msiSearch = Some(msiSearch),
      properties = Some(rsProps)
    )

    if(wantDecoy)
      _decoyResultSetOp = Some(loadedRS)
    else
      _targetResultSetOp = Some(loadedRS)
  }

  /**
   * Creates for each MsQuery the corresponding Spectrum and
   * execute the specified onEachSpectrum function on it.
   *
   */
  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {

    val queryByInitialId = msQueryByInitialId

    val peaklistId = peaklist.id
    val fragRuleSetIdOpt = if (fragmentationRuleSet.isDefined) Some(fragmentationRuleSet.get.id) else None

    logger.info("Iterating MSQueries spectra...")

    val bufferSize = 1000
    val querySectionQueue = new java.util.concurrent.LinkedBlockingQueue[Option[MascotQuerySection]](2 * bufferSize)
    var isLastSection = false
    var readSpectraCount = 0

    def stopSectionQueueMonitoring() {
      querySectionQueue.synchronized {
        // Notify querySectionQueue that we stopped to read spectra
        querySectionQueue.clear()
        querySectionQueue.put(None)
      }

      isLastSection = true
    }

    // Perform asynchronous IO
    import ExecutionContext.Implicits.global
    Future {

      this.eachQuerySection(this.fileLocation) { msqSection =>

        readSpectraCount += 1

        if ((readSpectraCount % LOG_SPECTRA_COUNT) == 0) {
          logger.debug(s"$readSpectraCount spectra have been read...")
        }

        querySectionQueue.put(Some(msqSection))

      } // ends eachQuerySection

      querySectionQueue.synchronized {
        isLastSection = true
        querySectionQueue.put(None) // add a None value to stop the queue monitoring
      }

    } onComplete {
      case scala.util.Success(r) => {
        logger.debug(s"All spectra ($readSpectraCount) have been read!")
      }
      case scala.util.Failure(t) => {
        try {
          logger.error("An error has been caught while reading spectra...")

          stopSectionQueueMonitoring()
        } finally {
          throw t
        }
      }
    }

    val sectionBuffer = new ArrayBuffer[MascotQuerySection](bufferSize)
    val spectrumQueue = new java.util.concurrent.LinkedBlockingQueue[Option[Spectrum]](2 * bufferSize)
    var isLastSpectrum = false

    val endOfParsingPromise = Promise[Unit]()

    Future {

      def parseSectionBufferInParallel() {
        sectionBuffer.par.foreach { section =>
          val spectrum = section.toSpectrum(queryByInitialId, fragRuleSetIdOpt, peaklistId)
          spectrumQueue.put(Some(spectrum))
        }
        sectionBuffer.clear()
      }

      logger.debug("Parse spectra in parallel...")

      while (isLastSection == false || !querySectionQueue.isEmpty()) {

        val sectionOpts = new java.util.ArrayList[Option[MascotQuerySection]]
        sectionOpts.add(querySectionQueue.take()) // wait here
        querySectionQueue.drainTo(sectionOpts)

        val sectionOptsIter = sectionOpts.iterator()
        while (sectionOptsIter.hasNext()) {
          val sectionOpt = sectionOptsIter.next
          if (sectionOpt.isDefined) {
            sectionBuffer += sectionOpt.get
            if (sectionBuffer.length == bufferSize) {
              parseSectionBufferInParallel()
            }
          }
        }
      }

      if (sectionBuffer.nonEmpty) {
        logger.debug("Parse last buffer of spectra...")

        // Parse last buffer
        parseSectionBufferInParallel()
      }

      isLastSpectrum = true
      spectrumQueue.put(None) // add a None value to stop the queue monitoring

    } onComplete {
      case scala.util.Success(r) => {
        logger.debug(s"All spectra ($readSpectraCount) have been parsed!")
        endOfParsingPromise.success()
      }
      case scala.util.Failure(t) => {
        logger.error("An error has been caught while parsing spectra...")

        // Stop sectionQueue monitoring
        stopSectionQueueMonitoring()

        // Notify spectrumQueue that we stopped to parse spectra
        spectrumQueue.clear()
        spectrumQueue.put(None)
        isLastSpectrum = true

        endOfParsingPromise.failure(t)
      }
    }

    while (isLastSpectrum == false || !spectrumQueue.isEmpty()) {

      val spectraOpts = new java.util.ArrayList[Option[Spectrum]]
      spectraOpts.add(spectrumQueue.take()) // wait here
      spectrumQueue.drainTo(spectraOpts)

      val spectraOptsIter = spectraOpts.iterator()
      while (spectraOptsIter.hasNext()) {
        val spectrumOpt = spectraOptsIter.next
        if (spectrumOpt.isDefined) {
          onEachSpectrum( spectrumOpt.get )
        }
      }
    }

    scala.concurrent.Await.result(endOfParsingPromise.future,duration.Duration.Inf)

    logger.debug(s"End of eachSpectrum method!")

    ()
  }

  //Nothing to do during parse !
  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {}

  //** Some pieces of code used to parse manually the Mascot query sections **//
  private val MASCOT_MULTIPART_BOUNDARY = "--gc0p4Jq0M2Yt08jU534c0p"
  private val MASCOT_MULTIPART_EOF = "--gc0p4Jq0M2Yt08jU534c0p--"
  private val MASCOT_INDEX_SECTION_LINE = """Content-Type: application/x-Mascot; name="index""""
  private val MASCOT_FIRST_QUERY_KEY = "query1="

  object MascotQuerySection {
    val QUERY_NUM = "query_num"
    val TITLE = "title"
    val RTINSECONDS = "rtinseconds"
    val INDEX = "index"
    val CHARGE = "charge"
    val MASS_MIN = "mass_min"
    val MASS_MAX = "mass_max"
    val INT_MIN = "int_min"
    val INT_MAX = "int_max"
    val NUM_VALS = "num_vals"
    val NUM_USED1 = "num_used1"
    val IONS1 = "Ions1"
  }

  class MascotQuerySection(val underlyingMap: scala.collection.Map[String,String]) {

    import MascotQuerySection._

    lazy val queryNumber = underlyingMap(QUERY_NUM).toInt
    lazy val rtRangeInSeconds: Option[(Float, Float)] = if (!underlyingMap.contains(RTINSECONDS)) None
    else {
      val rtInSecondsAsStr = underlyingMap(RTINSECONDS)
      val rtRanges = rtInSecondsAsStr.split(",").map( _.split("-") )
      Some(toFloatOrMinusOne(rtRanges.head.head.trim), toFloatOrMinusOne(rtRanges.last.last.trim))
    }

    lazy val index = underlyingMap(INDEX).toInt
    def charge = underlyingMap(CHARGE)
    lazy val mass_min = underlyingMap.get(MASS_MIN).map(_.toDouble)
    lazy val mass_max = underlyingMap.get(MASS_MAX).map(_.toDouble)
    lazy val int_min = underlyingMap.get(INT_MIN).map(_.toInt)
    lazy val int_max = underlyingMap.get(INT_MAX).map(_.toInt)
    lazy val num_vals = underlyingMap.get(NUM_VALS).map(_.toInt)
    lazy val num_used1 = underlyingMap.get(NUM_USED1).map(_.toInt)
    def ions1 = underlyingMap.get(IONS1)

    def toSpectrum(
      queryByInitialId: Map[Int, MsQuery],
      fragmentationRuleSetIdOpt: Option[Long],
      peaklistId: Long
    ): Spectrum = {

      val initialId = this.queryNumber
      val msq = queryByInitialId(initialId)

      // --- Peaks parsing --- //
      val msqIonsStringOpt = this.ions1

      val peaksAsStr = if (msqIonsStringOpt.isEmpty || msqIonsStringOpt.get.isEmpty) Array.empty[String]
      else msqIonsStringOpt.get.split(",")

      val peaksCount = peaksAsStr.length
      if (peaksCount == 0) {
        logger.debug(s"Spectrum of query #$initialId is empty")
      }

      val indices = new Array[Int](peaksCount)
      val mozList = new Array[Double](peaksCount)
      val intensityList = new Array[Float](peaksCount)

      var i = 0
      while (i < peaksCount) {
        val peakAsStr = peaksAsStr(i)
        val values = peakAsStr.split(":")
        assert( values.length >= 2, s"Invalid number fo values for peak string $peakAsStr")

        indices(i) = i
        mozList(i) = values(0).toDouble
        intensityList(i) = values(1).toFloat
        i += 1
      }

      val sortedIndices = indices.sortBy(mozList(_))
      val sortedMozList = new Array[Double](peaksCount)
      val sortedIntensityList = new Array[Float](peaksCount)

      i = 0
      while (i < peaksCount) {
        val mascotPeakIdx = sortedIndices(i)
        sortedMozList(i) = mozList(mascotPeakIdx)
        sortedIntensityList(i) = intensityList(mascotPeakIdx)
        i += 1
      }

        // Retrieve some spectrum meta-data
        val spectrumTitle = msq.asInstanceOf[Ms2Query].spectrumTitle
        val specTitleFieldMapOpt = if (peaklistSoftware.isDefined) peaklistSoftware.get.specTitleParsingRule.map(_.parseTitle(spectrumTitle)) else None
        val specTitleFieldMap = specTitleFieldMapOpt.getOrElse(Map.empty[SpectrumTitleFields.Value, String])

        val titleFields = SpectrumTitleFields
        var spectrumPropOpt = Option.empty[SpectrumProperties]

        val (firstRT, lastRT) = {
          if (this.rtRangeInSeconds.isEmpty) (-1f, -1f)
          else {
            val rtRangeInSeconds = this.rtRangeInSeconds.get

            val spectrumProp = new SpectrumProperties()
            spectrumProp.rtInSeconds = Some(rtRangeInSeconds._1)
            spectrumPropOpt = Some(spectrumProp)

            (rtRangeInSeconds._1/60.0f, rtRangeInSeconds._2/60.0f)
          }
        }

        val spec = new Spectrum(
          id = Spectrum.generateNewId,
          title = spectrumTitle,
          precursorMoz = msq.moz,
          precursorCharge = msq.charge,
          firstCycle = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_CYCLE, 0)),
          lastCycle = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_CYCLE, 0)),
          firstScan = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_SCAN, 0)),
          lastScan = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_SCAN, 0)),
          firstTime = toFloatOrMinusOne(specTitleFieldMap.getOrElse(titleFields.FIRST_TIME, firstRT)),
          lastTime = toFloatOrMinusOne(specTitleFieldMap.getOrElse(titleFields.LAST_TIME, lastRT)),
          mozList = Some(sortedMozList),
          intensityList = Some(sortedIntensityList),
          peaksCount = mozList.length,
          fragmentationRuleSetId = fragmentationRuleSetIdOpt,
          peaklistId = peaklist.id,
          properties = spectrumPropOpt
        )

      spec
    }
  }

  def eachQuerySection(file: File)(onEachSection: MascotQuerySection => Unit): Unit = {
    require(file != null && file.isFile, "Provided file is null or invalid")

    def newBufferedReader(inputStream: InputStream): BufferedReader = {
      new BufferedReader(
        new InputStreamReader(
          inputStream,
          // Force ANSI ISO-8859-1 to read Mascot .dat files
          MascotDataParser.LATIN_1_CHARSET
        )
      )
    }

    def tryWithBufferedReader( bfr: BufferedReader)(unsafeFn: => Unit) = {
      try {
        unsafeFn
      } finally {

        try {
          bfr.close()
        } catch {
          case t: Throwable => {
            logger.error(s"Error closing reader of file ${file.getAbsolutePath}", t)
          }
        }
      }
    }

    // Create a reader that reads lines reversely
    val revBfr = newBufferedReader(new fr.profi.util.io.FastReverseLineInputStream(file))

    // Search for the line number of the first query
    var firstQueryLineNumber = 0
    tryWithBufferedReader(revBfr) {
      var line = revBfr.readLine()
      var eof = false

      while (eof == false) {
        if (line == null || line == MASCOT_INDEX_SECTION_LINE) {
          eof = true
        }
        else if (line.startsWith(MASCOT_FIRST_QUERY_KEY)) {
          firstQueryLineNumber = line.split("=").last.toInt
        }
        line = revBfr.readLine()
      }
    }
    assert(firstQueryLineNumber > 0, "Can't parse query1 line number")

    this.logger.debug(s"Line number of query1 in Mascot result file is $firstQueryLineNumber")

    // Create a new reader positioned at this offset, then read the file line by line
    val bfr = newBufferedReader(new FileInputStream(file))

    val section = new collection.mutable.HashMap[String,String]()
    section.sizeHint(20)

    tryWithBufferedReader(bfr) {
      var lineNumber = 1
      var line = bfr.readLine()
      var eof = false
      var isInsideQueriesSection = false

      while (eof == false) {

        if (line == null) {
          eof = true
        }
        else if (isInsideQueriesSection == false && lineNumber == firstQueryLineNumber) {
          isInsideQueriesSection = true
        }
        else if (isInsideQueriesSection) {

          if (line == MASCOT_MULTIPART_EOF || line == MASCOT_INDEX_SECTION_LINE) {
            eof = true
          }
          else if (line == MASCOT_MULTIPART_BOUNDARY) {
            if (section.contains(MascotQuerySection.QUERY_NUM)) {
              onEachSection(new MascotQuerySection(section.clone()))
              section.clear()
            } else {
              logger.warn("Found a query section without 'query_num' field => skip it!")
            }
          }
          else if (line.startsWith("Content-Type")) {
            val queryNumAsStr = line.split("query").last.dropRight(1)
            section.put(MascotQuerySection.QUERY_NUM, queryNumAsStr)
          }
          else if (line.nonEmpty) {
            val lineParts = line.split('=')
            section.put(lineParts(0), lineParts(1))
          }
        }

        lineNumber += 1
        line = bfr.readLine()
      }

    }

    ()
  }
}
