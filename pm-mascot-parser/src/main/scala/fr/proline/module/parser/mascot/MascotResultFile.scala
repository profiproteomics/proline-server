package fr.proline.module.parser.mascot

import java.io.{ FileNotFoundException, File }
import java.lang.{ UnsatisfiedLinkError, System }
import java.net.URLDecoder
import scala.Array.canBuildFrom
import scala.collection.mutable.{ HashMap, ArrayBuffer }

import com.weiglewilczek.slf4s.Logging

import matrix_science.msparser.{ ms_searchparams, ms_peptidesummary, ms_mascotresults, ms_mascotresfile, ms_inputquery }

import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.msi.{ ISeqDatabaseProvider, IProteinProvider, IPeptideProvider, IPTMProvider }
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.algo.msi.validation.MascotValidationHelper
import fr.proline.util.primitives.toFloat

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

  final val LOG_SPECTRA_COUNT = 4000 // Print a log for each created spectrum

  // ---- For Java developers ;) : --- Constructor Code 

  // Requirements
  require(importProperties != null)

  private var _isClosed = false
  val parseProperties: Map[MascotParseParams.MascotParseParam, Any] = importProperties.map(entry => MascotParseParams.withName(entry._1) -> entry._2)
  val rsImportProperties = new MascotImportProperties()
  rsImportProperties.setIonsScoreCutoff(parseProperties.get(MascotParseParams.ION_SCORE_CUTOFF).map(toFloat(_)))
  rsImportProperties.setProteinsPvalueCutoff(parseProperties.get(MascotParseParams.PROTEIN_CUTOFF_PVALUE).map(toFloat(_)))
  rsImportProperties.setSubsetsThreshold(parseProperties.get(MascotParseParams.SUBSET_THRESHOLD).map(toFloat(_)))
  
  if (fileLocation == null || !fileLocation.exists()) throw new FileNotFoundException("Invalid specified file")
  logger.info("open Mascot dat file " + fileLocation.getAbsoluteFile())

  /* Load Matrix Science Mascot Parser native libraries */
  if (NativeLibrariesLoader.loadNativeLibraries == false) {
    val message = "Mascot Parser native libraries not loaded"
    logger.error(message)

    throw new RuntimeException(message)
  }
  
  // Create Mascot ms_mascotresfile and ms_peptidesummary from specified file name 
  private val mascotResFile: ms_mascotresfile = new ms_mascotresfile(fileLocation.getAbsolutePath(), 0, "")
  if (!mascotResFile.isValid()) throw new Exception("Invalid Mascot result file " + fileLocation.getAbsolutePath() + " specified")

  private val mascotSearchParams: ms_searchparams = this.mascotResFile.params()

  /** IResultFile values **/
  val msLevel: Int = 2 // Disable support for PMF data  
  val hasMs2Peaklist: Boolean = true // A Mascot dat file has always a peaklist
  val hasDecoyResultSet = if (mascotSearchParams.getDECOY == 1) true else false

  // ---- For Java developers ;) : --- END Constructor Code 

  //--- Lazy attributes --- //
  lazy val ptmHelper: MascotPTMHelper = {
     val helper = new MascotPTMHelper(parserContext)
     
     // Retrieve PTMs, fixed and variable, specified for Mascot Search
     val mascotVarModsAsStr = this.mascotSearchParams.getIT_MODS()
     logger.debug("Get specified variable Ptms using mascot string : " + mascotVarModsAsStr)
     val varPtmDefsByModName = helper.createVarPtmDefs(mascotVarModsAsStr)
     logger.debug("Found " + varPtmDefsByModName.size + " variables PTMs")

     val mascotFixedModsAsStr = this.mascotSearchParams.getMODS()
     logger.debug("Get specified fixed Ptms using mascot string : " + mascotFixedModsAsStr)
     val fixedPtmDefsByModName = helper.createFixedPtmDefs(mascotFixedModsAsStr)
     logger.debug("Found " + fixedPtmDefsByModName.size + " fixed PTMs")

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
    val nbrQueries: Int = this.mascotResFile.getNumQueries()

    def getMsQueryDbSearchProps(pepSummaryOpt: Option[ms_peptidesummary], msQueryNum: Int) = {
      if (pepSummaryOpt == None) None
      else {
        val pepSum = pepSummaryOpt.get
        var ht: Float = mascotResFile.getSectionValueDouble(ms_mascotresfile.SEC_SUMMARY, "qplughole"+msQueryNum).toFloat        
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

    for (q <- 1 to nbrQueries) { // Go through each Query
      
      val specTitle = URLDecoder.decode(this.mascotResFile.getQuerySectionValueStr(q, "title"), "UTF-8").replace('\\', '/') //WorkAround for \ char in spectrum storer  !
      
      val msQueryProps = new MsQueryProperties(
        targetDbSearch = getMsQueryDbSearchProps(this.targetPepSummary, q),
        decoyDbSearch = getMsQueryDbSearchProps(this.decoyPepSummary, q)
      )
      
      var query = new Ms2Query(
        id = Ms2Query.generateNewId,
        initialId = q,
        moz = this.mascotResFile.getObservedMass(q),
        charge = this.mascotResFile.getObservedCharge(q),
        spectrumTitle = specTitle,
        properties = Some(msQueryProps)
      )

      msQueryMapBuilder += (q -> query)
    }

    msQueryMapBuilder.result()

  }

  lazy val peaklist: Peaklist = {
    new Peaklist(
      id = Peaklist.generateNewId,
      fileType = this.mascotSearchParams.getFORMAT(), // TODO: check file extension first (ex: .raw)
      path = this.mascotSearchParams.getFILENAME(),
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
    logger.info("Parse Search Settings information ...")

    val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
    val searchParams = this.mascotSearchParams

    val nbrSearchedQueries = mascotResFile.getNumSeqsAfterTax(0) //Get num of seq in all dbs after taxonomy filter

    // Verify used SeqDatabases specified for Mascot Search exist, else Stop and throw an exception
    var nbrDBs = searchParams.getNumberOfDatabases()
    logger.debug("Get " + nbrDBs + " differents sequences database(s)")

    var seqDbs = new Array[SeqDatabase](nbrDBs)
    for (dbIndex <- 1 until (nbrDBs + 1)) {
      var dbName = searchParams.getDB(dbIndex)
      var filePath = mascotResFile.getFastaPath(dbIndex)

      val seqDbProvider = parserContext.getProvider(classOf[ISeqDatabaseProvider])
      var usedSeqSDb = seqDbProvider.getSeqDatabase(dbName, filePath)

      logger.debug("Seq DB " + dbIndex + ": " + dbName + ", " + filePath + " exist ? " + usedSeqSDb)
      if (usedSeqSDb != None)
        seqDbs.update(dbIndex - 1, usedSeqSDb.get)
      else {
        val msg = "Sequence DB used for identification is not referenced in system ... First load data in repository "
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
      instrumentConfig = this.instrumentConfig.getOrElse(null),
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
      peakList = this.peaklist,
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
    peptideSummary.setSubsetsThreshold( rsImportProperties.getSubsetsThreshold().getOrElse(1.0f).toDouble )

    Some(peptideSummary)

    //*** To test different creation modes : No Protein Group ?!   
    //new ms_peptidesummary(mascotResFile, ms_mascotresults.MSRES_GROUP_PROTEINS,
    //  0, 0, null, 0, 0, null, ms_peptidesummary.MSPEPSUM_NO_PROTEIN_GROUPING)

    //    pepSummary = new ms_peptidesummary(mascotResFile, ms_mascotresults.MSRES_GROUP_PROTEINS | ms_mascotresults.MSRES_SHOW_SUBSETS,
    //      0.05, Integer.MAX_VALUE, null, 0, 0, null)
  }

  private def _getPepSummary(wantDecoy: Boolean): ms_peptidesummary = {

    // Retrieve peptide summary corresponding to the wanted dataset
    val pepSummary = if (wantDecoy) this.decoyPepSummary else this.targetPepSummary
    if (pepSummary == None) {
      throw new Exception("can't load the peptide summary")
    }

    pepSummary.get
  }
  
  /**
   * Free all ms_parser attached objects
   */
  def close() {
    
    // Check if the result file is already closed
    if( _isClosed ) throw new java.lang.IllegalAccessException("MascotResultFile is already closed")
    
    mascotResFile.delete()
    mascotSearchParams.delete()
    if( _isTargetSummaryLoaded ) targetPepSummary.get.delete
    if( _isDecoySummaryLoaded ) decoyPepSummary.get.delete
  }

  /**
   * Parse specified mascot dat file and return corresponding ResultSet (target or decoy depending on wantDecoy parameter)
   *
   *
   */
  def getResultSet(wantDecoy: Boolean): ResultSet = {
    
    // Check if the result file is closed
    if( _isClosed ) throw new java.lang.IllegalAccessException("MascotResultFile is closed")
    
    val pepSummary = this._getPepSummary(wantDecoy)

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

    if (wantDecoy) logger.info("Load decoy identification results...")
    else logger.info("Load target identification results...")

    var dataParser: MascotDataParser = new MascotDataParser(
      pepSummary,
      this.mascotResFile,
      this.msiSearch.searchSettings,
      this.msQueryByInitialId,
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
    logger.debug("Parser has gone through " + pepMatchesByPep.size + " peptides creating " + allPepMatches.length + " peptide matches ")
    logger.info("Create ResultSet with " + allPepMatches.length + " peptide matches identifying " + protMatches.size + " proteins")

    val rsProps = new ResultSetProperties()
    rsProps.setMascotImportProperties(Some(rsImportProperties))
    
    new ResultSet(
      id = rsId,
      name = this.msiSearch.title,
      peptides = pepMatchesByPep.keySet.toArray,
      peptideMatches = allPepMatches,
      proteinMatches = protMatches,
      isDecoy = wantDecoy,
      isNative = true,
      msiSearch = Some(this.msiSearch),
      properties = Some(rsProps)
    )
  }

  /**
   * Creates for each MsQuery the corresponding Spectrum and
   * execute the specified onEachSpectrum function on it.
   *
   */
  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {

    val querybyInitialId = this.msQueryByInitialId
    logger.info("Iterate over MSQueries")

    var count = 0

    for ((initialId, msq) <- querybyInitialId) { // Go through each Query

      val mozList = new ArrayBuffer[Double]
      val intensityList = new ArrayBuffer[Float]

      // Extract peak data
      val mascotQ = new ms_inputquery(mascotResFile, initialId)
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
        this.logger.debug("spectrum of query#" + initialId + " is empty")
      }

      // TODO: parse spectrum title to extract timings
      val spectrumTitle = msq.asInstanceOf[Ms2Query].spectrumTitle
      val instConfigId = if (this.instrumentConfig.isDefined) this.instrumentConfig.get.id else 0

      val spec = new Spectrum(
        id = Spectrum.generateNewId,
        title = spectrumTitle,
        precursorMoz = msq.moz,
        precursorCharge = msq.charge,
        /*
                              firstCycle = Int = 0,
                              lastCycle = Int = 0,
                              firstScan = Int = 0,
                              lastScan = Int = 0,
                              firstTime = Float = 0,
                              lastTime = Float = 0,
                              */
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
      
      // --- Free memory --- //
      mascotQ.delete()
      
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
      this.msiSearch.searchSettings.softwareVersion
      //throw new Exception("mascot version must be provided in the import properties")
    ).toString
    
    val mascotServerURLAsStr = importProperties.getOrElse(
      MascotParseParams.MASCOT_SERVER_URL.toString,
      "http://www.matrixscience.com/cgi/"
      //throw new Exception("mascot server url must be provided in the import properties")
    ).toString
    
    this.logger.debug("Iterating over spectrum matches of result file '%s' (mascot version=%s ; server URL =%s)".format(
      fileLocation.getName,mascotVersion,mascotServerURLAsStr
    ))

    val mascotConfig = new MascotRemoteConfig(mascotVersion.asInstanceOf[String], mascotServerURLAsStr.asInstanceOf[String])
    val spectrumMatcher = new MascotSpectrumMatcher(mascotResFile, mascotConfig, ptmHelper)

    // Retrieve peptide summary corresponding to the wanted dataset
    val pepSummary = this._getPepSummary(wantDecoy)
    val nbrQueries = this.mascotResFile.getNumQueries()

    val maxRankPerQuery = pepSummary.getMaxRankValue()

    for (q <- 1 to nbrQueries; k <- 1 to maxRankPerQuery) { // Go through each Query        
      var currentMSPep = pepSummary.getPeptide(q, k)

      // Check that the peptide is not empty
      if (currentMSPep.getAnyMatch) {
        onEachSpectrumMatch(spectrumMatcher.getSpectrumMatch(currentMSPep))
      }
      
      // --- Free memory --- //
      currentMSPep.delete()
    }

  }

}
