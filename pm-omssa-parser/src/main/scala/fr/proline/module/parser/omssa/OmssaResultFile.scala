package fr.proline.module.parser.omssa

import java.io.File
import java.io.FileNotFoundException

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import com.weiglewilczek.slf4s.Logging

import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPeptideProvider

object OmssaParseParams extends Enumeration {
  type OmssaParseParam = Value
  //  val OMSSA_XSD_FILE = Value("omssa.xsd.file")
  //  val MOD_XML_FILE = Value("mods.xml.file")
  val USERMOD_XML_FILE = Value("usermod.xml.file") // must be asked to the user, default file exists in case
  val FASTA_CONTAINS_TARGET = Value("fasta.contains.target") // must be mandatory
  val FASTA_CONTAINS_DECOY = Value("fasta.contains.decoy") // must be mandatory
  val OMSSA_VERSION = Value("omssa.version") // 2.1.9 is default
  val FASTA_FILE_PATH = Value("path.to.fasta.file") // optional
  val FASTA_TAXONOMIES = Value("numeric.taxonomy.ids.separated.by.string.character") // optional
  val PEAK_LIST_FILE_PATH = Value("path.to.peak.list.file") // optional
  val RAW_FILE_PATH = Value("path.to.raw.file") // optional
}

/**
 * This class allows a hashmap with two keys for one value
 * In this parser, it is used to map a sequenceMatch to a peptideMatch and a proteinMatch
 *
 * @param <K1> first key (the peptide match id)
 * @param <K2> second key (the protein match id)
 * @param <V> the value (the sequence match object)
 */
case class TwoDimensionsMap[K1, K2, V]() {
  val wrapped = new scala.collection.mutable.HashMap[(K1, K2), V]
  def update(k1: K1, k2: K2, v: V) = wrapped.put((k1, k2), v)
  // or use wrapped.update if you don't care about the old value 
  def apply(k1: K1, k2: K2) = wrapped((k1, k2))
  //  def length = wrapped.size
  def exists(k1: K1, k2: K2) = wrapped.contains(k1, k2)
  def getOption(k1: K1, k2: K2) = {
    if(wrapped.contains(k1, k2)) Some(wrapped((k1, k2))) else None
  }
  def getOrElse(k1: K1, k2: K2, e: V) = {
    if(wrapped.contains(k1, k2)) wrapped((k1, k2)) else e
  }
  def getValues() = wrapped.values.toSeq
  def foreach[C](f: ((K1, K2), V) => C): Unit = wrapped.foreach(e => f((e._1._1, e._1._2), e._2))
}

/**
 *  Some exceptions for the parser
 */
class NotMatchingSearchSettingsException(info: String) extends Exception("All Settings have to match " + info) {}
class UnexpectedOmxFormatException(expected: String, found: String) extends Exception("Unexpected OMX format, expected mark-up is '" + expected + "' instead of '" + found + "'") {}
class UnknownPTMException() extends Exception("A peptide matched with an unknown PTM") {}

/**
 * Create an OmssaResultFile to parse specified OMSSA identification result file.
 *
 * @param fileLocation :  OMSSA identification result file to parse (omx file)
 * @param providerKey key to use to get correct data provider from provider factory
 * @param importProperties : parameters to use for parsing. Allowed values are those specified by OmssaParseParams. Should not be null.
 *
 */
class OmssaResultFile(val fileLocation: File, val parserContext: ProviderDecoratedExecutionContext, val importProperties: Map[String, Any]) extends IResultFile with Logging {

  // Requirements
  require(importProperties != null)

  // users parameters
  private var parseProperties: Map[OmssaParseParams.OmssaParseParam, Any] = importProperties.map(entry => OmssaParseParams.withName(entry._1) -> entry._2)
  // check mandatory parameters
  //      if(parseProperties.get(OmssaParseParams.FASTA_CONTAINS_TARGET) == None)  throw new Exception("User did not indicate '"+OmssaParseParams.FASTA_CONTAINS_TARGET+"' parameter")
  //      if(parseProperties.get(OmssaParseParams.FASTA_CONTAINS_DECOY) == None)  throw new Exception("User did not indicate '"+OmssaParseParams.FASTA_CONTAINS_DECOY+"' parameter")
  if (parseProperties.get(OmssaParseParams.FASTA_CONTAINS_TARGET) == None) {
    logger.info("FASTA_CONTAINS_TARGET is missing, default value will be used (true)")
    parseProperties += (OmssaParseParams.FASTA_CONTAINS_TARGET -> true)
  }
  if (parseProperties.get(OmssaParseParams.FASTA_CONTAINS_DECOY) == None) {
    logger.info("FASTA_CONTAINS_DECOY is missing, default value will be used (true)")
    parseProperties += (OmssaParseParams.FASTA_CONTAINS_DECOY -> true)
  }
  // parameters that must be present, at least with a default value
  if (parseProperties.get(OmssaParseParams.USERMOD_XML_FILE) == None) parseProperties += (OmssaParseParams.USERMOD_XML_FILE -> "")
  if (parseProperties.get(OmssaParseParams.OMSSA_VERSION) == None) parseProperties += (OmssaParseParams.OMSSA_VERSION -> "2.1.9")
  // add default values for optional parameters
  if (parseProperties.get(OmssaParseParams.FASTA_FILE_PATH) == None) parseProperties += (OmssaParseParams.FASTA_FILE_PATH -> "")
  if (parseProperties.get(OmssaParseParams.FASTA_TAXONOMIES) == None) parseProperties += (OmssaParseParams.FASTA_TAXONOMIES -> "")
  if (parseProperties.get(OmssaParseParams.PEAK_LIST_FILE_PATH) == None) parseProperties += (OmssaParseParams.PEAK_LIST_FILE_PATH -> "")
  if (parseProperties.get(OmssaParseParams.RAW_FILE_PATH) == None) parseProperties += (OmssaParseParams.RAW_FILE_PATH -> "")

  private val omxFile = fileLocation
  // check file existancy
  if (omxFile == null || !omxFile.exists()) throw new FileNotFoundException("Specified file does not exist")
  if (!omxFile.getName().endsWith(".omx")) throw new IllegalArgumentException("Specified file does not have '.omx' suffix")
  logger.info("open Omssa omx file " + omxFile.getAbsoluteFile())

  // loader for mandatory files that must be loaded BEFORE the omx file
  private var omssaLoader: OmssaMandatoryFilesLoader = new OmssaMandatoryFilesLoader(
    parseProperties.get(OmssaParseParams.USERMOD_XML_FILE).getOrElse("").toString,
    parserContext)
  def getOmssaLoader: OmssaMandatoryFilesLoader = omssaLoader

  // IResultFile values
  val msLevel: Int = 2 // Disable support for PMF data  
  val hasMs2Peaklist: Boolean = true // an OMSSA omx  file may not have spectra and search params included

  // read omssa file
  val fileReader = new OmssaReadFile(omxFile, parseProperties, omssaLoader, peaklist, parserContext)
  val hasDecoyResultSet = fileReader.containsDecoyProteinMatches
  val omssaSettingsInHashTable = fileReader.omssaSettingsInHashTable

  def getMsQueries = fileReader.getMsQueries.values.toArray[Ms2Query]

  lazy val peaklist: Peaklist = {
    val peaklistPath = parseProperties.get(OmssaParseParams.PEAK_LIST_FILE_PATH).toString
    var peaklistType = "mgf"; // mgf is the default value
    omssaLoader.spectrumFileTypes.foreach { fileType =>
      // look at the file extension
      if (peaklistPath.matches("." + fileType._2 + "$")) peaklistType = fileType._2
    }
    new Peaklist(id = Peaklist.generateNewId,
      fileType = peaklistType,
      path = peaklistPath,
      rawFileName = parseProperties.get(OmssaParseParams.RAW_FILE_PATH).toString,
      msLevel = 2,
      peaklistSoftware = null)
  }

  // Load the MS queries here when requested
  lazy val msQueryByInitialId: Map[Int, MsQuery] = {
    val msQueryMapBuilder = scala.collection.immutable.Map.newBuilder[Int, MsQuery]
    for (query <- fileReader.getMsQueries) {
      msQueryMapBuilder += (query._1 -> query._2)
    }
    msQueryMapBuilder.result()
  }

  /**
   * Create from the omx file a MSISearch with all associated information :
   * Peaklist, Enzyme, SeqDatabase and SearchSettings
   */
  lazy val msiSearch: MSISearch = {
    val search: MSISearch = fileReader.getMsiSearch
    search.searchSettings.instrumentConfig = this.instrumentConfig.getOrElse(null)
    search
  }
  def getMSISearch: MSISearch = msiSearch

  /**
   * Parse specified OMSSA omx file and return corresponding ResultSet
   */
  def getResultSet(wantDecoy: Boolean): ResultSet = {

    logger.debug("hasDecoyResultSet=" + hasDecoyResultSet + " && wantDecoy=" + wantDecoy)
    if (!hasDecoyResultSet && wantDecoy) {
      throw new Exception("can't load the peptide summary")
    }

    if (wantDecoy) logger.info("Load decoy identification results...")
    else logger.info("Load target identification results...")

    /*
     * Get all identified peptides and retrieve related information :
     *  - Query 
     *  - PeptideMatch
     *  - Identified proteins
     */
    val rsId = ResultSet.generateNewId
    parseMatches(fileReader.getPeptideToPeptideMatches,
      fileReader.getPeptideMatchProteinMatchToSequenceMatch,
      fileReader.getPeptideMatchToProteinMatches,
      wantDecoy, rsId)

    // --- Create final ResultSet issued from Omssa Result File
    val pepMatchesByPep = pepToPeptideMatches.map { case (k, v) => k -> v.toArray } // this hash contains the peptides/peptidematches corresponding to the target/decoy value given
    var allPepMatches = pepMatchesByPep.values.flatMap { p => p } toArray
    val protMatches = proteinAccessionNumberToProteinMatch.values.toArray

    val rsProps = new ResultSetProperties()
    val rsImportProperties = new OmssaImportProperties()
    rsImportProperties.setRawSettings(Some(omssaSettingsInHashTable))
    rsProps.setOmssaImportProperties(Some(rsImportProperties))

    logger.debug("Parser has read " + fileReader.getMsQueries.size + " spectra")
    logger.debug("Parser has gone through " + pepMatchesByPep.size + " peptides creating " + allPepMatches.length + " peptide matches ")
    logger.info("Create ResultSet for " + (if (wantDecoy) "decoy" else "target") + " entries with " + allPepMatches.length + " peptide matches identifying " + protMatches.size + " proteins")

    new ResultSet(id = rsId,
      peptides = pepMatchesByPep.keySet.toArray,
      peptideMatches = allPepMatches,
      proteinMatches = protMatches,
      isDecoy = wantDecoy,
      isNative = true,
      msiSearch = Some(msiSearch),
      properties = Some(rsProps))
  }

  private var pepToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]] = null
  private var proteinAccessionNumberToProteinMatch: HashMap[String, ProteinMatch] = null
  /**
   * Create all necessary data for each peptide Matches identifying at least one protein.
   * Created objects are :
   * - PeptideMatch & Query associated to new or existing Peptide
   * - SequenceMatch and ProteinMatch for identified Proteins (new or existing)
   *
   */
  private def parseMatches(peptideToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]],
                           peptideMatchProteinMatchToSequenceMatch: TwoDimensionsMap[Long, Long, SequenceMatch],
                           peptideMatchToProteinMatches: HashMap[Long, ArrayBuffer[ProteinMatch]],
                           wantDecoy: Boolean, resultSetId: Long) = {
    // Define/reset some vars
    val bestPepMatchByPepKey = new HashMap[String, PeptideMatch]()
    proteinAccessionNumberToProteinMatch = new HashMap[String, ProteinMatch]()

    pepToPeptideMatches = peptideToPeptideMatches
    logger.debug(" Go through query / pep done . Found " + pepToPeptideMatches.size + " different peptides")

    val pepsToSearch = Array.newBuilder[Pair[String, Array[LocatedPtm]]]
    pepToPeptideMatches.foreach(entry => {
      pepsToSearch += Pair(entry._1.sequence, entry._1.ptms)
    })
    val searchPeps = pepsToSearch.result

    var pepProvider = parserContext.getProvider(classOf[IPeptideProvider])
    var protProvider = parserContext.getProvider(classOf[fr.proline.core.om.provider.msi.IProteinProvider])
    val pepByUniqueKey = fileReader.getPeptideByUniqueKey
    val foundPep = pepProvider.getPeptidesAsOptionsBySeqAndPtms(searchPeps)
    logger.debug(" Found Pep in PS : " + foundPep.filter(_.isDefined).size)

    foundPep.foreach(fPep => {
      if (fPep.isDefined) {

        val uniqueKey = fPep.get.uniqueKey
        logger.trace("Search pep " + uniqueKey + "  => " + uniqueKey)

        val oldPep = pepByUniqueKey.get(uniqueKey).get
        pepByUniqueKey.put(uniqueKey, fPep.get)
        if (pepToPeptideMatches.get(oldPep).isDefined) {
          var pepMatches = pepToPeptideMatches.get(oldPep).get
          var newPepMatches = new ArrayBuffer[PeptideMatch]
          pepMatches.foreach(f => {
            val newPepMatch = new PeptideMatch(
              id = f.id,
              rank = f.rank,
              score = f.score,
              scoreType = f.scoreType,
              deltaMoz = f.deltaMoz,
              isDecoy = f.isDecoy,
              peptide = fPep.get,
              missedCleavage = f.missedCleavage,
              fragmentMatchesCount = f.fragmentMatchesCount,
              msQuery = f.msQuery,
              properties = f.properties,
              resultSetId = f.resultSetId
            )
            newPepMatches += newPepMatch
          })
          pepToPeptideMatches.remove(oldPep)
          pepToPeptideMatches.put(fPep.get, newPepMatches)
        } // End PepMatch associated to peptide
      } //End peptide found
    })
    if (pepToPeptideMatches.size == 0) throw new Exception("No pepToPeptideMatches left")

    // Determine best peptide match for each peptide
    logger.debug("Determining the best peptide match for each peptide...")
    for ((pep, pepMatches) <- pepToPeptideMatches) {

      var bestPepMatch = pepMatches(0)
      for (i <- 1 until pepMatches.length) {
        val nextPepMatch = pepMatches(i)
        if ((bestPepMatch.score < nextPepMatch.score) || ((bestPepMatch.score == nextPepMatch.score) && (bestPepMatch.id < nextPepMatch.id)))
          bestPepMatch = nextPepMatch
      }

      bestPepMatchByPepKey.update(pep.uniqueKey, bestPepMatch)
    }

    val wrappedProtToProtMatch = new HashMap[ProteinWrapper, ProteinMatch]()
    val protAccSeqDbToProteinWrapper = new HashMap[String, ProteinWrapper]()
    //Get SeqDatabase in which current protein was retrieve from
    val seqDbs: Array[SeqDatabase] = fileReader.getMsiSearch.searchSettings.seqDatabases
    // Second pass to build protein matches and sequence matches
    logger.debug(" Go through bestPepMatchByPepKey " + bestPepMatchByPepKey.size)
    for (bestPepMatch <- bestPepMatchByPepKey.values) {
      val protAcc = peptideMatchToProteinMatches.get(bestPepMatch.id).get

      if (protAcc.size > 0) { // A least one protein matched

        var parsedPep = bestPepMatch.peptide

        // *****  go through matched proteins
        for (protMatch <- protAcc) {
          //****  Get or created ProteinWrapper, and Protein if defined, for matched Protein
          var prot = Option.empty[Protein]
          // Check if the protein has been already accessed
          val protWrapperKey = protMatch.accession + seqDbs(0).id
          if (protAccSeqDbToProteinWrapper.contains(protWrapperKey)) {
            prot = protAccSeqDbToProteinWrapper.get(protWrapperKey).get.wrappedProt
          } else {
            // Try to get Protein from repository
            prot = protProvider.getProtein(protMatch.accession, seqDbs(0))
            protAccSeqDbToProteinWrapper += protWrapperKey -> new ProteinWrapper(seqDbs(0).id, protMatch.accession, prot)
          }
          val currentSeqMatch = peptideMatchProteinMatchToSequenceMatch(bestPepMatch.id, protMatch.id)
          // Create SequenceMatch and ProteinMatch, if necessary, for current Matched Protein
          var seqMatch = new SequenceMatch(
            start = currentSeqMatch.start,
            end = currentSeqMatch.end,
            residueBefore = currentSeqMatch.residueBefore,
            residueAfter = currentSeqMatch.residueAfter,
            isDecoy = wantDecoy,
            peptide = Some(parsedPep),
            bestPeptideMatch = Some(bestPepMatch),
            resultSetId = bestPepMatch.resultSetId
          )

          // Get ProteinMatch associated to this protein, through its ProteinWrapper
          var protMatchOpt: Option[ProteinMatch] = wrappedProtToProtMatch.get(protAccSeqDbToProteinWrapper.get(protWrapperKey).get)

          var newProtMatch: ProteinMatch = null
          if (protMatchOpt == None) {
            val seqDbIds = seqDbs map { _.id }
            val protMatchAc = protMatch.accession
            val protMatchDesc = protMatch.description

            // Not already define, create ProteinMatch and add new entry in protToProtMatch
            newProtMatch = new ProteinMatch(id = ProteinMatch.generateNewId,
              accession = protMatchAc,
              description = protMatchDesc,
              peptideMatchesCount = 0, // FIXME: assign the right number
              scoreType = "omssa:expect value",
              isDecoy = wantDecoy,
              protein = (if (prot == None) null else prot), // If prot is None => No protein is defined not protein not retrieve !
              seqDatabaseIds = seqDbIds,
              resultSetId = bestPepMatch.resultSetId
            )
            wrappedProtToProtMatch += (protAccSeqDbToProteinWrapper.get(protWrapperKey).get -> newProtMatch)
          } else {
            newProtMatch = protMatchOpt.get
          }

          // Add created seqMatch to Protein Match
          val newSeqMatches = new ArrayBuffer[SequenceMatch]()
          if (newProtMatch.sequenceMatches != null) newSeqMatches ++= newProtMatch.sequenceMatches
          newSeqMatches += seqMatch

          newProtMatch.sequenceMatches = newSeqMatches.toArray

          // Update Protein Match score
          newProtMatch.score = newProtMatch.score + bestPepMatch.score

          //Update  Protein Match peptideMatchesCount
          newProtMatch.peptideMatchesCount = newProtMatch.peptideMatchesCount + pepToPeptideMatches.get(bestPepMatch.peptide).get.length
        }
      }
    }
    wrappedProtToProtMatch.values.foreach(protMatch => {
      proteinAccessionNumberToProteinMatch.put(protMatch.accession, protMatch)
    })

  }

  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {

    logger.info("eachSpectrum(" + omxFile.getAbsolutePath() + ")")
    new OmssaListSpectrum(omxFile, peaklist.id, this.instrumentConfig.getOrElse(null), onEachSpectrum)
    ()
  }

  private var spectrumList: ArrayBuffer[Spectrum] = null
  private def storeSpectrum(spectrum: Spectrum) = { spectrumList += spectrum }
  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {
    logger.info("eachSpectrumMatch(" + wantDecoy + ")")
    try {
      // first reset the list of spectra
      // spectra a deleted from this list after being used, because of the important amount of memory that may be needed
      // this list should already be empty at this point, but reset is forced anyway
      spectrumList = new ArrayBuffer[Spectrum]
      // list all the spectra and fill the array
      eachSpectrum(storeSpectrum)
      // then read the file (again) to get the detailed PeptideMatches
      new OmssaSpectrumMatcher(omxFile, wantDecoy, spectrumList, omssaLoader, getMSISearch.searchSettings, fileReader.mzScale, onEachSpectrumMatch)
    } catch {
      case e: Exception =>
        logger.error("eachSpectrumMatch in error", e)
        throw e
    }
    ()
  }

  /**
   * Clean loaded stuff is any
   */
  def close() {}

}
