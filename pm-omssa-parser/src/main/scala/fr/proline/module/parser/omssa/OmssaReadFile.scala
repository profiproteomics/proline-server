package fr.proline.module.parser.omssa

import fr.proline.core.om.model.msi.{ Protein, ProteinMatch, Peptide, PeptideMatch, Peaklist, Ms2Query, SequenceMatch, MSISearch, SeqDatabase, SearchSettings, SearchSettingsProperties, PtmDefinition, LocatedPtm, InstrumentConfig, PeptideMatchProperties }
//import fr.proline.core.om.model.msi.PeptideMatchOmssaProperties
import fr.proline.core.om.provider.msi.{ IProteinProvider, ISeqDatabaseProvider, IPeptideProvider, IPTMProvider }
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.builder.PtmDefinitionBuilder
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import java.io.File
import javax.xml.stream.XMLInputFactory
import org.codehaus.staxmate.in.{ SMHierarchicCursor, SMInputCursor }
import org.codehaus.staxmate.SMInputFactory
import com.weiglewilczek.slf4s.Logging
import fr.proline.context.DatabaseConnectionContext
import net.liftweb.json.{ pretty, compact }
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._

case class ProteinWrapper(val seqdbId: Int, val protAccess: String, var wrappedProt: Option[Protein]) {
  override def equals(other: Any): Boolean = {
    other match {
      case otherRefProt: ProteinWrapper => return protAccess.equals(otherRefProt.protAccess) && seqdbId.equals(otherRefProt.seqdbId)
      case _                            => return false
    }
  }
}

class OmssaReadFile(val omxFile: File,
                    val parseProperties: Map[OmssaParseParams.OmssaParseParam, Any],
                    val omssaLoader: OmssaMandatoryFilesLoader,
                    val peaklist: Peaklist,
                    val instrumentConfig: InstrumentConfig,
                    //  val omssaDefaultVersion: String, 
                    val parserContext: ProviderDecoratedExecutionContext //  entityProviders: EntityProviders
                    ) extends Logging {

  private val omssaScoreType = "omssa:expect value"
  private var pepByUniqueKey: HashMap[String, Peptide] = null
  def getPeptideByUniqueKey: HashMap[String, Peptide] = pepByUniqueKey
  private var msQueries: HashMap[Int, Ms2Query] = null
  def getMsQueries: HashMap[Int, Ms2Query] = msQueries
  private var protAccSeqDbToProteinWrapper: HashMap[String, ProteinWrapper] = null
  private var peptideToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]] = null
  def getPeptideToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]] = peptideToPeptideMatches
  private var peptideMatchToProteinMatches: HashMap[Int, ArrayBuffer[ProteinMatch]] = null
  def getPeptideMatchToProteinMatches: HashMap[Int, ArrayBuffer[ProteinMatch]] = peptideMatchToProteinMatches
  private var peptideMatchProteinMatchToSequenceMatch: TwoDimensionsMap[Int, Int, SequenceMatch] = null
  def getPeptideMatchProteinMatchToSequenceMatch: TwoDimensionsMap[Int, Int, SequenceMatch] = peptideMatchProteinMatchToSequenceMatch
  private var proteinAccessionNumbersToProteinMatches: HashMap[String, ProteinMatch] = null
  def mzScale: Int = currentFileMzScale
  private var nbSequencesInFastaFile: Int = 0
  private var searchSettingsReference: ArrayBuffer[String] = null
  private var searchSettingsCandidate: ArrayBuffer[String] = null
  private var seqDatabase: SeqDatabase = null
  private var msiSearch: MSISearch = null
  def getMsiSearch: MSISearch = msiSearch
  private val hasTargetResultSet: Boolean = parseProperties.getOrElse(OmssaParseParams.FASTA_CONTAINS_TARGET, true).toString.toBoolean
  def searchForTargetEntries: Boolean = hasTargetResultSet
  private val hasDecoyResultSet: Boolean = parseProperties.getOrElse(OmssaParseParams.FASTA_CONTAINS_DECOY, true).toString.toBoolean
  def searchForDecoyEntries: Boolean = hasDecoyResultSet
  //  private var omssaSettingsInJsonFormat: ArrayBuffer[JObject] = null
  def getSettingsInJsonFormat: ArrayBuffer[JObject] = omssaSettingsInJsonFormat
  // the Mz in the omssa files are Integers, they must be divided by this number to get the real value
  // a special class is used to get the value from the file before reading it
  private val mozScaleExtractor = new OmssaMozScaleExtractor(omxFile) 
  private var currentFileMzScale = mozScaleExtractor.mozScaleValue()

  _parseOmxFile()

  private def _parseOmxFile() {
    pepByUniqueKey = new HashMap[String, Peptide]()
    //    var pepProvider = entityProviders.peptideProvider
    //    var ptmProvider = entityProviders.ptmProvider
    //    var protProvider = entityProviders.proteinProvider
    var pepProvider = parserContext.getProvider(classOf[IPeptideProvider])
    val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
    var protProvider = parserContext.getProvider(classOf[IProteinProvider])
    val seqDbProvider = parserContext.getProvider(classOf[ISeqDatabaseProvider])
    msQueries = new HashMap[Int, Ms2Query]()
    protAccSeqDbToProteinWrapper = new HashMap[String, ProteinWrapper]()
    peptideToPeptideMatches = new HashMap[Peptide, ArrayBuffer[PeptideMatch]]()
    peptideMatchToProteinMatches = new HashMap[Int, ArrayBuffer[ProteinMatch]]()
    peptideMatchProteinMatchToSequenceMatch = new TwoDimensionsMap[Int, Int, SequenceMatch]()
    proteinAccessionNumbersToProteinMatches = new HashMap[String, ProteinMatch]()

    logger.info("readOmxFile(" + omxFile.getAbsolutePath() + ")")
    var nbSpectra: Int = 0
    // open an input factory
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
    // get the root cursor
    val MSSearch: SMHierarchicCursor = inf.rootElementCursor(omxFile)
    MSSearch.setElementTracking(SMInputCursor.Tracking.PARENTS)
    MSSearch.advance
    // advance the cursor to the first child of <MSSearch>
    val MSSearch_firstChild = MSSearch.childElementCursor().advance()
    if (MSSearch_firstChild.getPrefixedName() != "MSSearch_request") {
      throw new UnexpectedOmxFormatException("MSSearch_request", MSSearch_firstChild.getPrefixedName())
    }
    while (MSSearch_firstChild.getCurrEvent() != null) {
      MSSearch_firstChild.getPrefixedName() match {
        /***REQUEST-SPECTRA*************************************************************************/
        case "MSSearch_request" => // this part contains the spectra and the settings
          // advance the cursor to the first child of MSSearch_request (MSRequest)
          val MSRequest = MSSearch_firstChild.childElementCursor().advance()
          // advance the cursor to the first child of MSRequest (which appears only once)
          val MSRequest_firstChild = MSRequest.childElementCursor().advance()
          while (MSRequest_firstChild.getCurrEvent() != null) {
            MSRequest_firstChild.getPrefixedName() match {
              case "MSRequest_spectra" => // list the spectra given for the omssa search
                // advance the cursor to the first child of MSRequest_spectra (MSSpectrumset)
                val MSSpectrumset = MSRequest_firstChild.childElementCursor().advance()
                // advance the cursor to the first child of MSSpectrumset (which appears only once)
                val MSSpectrumset_firstChild = MSSpectrumset.childElementCursor().advance()
                while (MSSpectrumset_firstChild.getCurrEvent() != null) {
                  MSSpectrumset_firstChild.getPrefixedName() match {
                    case "MSSpectrum" => // this contains a full spectrum
                      nbSpectra += 1
                      // preparing the values to save for the current spectrum
                      var spectrumId: Int = 0
                      var precursorMz: Double = 0
                      var precursorCharge: Int = 0
                      var spectrumTitle: String = ""
                      // advance the cursor to the first child of MSSpectrum
                      val MSSpectrum_children = MSSpectrumset_firstChild.childElementCursor().advance()
                      // loop on the children of MSSpectrum
                      while (MSSpectrum_children.getCurrEvent() != null) {
                        MSSpectrum_children.getPrefixedName() match {
                          case "MSSpectrum_number"      => spectrumId = MSSpectrum_children.collectDescendantText(false).toInt
                          case "MSSpectrum_charge"      => precursorCharge = MSSpectrum_children.childElementCursor().advance().collectDescendantText(false).toInt
                          case "MSSpectrum_precursormz" => precursorMz = MSSpectrum_children.collectDescendantText(false).toDouble / 1000
                          case "MSSpectrum_ids"         => spectrumTitle = MSSpectrum_children.childElementCursor().advance().collectDescendantText(false).replace('\\', '/')
                          case _                        => // there is more information about the spectrum that is not treated (it takes too much memory and it is useless in the resultset, the full set is loaded in eachSpectrum)
                        }
                        MSSpectrum_children.advance()
                      }
                      // the title may be missing (if the input file is pkl/dta)
                      if (spectrumTitle == "") spectrumTitle = " Cmpd " + spectrumId + ", +MSn(" + precursorMz + "), ? min"
                      // dynamically generate the spectrum object and add it to a global hash
                      msQueries.put(
                        spectrumId,
                        new Ms2Query(
                          id = spectrumId,
                          initialId = spectrumId,
                          moz = precursorMz,
                          charge = precursorCharge,
                          spectrumTitle = spectrumTitle))
                    case _ => // this case should never occur
                  }
                  MSSpectrumset_firstChild.advance()
                }

              case "MSRequest_settings" =>
                // reference search settings
                searchSettingsCandidate = new ArrayBuffer[String]()
                goThrough(MSRequest_firstChild.childElementCursor.advance.childElementCursor.advance)
                searchSettingsReference = searchSettingsCandidate.clone
                searchSettingsCandidate = null
                parseMSISearch(nbSpectra, seqDbProvider)

              case "MSRequest_moresettings" => // if the current omx file is the merge of multiple omx files, all the settingsets have to match
                // advance the cursor to the first child of MSRequest_moresettings (MSSearchSettingsSet)
                val MSSearchSettingsSet = MSRequest_firstChild.childElementCursor().advance()
                // advance the cursor to the first child of MSSearchSettingsSet (which appears only once)
                val MSSearchSettingsSet_firstChild = MSSearchSettingsSet.childElementCursor().advance()
                while (MSSearchSettingsSet_firstChild.getCurrEvent() != null) {
                  MSSearchSettingsSet_firstChild.getPrefixedName() match {
                    case "MSSearchSettings" =>
                      searchSettingsCandidate = new ArrayBuffer[String]()
                      goThrough(MSSearchSettingsSet_firstChild.childCursor.advance)
                      checkSettingSetSimilarity // throws an exception if they do not match (it is pointless to continue if they don't)
                      searchSettingsCandidate = null
                    case _ => // this case should never occur
                  }
                  MSSearchSettingsSet_firstChild.advance()
                }
                logger.info("All search settings match")

              case _ => // this case should never occur
            }
            MSRequest_firstChild.advance()
          }
        /***RESPONSE-MATCHES*************************************************************************/
        case "MSSearch_response" => // this part contains the matches for each spectra and the detail of each protein
          // advance the cursor to the first child of MSSearch_response (MSResponse)
          val MSResponse = MSSearch_firstChild.childElementCursor().advance()
          // advance the cursor to the first child of MSResponse (which appears only once)
          val MSResponse_firstChild = MSResponse.childElementCursor().advance()
          while (MSResponse_firstChild.getCurrEvent() != null) {
            MSResponse_firstChild.getPrefixedName() match {
              case "MSResponse_hitsets" =>
                val MSHitSet = MSResponse_firstChild.childElementCursor().advance()
                // for each query
                while (MSHitSet.getCurrEvent() != null) {
                  // other variable to prepare
                  var hitSetNumber = 0 // is this used ?? yes, to get the Ms2Query object
                  // advance the cursor to the first child of the query
                  val MSHitSet_firstChild = MSHitSet.childElementCursor().advance()
                  while (MSHitSet_firstChild.getCurrEvent() != null) {
                    MSHitSet_firstChild.getPrefixedName() match {
                      case "MSHitSet_number" => // the id of the query (corresponds to the id of the spectrum)
                        hitSetNumber = MSHitSet_firstChild.collectDescendantText(false).toInt
                      case "MSHitSet_hits" =>
                        var peptideMatchRank: Int = 0
                        // advance the cursor to the first child of MSHitSet_hits (MSHits -> one per PeptideMatch)
                        val MSHits = MSHitSet_firstChild.childElementCursor().advance()
                        while (MSHits.getCurrEvent() != null) {
                          peptideMatchRank += 1
                          // prepare the variables for the peptideMatch
                          var peptideSequence = ""
                          var peptideCharge = 0
                          val peptideLocatedPtms = new ArrayBuffer[LocatedPtm]()
                          var peptideMatchExpectValue: Double = 0
                          var peptideMatchPValue: Float = 0 // this must be stored in the properties
                          // the MoZ value is : (calculated mass - theoretical mass) / charge
                          var peptideMatchDeltaMoz: Float = 0 // warning : this value has to be divided by mozScale afterwards (the value is not read at this point)
                          var peptideMatchIsDecoy: Boolean = false
                          var peptideMatchFragmentMatchesCount: Int = 0
                          val proteinMatches = new ArrayBuffer[ProteinMatch]()
                          var sequenceMatchResidueBefore: Option[Char] = None
                          var sequenceMatchResidueAfter: Option[Char] = None
                          val proteinMatchIdToSequenceMatches = new HashMap[Int, SequenceMatch]()
                          // for each data on the hitset
                          val MSHits_firstChild = MSHits.childElementCursor().advance()
                          while (MSHits_firstChild.getCurrEvent() != null) {
                            MSHits_firstChild.getPrefixedName() match {
//                              case "MSHits_evalue" => peptideMatchExpectValue = MSHits_firstChild.collectDescendantText(false).toFloat
                              case "MSHits_evalue" => peptideMatchExpectValue = MSHits_firstChild.collectDescendantText(false).toDouble
                              case "MSHits_pvalue" => peptideMatchPValue = MSHits_firstChild.collectDescendantText(false).toFloat
                              case "MSHits_charge" => peptideCharge = MSHits_firstChild.collectDescendantText(false).toInt
                              case "MSHits_pephits" => // one MSPepHit per ProteinMatch/SequenceMatch
                                val MSPepHit = MSHits_firstChild.childElementCursor().advance()
                                var peptideMatchRank = 0
                                // for each pephit
                                while (MSPepHit.getCurrEvent() != null) {
                                  // prepare the variables for the sequence match
                                  peptideMatchRank += 1
                                  var sequenceMatchStart: Int = 0
                                  var sequenceMatchStop: Int = 0
                                  var proteinMatchAccessionNumber = ""
                                  var proteinMatchGiNumber = ""
                                  var proteinMatchDescription = ""
                                  val MSPepHit_firstChild = MSPepHit.childElementCursor().advance()
                                  while (MSPepHit_firstChild.getCurrEvent() != null) {
                                    MSPepHit_firstChild.getPrefixedName() match {
                                      case "MSPepHit_start"     => sequenceMatchStart = MSPepHit_firstChild.collectDescendantText(false).toInt
                                      case "MSPepHit_stop"      => sequenceMatchStop = MSPepHit_firstChild.collectDescendantText(false).toInt
                                      case "MSPepHit_accession" => proteinMatchAccessionNumber = MSPepHit_firstChild.collectDescendantText(false)
                                      case "MSPepHit_gi"        => proteinMatchGiNumber = MSPepHit_firstChild.collectDescendantText(false)
                                      case "MSPepHit_defline"   => proteinMatchDescription = MSPepHit_firstChild.collectDescendantText(false)
                                      case _                    =>
                                    }
                                    MSPepHit_firstChild.advance()
                                  }
                                  MSPepHit.advance()
                                  if (proteinMatchGiNumber != "") proteinMatchAccessionNumber = proteinMatchGiNumber // MSPepHit_gi is only given for ncbi databases
                                  // create and store the ProteinMatch object
                                  var proteinMatch: ProteinMatch = null
//                                  if (proteinAccessionNumbersToProteinMatches.contains(proteinMatchAccessionNumber) && proteinAccessionNumbersToProteinMatches.get(proteinMatchAccessionNumber) != None) {
//                                    // if the protein match already exists
//                                    proteinMatch = proteinAccessionNumbersToProteinMatches.get(proteinMatchAccessionNumber).get
//                                  } else { // case of a new protein match
                                    // create or get the Protein object
                                    var protein = Option.empty[Protein]
                                    val protWrapperKey = proteinMatchAccessionNumber + (seqDatabase.id)
                                    if (protAccSeqDbToProteinWrapper.contains(protWrapperKey)) { // Check if the protein has been already accessed
                                      protein = protAccSeqDbToProteinWrapper.get(protWrapperKey).get.wrappedProt
                                    } else { // Try to get Protein from repository  
                                      protein = protProvider.getProtein(proteinMatchAccessionNumber, seqDatabase)
                                      protAccSeqDbToProteinWrapper += protWrapperKey -> new ProteinWrapper(seqDatabase.id, proteinMatchAccessionNumber, protein)
                                    }
                                    // create the protein match
                                    proteinMatch = new ProteinMatch(
                                      id = ProteinMatch.generateNewId,
                                      accession = proteinMatchAccessionNumber,
                                      description = proteinMatchDescription,
                                      peptideMatchesCount = 0,
                                      scoreType = omssaScoreType,
                                      isDecoy = proteinMatchDescriptionContainsDecoyTag(proteinMatchDescription),
                                      protein = (if (protein == None) null else protein), //If protein is None => No protein is defined not protein not retrieve !
                                      seqDatabaseIds = Array(seqDatabase.id)
                                    )
                                    proteinAccessionNumbersToProteinMatches.put(proteinMatch.accession, proteinMatch)
//                                  }
                                  proteinMatches += proteinMatch
                                  // create and store the SequenceMatch object (adding +1 to the start and end position because the omssa count starts at 0)
                                  proteinMatchIdToSequenceMatches.put(
                                    proteinMatch.id,
                                    new SequenceMatch( // this sequence match is not complete for the moment
                                      start = sequenceMatchStart + 1,
                                      end = sequenceMatchStop + 1,
                                      residueBefore = 0, // not read yet
                                      residueAfter = 0 // not read yet
                                    )
                                  )
                                }
                              case "MSHits_mzhits" =>
                                val MSMZHit = MSHits_firstChild.childElementCursor().advance()
                                while (MSMZHit.getCurrEvent() != null) {
                                  MSMZHit.getPrefixedName() match {
                                    case "MSMZHit" => peptideMatchFragmentMatchesCount += 1
                                    case _         =>
                                  }
                                  MSMZHit.advance()
                                }
                              case "MSHits_pepstring" => peptideSequence = MSHits_firstChild.collectDescendantText(false) //.replace("U", "") // the selenocysteine (U) is not recognized by biojava 
                              case "MSHits_mass"      => peptideMatchDeltaMoz += MSHits_firstChild.collectDescendantText(false).toFloat
                              case "MSHits_mods" =>
                                val MSModHit = MSHits_firstChild.childElementCursor().advance()
                                // for each MSModHit
                                while (MSModHit.getCurrEvent() != null) {
                                  // for each child of MSModHit
                                  val MSModHit_firstChild = MSModHit.childElementCursor().advance()
                                  var locatedPtmSite: Int = 0
                                  var locatedPtmDefinition: Option[PtmDefinition] = None
                                  while (MSModHit_firstChild.getCurrEvent() != null) {
                                    MSModHit_firstChild.getPrefixedName() match {
                                      case "MSModHit_site"    => locatedPtmSite = MSModHit_firstChild.collectDescendantText(false).toInt
//                                      case "MSModHit_modtype" => locatedPtmDefinition = omssaLoader.ptmDefinitions.get(MSModHit_firstChild.childElementCursor().advance().collectDescendantText(false).toInt)
                                      case "MSModHit_modtype" =>
                                        val ptmId = MSModHit_firstChild.childElementCursor().advance().collectDescendantText(false).toInt
                                        if(omssaLoader.ptmDefinitions.get(ptmId).isDefined) {
                                        	locatedPtmDefinition = omssaLoader.ptmDefinitions.get(ptmId)
                                        } else {
                                        	throw new UnknownPTMException()
                                        }
                                      case _                  =>
                                    }
                                    MSModHit_firstChild.advance()
                                  }
                                  if (locatedPtmDefinition != None) { // there may be no ptm for this hit
                                    // the site may be equal to 0 for a ptm location equal to "anywhere"
                                    if (locatedPtmDefinition.get.location.matches(".+N-term$")) locatedPtmSite = 0
                                    else if (locatedPtmDefinition.get.location.matches(".+C-term$")) locatedPtmSite = -1
                                    peptideLocatedPtms += PtmDefinitionBuilder.buildLocatedPtm(
                                      ptmDef = locatedPtmDefinition.get,
                                      seqPos = locatedPtmSite)
                                  }
                                  MSModHit.advance()
                                }
                              case "MSHits_pepstart" => sequenceMatchResidueBefore = MSHits_firstChild.collectDescendantText(false).firstOption
                              case "MSHits_pepstop"  => sequenceMatchResidueAfter = MSHits_firstChild.collectDescendantText(false).firstOption
                              case "MSHits_theomass" => peptideMatchDeltaMoz -= MSHits_firstChild.collectDescendantText(false).toFloat
                              case _                 =>
                            }
                            MSHits_firstChild.advance()
                          }
                          MSHits.advance()
                          // create the Peptide object
                          val peptide = this.getOrCreatePeptide(peptideLocatedPtms, peptideSequence, pepProvider)
                          // add properties
//                          val peptideMatchOmssaProperties = new PeptideMatchOmssaProperties( pValue = peptideMatchPValue )
//                          val peptideMatchProperties = new PeptideMatchProperties( omssaProperties = Some(peptideMatchOmssaProperties) )
                          // create the PeptideMatch object
                          val peptideMatch = new PeptideMatch(
                            id = PeptideMatch.generateNewId,
                            rank = peptideMatchRank,
                            score = (-1 * scala.math.log10(peptideMatchExpectValue)).toFloat, // -log(evalue) is stored
                            scoreType = omssaScoreType,
                            deltaMoz = (peptideMatchDeltaMoz / peptideCharge) / currentFileMzScale, 
                            isDecoy = (proteinMatches.length > 0 && proteinMatches(0).isDecoy), // if the first protein match is tagged as decoy, the peptide match is decoy too
                            peptide = peptide,
                            missedCleavage = 0, // how to get this ??? count the number of amino acids corresponding to the used enzyme ?
                            fragmentMatchesCount = peptideMatchFragmentMatchesCount,
//                            properties = Some(peptideMatchProperties),
                            msQuery = msQueries.get(hitSetNumber).getOrElse(null))
                          // add the proteinMatches if the peptideMatch is the best peptideMatch for this peptide
//                          var bestPeptideMatch = peptideMatch
//                          for (pm <- peptideToPeptideMatches.get(peptide).getOrElse(new ArrayBuffer[PeptideMatch]())) {
//                            if (bestPeptideMatch.score < pm.score || (bestPeptideMatch.score == pm.score && bestPeptideMatch.id < pm.id)) bestPeptideMatch = pm
//                          }
//                          if (peptideMatch.id == bestPeptideMatch.id) {
                            // add the protein matches to the currently best peptide match
                            peptideMatchToProteinMatches.put(peptideMatch.id, proteinMatches)
                            for ((proteinMatchId, sequenceMatch) <- proteinMatchIdToSequenceMatches) {
                              // creating the final SequenceMatch object
                              peptideMatchProteinMatchToSequenceMatch(peptideMatch.id, proteinMatchId) = new SequenceMatch(
                                  start = sequenceMatch.start,
                                  end = sequenceMatch.end,
                                  residueBefore = sequenceMatchResidueBefore.getOrElse('-'),
                                  residueAfter = sequenceMatchResidueAfter.getOrElse('-')//,
//                                  peptide = Some(bestPeptideMatch.peptide),
//                                  bestPeptideMatch = Some(bestPeptideMatch),
//                                  isDecoy = bestPeptideMatch.isDecoy
                              )
                            }
//                          }
                          peptideToPeptideMatches.getOrElseUpdate(peptide, new ArrayBuffer[PeptideMatch]) += peptideMatch
                        }
                      case _ => // contains useless information
                    }
                    MSHitSet_firstChild.advance()
                  }
                  MSHitSet.advance()
                }

              case "MSResponse_scale" => currentFileMzScale = MSResponse_firstChild.collectDescendantText(false).toInt
              case "MSResponse_dbversion" => nbSequencesInFastaFile = MSResponse_firstChild.collectDescendantText(false).toInt
              case _                  => // the bioseq part is not read
            }
            MSResponse_firstChild.advance()
          }

        /***END*************************************************************************/
        case _ => // this case should never occur
      }
      MSSearch_firstChild.advance()
    }
    // close the stream
    MSSearch.getStreamReader().closeCompletely()

    logger.info("readOmxFile ended correctly")
  }

  /**
   * Search for OM Peptide corresponding to specified ms_peptide. Search / Creation is done as follow
   *   - Calculate associated unique key and retrieve peptide from pepByUniqueKey
   *   - Get peptide from repository using specified IPTMProvider (and store it in pepByUniqueKey)
   *   - Create new one !(and store it in pepByUniqueKey)
   */
  private def getOrCreatePeptide(peptideLocatedPtms: ArrayBuffer[LocatedPtm], peptideSequence: String, pepProvider: IPeptideProvider): Peptide = {

    val tmp = pepByUniqueKey.size
    val peptideLocatedPtmsAsArray = peptideLocatedPtms.toArray[LocatedPtm]
    // 1. retrieve peptide from pepByUniqueKey
    val uniqueKey = peptideSequence + "%" + Peptide.makePtmString(peptideLocatedPtmsAsArray)
    var currentPeptide = pepByUniqueKey.get(uniqueKey)
    var storeInMap = false
    // 2. get peptide from repository 
    if (currentPeptide == None) {
      storeInMap = true
      currentPeptide = pepProvider.getPeptide(peptideSequence, peptideLocatedPtmsAsArray)
    }
    // 3. create new one 
    if (currentPeptide == None) {
      currentPeptide = Some(new Peptide(sequence = peptideSequence, ptms = peptideLocatedPtmsAsArray))
    }
    if (storeInMap) { pepByUniqueKey += (uniqueKey -> currentPeptide.get) }
    // return the Peptide object
    return currentPeptide.get
  }

  /*
   * this function is only called once (as all other settings have to match) during the parsing
   * it retrieves all required parameters for proline core items creation
   * it use an case class because it is not possible to declaer and instanciate later an object type
   */
  private def parseMSISearch(nbSpectra: Int, seqDbProvider: ISeqDatabaseProvider) = {
    // prepare variables
    var inputFileType = ""
    var inputFilePath = ""
//    var outputFilePath = ""
    //    var usedEnzymes = new ArrayBuffer[String]()
    var usedEnzymes = new ArrayBuffer[fr.proline.core.om.model.msi.Enzyme]()
    var maxMissedCleavages = -1
    var msLevel = 1; //var settingId = 0
    var msVarPtms = new ArrayBuffer[PtmDefinition]()
    var msFixedPtms = new ArrayBuffer[PtmDefinition]()
    var dbName = ""
    val version = ""
    var taxonomies = new ArrayBuffer[String]()
    var minMsChargeState = -1
    var maxMsChargeState = -1
    var ms1ErrorTol = -1.0
    var ms1ErrorTolUnit = omssaLoader.toleranceUnit("")

    ms1ErrorTol = extract(find("MSSearchSettings_peptol")).toDouble
    maxMissedCleavages = extract(find("MSSearchSettings_missedcleave")).toInt
    dbName = extract(find("MSSearchSettings_db"))
    minMsChargeState = extract(find("MSChargeHandle_mincharge")).toInt
    maxMsChargeState = extract(find("MSChargeHandle_maxcharge")).toInt
    ms1ErrorTolUnit = omssaLoader.toleranceUnit(extract(find("MSSearchSettings_pepppm/value")))
    inputFileType = omssaLoader.spectrumFileTypes(extract(find("MSSpectrumFileType")).toInt)
//    outputFilePath = omxFile.getAbsolutePath
    inputFilePath = parseProperties.get(OmssaParseParams.PEAK_LIST_FILE_PATH).toString

    searchSettingsReference.filter(element => element.contains("MSSearchSettings_fixed/MSMod/*>")).foreach(
      element => {
        val ptm: Option[PtmDefinition] = omssaLoader.ptmDefinitions.get(extract(element).toInt)
        if (ptm != None) msFixedPtms += ptm.get
      })
    searchSettingsReference.filter(element => element.contains("MSSearchSettings_variable/MSMod/*>")).foreach(
      element => {
        val ptm: Option[PtmDefinition] = omssaLoader.ptmDefinitions.get(extract(element).toInt)
        if (ptm != None) msVarPtms += ptm.get
      })
    searchSettingsReference.filter(element => element.contains("MSSearchSettings_taxids_E/*>")).foreach(
      element => {
        taxonomies += extract(element)
      })
    searchSettingsReference.filter(element => element.contains("MSEnzymes/*>")).foreach(
      element => {
        //        usedEnzymes += omssaLoader.enzymes.get(extract(element).toInt).getOrElse("")
        usedEnzymes += new fr.proline.core.om.model.msi.Enzyme(omssaLoader.enzymes.get(extract(element).toInt).getOrElse(""))
      })

    val fastaFilePath = parseProperties.get(OmssaParseParams.FASTA_FILE_PATH).toString
    var usedSeqSDb = seqDbProvider.getSeqDatabase(dbName, fastaFilePath)

    seqDatabase = null
    if (usedSeqSDb != None) seqDatabase = usedSeqSDb.get
    else {
      logger.warn("Sequence DB used for identification is not referenced in system ... First load data in repository ")
      seqDatabase = new SeqDatabase(
        id = SeqDatabase.generateNewId,
        name = dbName,
        filePath = parseProperties.get(OmssaParseParams.FASTA_FILE_PATH).toString,
        sequencesCount = nbSequencesInFastaFile,
        version = version,
        releaseDate = new java.util.Date)
    }

    var allChargeStates = new ArrayBuffer[String]()
    for (i <- minMsChargeState until maxMsChargeState) allChargeStates += i.toString()
    var chargeStates = ""
    if (allChargeStates.size > 0) chargeStates = allChargeStates.reduceLeft(_ + ", " + _)
    var strTaxonomies = ""
    if (taxonomies.size > 0) strTaxonomies = taxonomies.reduceLeft(_ + ", " + _)

    var searchSettings: SearchSettings = new SearchSettings(
      id = SearchSettings.generateNewId(),
      softwareName = "OMSSA",
      //      softwareVersion = parseProperties.getOrElse(OmssaParseParams.OMSSA_VERSION, omssaDefaultVersion).toString, // not in omx file
      //      taxonomy = parseProperties.getOrElse(OmssaParseParams.FASTA_TAXONOMIES, "").toString, // the mark up exists for this data, but is not used at this moment
      softwareVersion = parseProperties.get(OmssaParseParams.OMSSA_VERSION).toString, // not in omx file
      taxonomy = parseProperties.get(OmssaParseParams.FASTA_TAXONOMIES).toString, // the mark up exists for this data, but is not used at this moment
      maxMissedCleavages = maxMissedCleavages,
      ms1ChargeStates = chargeStates,
      ms1ErrorTol = ms1ErrorTol,
      ms1ErrorTolUnit = ms1ErrorTolUnit,
      isDecoy = (!hasTargetResultSet && hasDecoyResultSet),
      usedEnzymes = usedEnzymes.toArray,
      variablePtmDefs = msVarPtms.toArray,
      fixedPtmDefs = msFixedPtms.toArray,
      seqDatabases = Array(seqDatabase),
      instrumentConfig = this.instrumentConfig,
      quantitation = "")

    //Create MSISearch regrouping all these information
    msiSearch = new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = omxFile.getName(),
      submittedQueriesCount = nbSpectra,
      searchSettings = searchSettings,
      peakList = peaklist,
      date = new java.util.Date,
      resultFileDirectory = omxFile.getPath(),
      queriesCount = nbSpectra)
    //      properties = Some(convertSettingsInJson))
  }

  /**
   * The purpose of this function is to create an object that will contain all the omssa settings in Json format
   * Because MSISearch object can not contain all of them, and because these settings can be important
   * @return a SearchSettingsProperties object (empty for the moment)
   */
  private lazy val omssaSettingsInJsonFormat: ArrayBuffer[JObject] = {
    var tmpOmssaSettingsInJsonFormat = new ArrayBuffer[JObject]
    searchSettingsReference.filter(element => element.contains("*>")).foreach(element => tmpOmssaSettingsInJsonFormat += putSettingsInJsonCompatibleObjects(element))
    tmpOmssaSettingsInJsonFormat
    //    logger.debug(pretty(render(omssaSettingsInJsonFormat)))
    //    var properties = new SearchSettingsProperties
    // properties = compact(render(omssaSettingsInJsonFormat))
    //    return properties
  }
  /**
   * Recursive function to gather all the omssa settings in json
   * @param setting : the setting returned by the function that parses the omssa xml file
   * setting should look like one of those :
   * /MSSearchSettings_precursorsearchtype/MSSearchType/ *>4 => MSSearchSettings_precursorsearchtype->MSSearchType->4
   * /MSSearchSettings_peptol/ *>0.25 => MSSearchSettings_peptol->0.25
   * /MSSearchSettings_db/ *>db/spHomo_DCpSP_ABU_20121206 => MSSearchSettings_db => db/spHomo_DCpSP_ABU_20121206
   * /MSSearchSettings_chargehandling/MSChargeHandle/MSChargeHandle_calcplusone/MSCalcPlusOne/ *>1 => MSSearchSettings_chargehandling->MSChargeHandle->MSChargeHandle_calcplusone->MSCalcPlusOne->1
   * @return a JObject containing the current level and the children
   */
  private def putSettingsInJsonCompatibleObjects(_setting: String): JObject = {
    // split the setting to separate the current key to the rest of the string
    val setting = _setting.replaceFirst("/", "")
    val index = setting.indexOf("/")
    val key = setting.substring(0, index)
    val value = setting.substring(index)
    // if the rest of the string is the value
    if (value.startsWith("/*>")) {
      // return the value (exit case)
      return (key -> value.replace("/*>", ""))
    } else {
      // otherwise call the function recursively to get the JObject
      return (key -> putSettingsInJsonCompatibleObjects(value))
    }
  }

  /*
   * this function extract value from cutted path/ *>value
   */
  private def extract(input: String): String = {
    if (input.isEmpty) return ""
    val v = input drop input.indexOf(">") + 1
    if (v.isEmpty) return ""
    return v
  }

  /*
   * this function seek the value token inside the settings and return : cutted path/ *>value
   */
  private def find(token: String): String = {
    if (token.isEmpty)
      return ""
    val v = searchSettingsReference.find((element: String) => element.contains(token + "/*>")).getOrElse("")
    return v
  }

  /*
   * goThrough is a forest depth-first go through recursively
   * each node try to watch his son then his brother, not finding the current event mean the end of this branch
   * 
   * as it crosses settings generically, it stores automatically all node's path and, if applicable, it value 
   * the path is cutted at start to make match first settings and other. The initial call as to be inside _/MSSearchSettings/_
   * it is store for all node to
   * value has two cases :
   * . a textual event happens when Staxmate capture x in <tag>x</tag>, you cannot access generically it before getting there
   * . a element event  happens in all other case but sometimes (esp. in new version of omssa file stores values as attributes of the tag),
   * so in this case, they are read and store individually
   * 
   *  in brief :
   *  element w/o attributes -> add just the cutted path
   *  element w/  attributes -> add cutted path/attributename/ *>attributevalue (for each attribute)
   *  textual                -> add cutted path/ *>trimedvalue
   *  
   *  the "/ *>" pattern is then easy to parse
   * 
   */
  private def goThrough(node: SMInputCursor) {
    if (node.getCurrEvent == null) {
      return
    }
    if (node.getCurrEvent.isElementEvent) {
      val path = node.getPathDesc.replaceAll("\\[...?\\]", "").replaceAll("^.*\\/MSSearchSettings(\\/|$)", "/")
      if (path.matches(".*MSSearchSettings_settingid$") || path.matches(".*MSInFile_infile$") || path.matches(".*MSOutFile_outfile$")) {
        return goThrough(node.advance)
      }
      var nbAttr = node.getAttrCount()
      if (nbAttr > 0) {
        for (i <- 0.until(nbAttr)) {
          searchSettingsCandidate += path + "/" + node.getAttrLocalName(i) + "/*>" + node.getAttrValue(i)
        }
      } else searchSettingsCandidate += path
    }
    if (node.getCurrEvent.isTextualEvent) {
      val text = node.getText.trim
      if (!text.isEmpty) searchSettingsCandidate += node.getPathDesc.replaceAll("\\[...?\\]", "").replaceAll("^.*\\/MSSearchSettings(\\/|$)", "/") + ">" + text
    } else goThrough(node.childCursor.advance) // son
    goThrough(node.advance) // bro
  }

  /*
   * this function is called after each othersetting go through to catch if it match the reference (first one)
   * the first analysis try to find each reference element inside the last parsed setting (inverse isn't enought)
   * the second try to catch when there are different number of occurence of a line in both (which doesn't have to happen they are stored generically)
   * 
   * this verification order offer the possibility to watch which elements differ in most of case
   */
  private def checkSettingSetSimilarity: Boolean = {
    val mainErrorMessage = "Multiple sets of settings with heterogeneous search settings (this OMSSA file is the merge of different OMSSA searches)"
    searchSettingsReference.foreach(
      (element: String) => if (searchSettingsCandidate.indexOf(element) == -1) {
//        throw new NotMatchingSearchSettingsException("|ref| : " + searchSettingsReference.length + " vs |curr| : " + searchSettingsCandidate.length + "\r\nelement : " + element)
        throw new NotMatchingSearchSettingsException(mainErrorMessage + " : The setting '" + element + "' has at least two different values")
        return false
      })
    if (searchSettingsReference.length != searchSettingsCandidate.length) {
//      throw new NotMatchingSearchSettingsException("|ref| : " + searchSettingsReference.length + " vs |curr| : " + searchSettingsCandidate.length)
      throw new NotMatchingSearchSettingsException(mainErrorMessage + " : The number of settings is different")
      return false
    }
    true
  }

  /**
   * this method tells if a proteinMatch object is target or decoy
   * this method is temporary and should be replaced by a global ProlineCore method
   * @param proteinMatch the proteinMatch to analyse
   * @return true if the proteinMatch object is tagged as decoy
   */
  private def proteinMatchIsDecoy(proteinMatch: ProteinMatch): Boolean = {
    // look at the global parameters
    if (hasTargetResultSet && !hasDecoyResultSet) return false // if the file contains only target entries
    else if (!hasTargetResultSet && hasDecoyResultSet) return true // if the file contains only decoy entries
    // otherwise use a regex on the accession number or the description to find to know if the protein is a decoy
    //    else if (proteinMatch.description.matches("^Reverse sequence, was .*")) return true
    else if (proteinMatchDescriptionContainsDecoyTag(proteinMatch.description)) return true
    //logger.debug(proteinMatch.accession + ": "+ proteinMatch.description+ " is decoy : "+isDecoy+" ("+hasTargetResultSet+"/"+hasDecoyResultSet+")")
    return false
  }
  private def proteinMatchDescriptionContainsDecoyTag(description: String): Boolean = {
    return description.matches("^Reverse sequence, was .*")

  }
}
