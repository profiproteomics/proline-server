package fr.proline.module.parser.omssa

import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.msi.{ IProteinProvider, ISeqDatabaseProvider, IPeptideProvider }
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import java.io.File
import javax.xml.stream.XMLInputFactory
import org.codehaus.staxmate.in.{ SMHierarchicCursor, SMInputCursor }
import org.codehaus.staxmate.SMInputFactory
import com.typesafe.scalalogging.LazyLogging
import fr.proline.context.DatabaseConnectionContext
import java.io.InputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import org.apache.commons.compress.compressors.CompressorStreamFactory

case class ProteinWrapper(val seqdbId: Long, val protAccess: String, var wrappedProt: Option[Protein]) {
  override def equals(other: Any): Boolean = {
    other match {
      case otherRefProt: ProteinWrapper => return protAccess.equals(otherRefProt.protAccess) && seqdbId.equals(otherRefProt.seqdbId)
      case _                            => return false
    }
  }
}
case class simpleSequenceMatch(val accessionNumber: String, val start: Int, val stop: Int)

object OmssaReadFile {
  private def getBz2FileAsStream(omxFile: File) : InputStream = {
    val fis: FileInputStream = new FileInputStream(omxFile)
    val bis: BufferedInputStream = new BufferedInputStream(fis)
    new CompressorStreamFactory().createCompressorInputStream(bis)
  }
  def openOmxFile(inf: SMInputFactory, omxFile: File) : SMHierarchicCursor = {
    if(omxFile.getName().endsWith(".omx.bz2"))
      inf.rootElementCursor(this.getBz2FileAsStream(omxFile))
    else
      inf.rootElementCursor(omxFile)
  }
}

class OmssaReadFile(val omxFile: File,
                    val parseProperties: Map[OmssaParseParams.OmssaParseParam, Any],
                    val omssaLoader: OmssaMandatoryFilesLoader, 
                    val parserContext: ProviderDecoratedExecutionContext
                    ) extends LazyLogging {

  private val OMSSA_HIGHEST_SCORE = 20
  
  private val omssaScoreType = PeptideMatchScoreType.OMSSA_EVALUE
  private val omssaScoreTypeAsStr = "omssa:expect value"
  private var pepByUniqueKey: HashMap[String, Peptide] = null
  def getPeptideByUniqueKey: HashMap[String, Peptide] = pepByUniqueKey
  private var msQueries: HashMap[Int, Ms2Query] = null
  def getMsQueries: HashMap[Int, Ms2Query] = msQueries
  private var protAccSeqDbToProteinWrapper: HashMap[String, ProteinWrapper] = null
  private var peptideToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]] = null
  def getPeptideToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]] = peptideToPeptideMatches
  private var peptideMatchToProteinMatches: HashMap[Long, ArrayBuffer[ProteinMatch]] = null
  def getPeptideMatchToProteinMatches: HashMap[Long, ArrayBuffer[ProteinMatch]] = peptideMatchToProteinMatches
  private var peptideMatchProteinMatchToSequenceMatch: TwoDimensionsMap[Long, Long, SequenceMatch] = null
  def getPeptideMatchProteinMatchToSequenceMatch: TwoDimensionsMap[Long, Long, SequenceMatch] = peptideMatchProteinMatchToSequenceMatch
  private var proteinAccessionNumbersToProteinMatches: HashMap[String, ProteinMatch] = null
  def mzScale: Int = currentFileMzScale
  private var searchSettingsReference: ArrayBuffer[String] = null
  private var searchSettingsCandidate: ArrayBuffer[String] = null
  private var seqDatabase: SeqDatabase = null
  private var msiSearch: MSISearch = null
  def getMsiSearch: MSISearch = msiSearch
  val omssaPreloader = new OmssaFilePreloader(omxFile)
  private val currentFileMzScale = omssaPreloader.mozScaleValue
  private val nbSequencesInFastaFile = omssaPreloader.nbSequencesInFastaFile
  private var allowNtermMethionineCleavage = true
  private var peaklist: Peaklist = null
  def getPeaklist: Peaklist = {
    if(peaklist == null) setPeaklist("")
    peaklist
  }
  private val proteinSequencesByInternalIds = omssaPreloader.proteinInternalIdToSequence

  _parseOmxFile()

  private def _parseOmxFile() {
    pepByUniqueKey = new HashMap[String, Peptide]()
    var pepProvider = parserContext.getProvider(classOf[IPeptideProvider])
    var protProvider = parserContext.getProvider(classOf[IProteinProvider])
    val seqDbProvider = parserContext.getProvider(classOf[ISeqDatabaseProvider])
    msQueries = new HashMap[Int, Ms2Query]()
    protAccSeqDbToProteinWrapper = new HashMap[String, ProteinWrapper]()
    peptideToPeptideMatches = new HashMap[Peptide, ArrayBuffer[PeptideMatch]]()
    peptideMatchToProteinMatches = new HashMap[Long, ArrayBuffer[ProteinMatch]]()
    peptideMatchProteinMatchToSequenceMatch = new TwoDimensionsMap[Long, Long, SequenceMatch]()
    proteinAccessionNumbersToProteinMatches = new HashMap[String, ProteinMatch]()

    logger.info("readOmxFile(" + omxFile.getAbsolutePath() + ")")
    var nbSpectra: Int = 0
//    // get the root cursor
    val MSSearch: SMHierarchicCursor = OmssaReadFile.openOmxFile(new SMInputFactory(XMLInputFactory.newInstance()), omxFile)
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
                          case "MSSpectrum_ids"         => spectrumTitle = MSSpectrum_children.childElementCursor().advance().collectDescendantText(false).replace("\\\"", "\"").replace('\\', '/')
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
                          id = Ms2Query.generateNewId,
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
                          val peptideMatchIonSeries = new ArrayBuffer[String]()
                          // the MoZ value is : (calculated mass - theoretical mass) / charge
                          var peptideMatchDeltaMoz: Float = 0 // warning : this value has to be divided by mozScale afterwards (the value is not read at this point)
                          var peptideMatchIsDecoy: Boolean = false
                          var peptideMatchFragmentMatchesCount: Int = 0
                          val proteinMatches = new ArrayBuffer[ProteinMatch]()
                          var sequenceMatchResidueBefore: Option[Char] = None
                          var sequenceMatchResidueAfter: Option[Char] = None
                          val proteinMatchIdToSequenceMatches = new HashMap[Long, SequenceMatch]()
                          // for each data on the hitset
                          val MSHits_firstChild = MSHits.childElementCursor().advance()
                          while (MSHits_firstChild.getCurrEvent() != null) {
                            MSHits_firstChild.getPrefixedName() match {
                              case "MSHits_evalue" =>
                                peptideMatchExpectValue = MSHits_firstChild.collectDescendantText(false).toDouble
                              case "MSHits_pvalue" => peptideMatchPValue = MSHits_firstChild.collectDescendantText(false).toFloat
                              case "MSHits_charge" => peptideCharge = MSHits_firstChild.collectDescendantText(false).toInt
                              case "MSHits_pephits" => // one MSPepHit per ProteinMatch/SequenceMatch
                                val MSPepHit = MSHits_firstChild.childElementCursor().advance()
                                var peptideMatchRank = 0
                                // variable used to check that the sequence match does not already exist (bug in omssa)
                                val simpleSequenceMatches = new ArrayBuffer[simpleSequenceMatch]()
                                // for each pephit
                                while (MSPepHit.getCurrEvent() != null) {
                                  // prepare the variables for the sequence match
                                  peptideMatchRank += 1
                                  var sequenceMatchStart: Int = 0
                                  var sequenceMatchStop: Int = 0
                                  var proteinMatchAccessionNumber = ""
                                  var proteinMatchGiNumber = ""
                                  var proteinMatchDescription = ""
                                  var proteinSequence = ""
                                  val MSPepHit_firstChild = MSPepHit.childElementCursor().advance()
                                  while (MSPepHit_firstChild.getCurrEvent() != null) {
                                    MSPepHit_firstChild.getPrefixedName() match {
                                      case "MSPepHit_start"     => sequenceMatchStart = MSPepHit_firstChild.collectDescendantText(false).toInt
                                      case "MSPepHit_stop"      => sequenceMatchStop = MSPepHit_firstChild.collectDescendantText(false).toInt
                                      case "MSPepHit_accession" => proteinMatchAccessionNumber = MSPepHit_firstChild.collectDescendantText(false)
                                      case "MSPepHit_gi"        => proteinMatchGiNumber = MSPepHit_firstChild.collectDescendantText(false)
                                      case "MSPepHit_defline"   => proteinMatchDescription = MSPepHit_firstChild.collectDescendantText(false)
                                      case "MSPepHit_oid"       => proteinSequence = proteinSequencesByInternalIds.get(MSPepHit_firstChild.collectDescendantText(false).toInt).getOrElse("")
                                      case _                    =>
                                    }
                                    MSPepHit_firstChild.advance()
                                  }
                                  MSPepHit.advance()
                                  if (proteinMatchGiNumber != "") proteinMatchAccessionNumber = proteinMatchGiNumber // MSPepHit_gi is only given for ncbi databases

                                  // check that the sequence match does not already exist (bug in omssa)
                                  if (!simpleSequenceMatches.contains(new simpleSequenceMatch(proteinMatchAccessionNumber, sequenceMatchStart, sequenceMatchStop))) {
                                    // create and store the ProteinMatch object
                                    var proteinMatch: ProteinMatch = null
                                    // create or get the Protein object
                                    var protein = Option.empty[Protein]
                                    val protWrapperKey = proteinMatchAccessionNumber + (seqDatabase.id)
                                    if (protAccSeqDbToProteinWrapper.contains(protWrapperKey)) { // Check if the protein has been already accessed
                                      protein = protAccSeqDbToProteinWrapper.get(protWrapperKey).get.wrappedProt
                                    } else { // Try to get Protein from repository  
                                      protein = protProvider.getProtein(proteinMatchAccessionNumber, seqDatabase)
                                      if(protein == None && proteinSequence != "") {
                                        logger.debug("Adding sequence to protein "+proteinMatchAccessionNumber)
                                        protein = Some(new Protein(id = Protein.generateNewId, sequence = proteinSequence))
                                      }
                                      protAccSeqDbToProteinWrapper += protWrapperKey -> new ProteinWrapper(seqDatabase.id, proteinMatchAccessionNumber, protein)
                                    }
                                    // create the protein match
                                    proteinMatch = new ProteinMatch(
                                      id = ProteinMatch.generateNewId,
                                      accession = proteinMatchAccessionNumber,
                                      description = proteinMatchDescription,
                                      peptideMatchesCount = 0,
                                      scoreType = omssaScoreTypeAsStr,
                                      isDecoy = false,
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
                                    simpleSequenceMatches += new simpleSequenceMatch(proteinMatchAccessionNumber, sequenceMatchStart, sequenceMatchStop)
                                  }
                                }
                              case "MSHits_mzhits" =>
                                val MSMZHits = MSHits_firstChild.childElementCursor().advance()
                                while (MSMZHits.getCurrEvent() != null) {
                                  MSMZHits.getPrefixedName() match {
                                    case "MSMZHit" => 
                                      val MSMZHit = MSMZHits.childElementCursor().advance()
                                      peptideMatchFragmentMatchesCount += 1
                                      while (MSMZHit.getCurrEvent() != null) {
                                        MSMZHit.getPrefixedName() match {
                                          case "MSMZHit_ion" => peptideMatchIonSeries += omssaLoader.ionTypes.get(MSMZHit.childElementCursor().advance().collectDescendantText(false).toInt).getOrElse("")
                                          case _ =>
                                        }
                                        MSMZHit.advance()
                                      }
                                    case _         =>
                                  }
                                  MSMZHits.advance()
                                }
                              case "MSHits_pepstring" => peptideSequence = MSHits_firstChild.collectDescendantText(false)
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
                                      case "MSModHit_site" => locatedPtmSite = MSModHit_firstChild.collectDescendantText(false).toInt + 1
                                      case "MSModHit_modtype" =>
                                        val ptmId = MSModHit_firstChild.childElementCursor().advance().collectDescendantText(false).toInt
                                        val myPtm = omssaLoader.getPtmDefinition(ptmId, peptideSequence.charAt(locatedPtmSite - 1))
                                        if (myPtm.isDefined) {
                                          locatedPtmDefinition = myPtm
                                        } else {
                                          logger.error("Unknown PTM with ptmId="+ptmId+" and peptide="+peptideSequence.substring(locatedPtmSite-1)+"["+peptideSequence.substring(locatedPtmSite-1, locatedPtmSite)+"]"+peptideSequence.substring(locatedPtmSite, peptideSequence.size))
                                          throw new UnknownPTMException()
                                        }
                                      case _ =>
                                    }
                                    MSModHit_firstChild.advance()
                                  }
                                  if (locatedPtmDefinition != None) { // there may be no ptm for this hit
                                    // the site may be equal to 0 for a ptm location equal to "anywhere"
                                    if (locatedPtmDefinition.get.location.matches(".+N-term$")) locatedPtmSite = 0
                                    else if (locatedPtmDefinition.get.location.matches(".+C-term$")) locatedPtmSite = -1
                                    peptideLocatedPtms += LocatedPtm(ptmDef = locatedPtmDefinition.get, seqPos = locatedPtmSite)
                                  }
                                  MSModHit.advance()
                                }
                              case "MSHits_pepstart" => sequenceMatchResidueBefore = MSHits_firstChild.collectDescendantText(false).headOption
                              case "MSHits_pepstop"  => sequenceMatchResidueAfter = MSHits_firstChild.collectDescendantText(false).headOption
                              case "MSHits_theomass" => peptideMatchDeltaMoz -= MSHits_firstChild.collectDescendantText(false).toFloat
                              case _                 =>
                            }
                            MSHits_firstChild.advance()
                          }
                          MSHits.advance()
                          // check if any fixed ptms may match this peptide
                          msiSearch.searchSettings.fixedPtmDefs.foreach(ptm => {
                            PtmLocation.withName(ptm.location) match {
                              case PtmLocation.ANYWHERE => // fixed ptm on any position 
                                var index = peptideSequence.indexOf(ptm.residue, 0)
                                while(index != -1) { // first AA will have position 1
                                  peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = index+1)
                                  index = peptideSequence.indexOf(ptm.residue, index+1)
                                }
                              case PtmLocation.ANY_N_TERM => // fixed ptm on N-terminal amino acid
                                if(ptm.residue == '\0') peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = 0)
                                else if(peptideSequence.startsWith(ptm.residue.toString())) peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = 0)
                              case PtmLocation.ANY_C_TERM => // fixed ptm on C-terminal amino acid 
                                if(ptm.residue == '\0') peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = -1)
                                else if(peptideSequence.endsWith(ptm.residue.toString())) peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = -1)
                              case PtmLocation.PROT_N_TERM => // if residueBefore is null, then it's protein Nter
                                if(sequenceMatchResidueBefore == None) {
                                  if(ptm.residue == '\0') peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = 0)
                                  else if(peptideSequence.startsWith(ptm.residue.toString())) peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = 0)
                                } else if(allowNtermMethionineCleavage && sequenceMatchResidueBefore.get == 'M' && (ptm.residue == '\0' || ptm.residue == 'M') && proteinMatchIdToSequenceMatches.filter(_._2.start <= 2).size > 0) {
                                  // omssa has an option to allow cleavage of methionine if it's protein n-terminal
                                  // this is why there is no many conditions : option has to be activated and previous AA has to be M and first AA in the protein sequence
                                  peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = 0)
                                }
                              case PtmLocation.PROT_C_TERM => // if residueAfter is null, then it's protein Cter
                                if(sequenceMatchResidueAfter == None && (ptm.residue == '\0' || peptideSequence.endsWith(ptm.residue.toString()))) peptideLocatedPtms += LocatedPtm(ptmDef = ptm, seqPos = -1) 
                            }
                          })
                          // create the Peptide object
                          val peptide = this.getOrCreatePeptide(peptideLocatedPtms, peptideSequence, pepProvider)
                          // add properties
                          val ionSeries = peptideMatchIonSeries.filter(!_.isEmpty()).distinct.toArray
                          val ionSeriesOpt = if(ionSeries.isEmpty) None else Some(ionSeries)
                          val peptideMatchOmssaProperties = new PeptideMatchOmssaProperties(pValue = peptideMatchPValue, correctedCharge = peptideCharge, ionSeries = ionSeries)
                          val peptideMatchProperties = new PeptideMatchProperties(omssaProperties = Some(peptideMatchOmssaProperties))
                          // create the PeptideMatch object
                          if(!msQueries.isDefinedAt(hitSetNumber)) { logger.warn("No MSQuery for query "+hitSetNumber) }
                          val query =  msQueries.get(hitSetNumber).getOrElse(null)
                          val qcharge = if(query != null) {query.charge } else 0
                          val qexperimentalMz = if(query != null) {query.moz.toFloat} else -1f
                          val peptideMatch = new PeptideMatch(
                            id = PeptideMatch.generateNewId,
                            rank = peptideMatchRank,
                            score = minusLogEValue(peptideMatchExpectValue), // -log(evalue) is stored
                            scoreType = omssaScoreType,
                            charge = qcharge,
                            deltaMoz = (peptideMatchDeltaMoz / peptideCharge) / currentFileMzScale,
                            isDecoy = (proteinMatches.length > 0 && proteinMatches(0).isDecoy), // if the first protein match is tagged as decoy, the peptide match is decoy too
                            peptide = peptide,
                            missedCleavage = PeptideMatch.countMissedCleavages(peptideSequence, sequenceMatchResidueBefore, sequenceMatchResidueAfter, msiSearch.searchSettings.usedEnzymes), 
                            fragmentMatchesCount = peptideMatchFragmentMatchesCount,
                            properties = Some(peptideMatchProperties),
                            msQuery = query)
                          // add the protein matches to the currently best peptide match
                          peptideMatchToProteinMatches.put(peptideMatch.id, proteinMatches)
                          for ((proteinMatchId, sequenceMatch) <- proteinMatchIdToSequenceMatches) {
                            // creating the final SequenceMatch object
                            peptideMatchProteinMatchToSequenceMatch(peptideMatch.id, proteinMatchId) = new SequenceMatch(
                              start = sequenceMatch.start,
                              end = sequenceMatch.end,
                              residueBefore = sequenceMatchResidueBefore.getOrElse('-'),
                              residueAfter = sequenceMatchResidueAfter.getOrElse('-')
                            )
                          }
                          peptideToPeptideMatches.getOrElseUpdate(peptide, new ArrayBuffer[PeptideMatch]) += peptideMatch
                        }
                      case _ => // contains useless information
                    }
                    MSHitSet_firstChild.advance()
                  }
                  MSHitSet.advance()
                }
              case _                      => // the bioseq part is not read
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
   * 20 should be the maximal score value (very low or null evalues would give infinite score)
   * @param evalue the e-value read in the omssa file
   * @return -log(e-value)
   */
  private def minusLogEValue(evalue: Double): Float = {
    var score = (-1 * scala.math.log10(evalue.toFloat)).toFloat
    if(score > OMSSA_HIGHEST_SCORE) score = OMSSA_HIGHEST_SCORE
    score
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
    val msiSearchProvider = new SQLMsiSearchProvider(parserContext.getUDSDbConnectionContext(), parserContext.getMSIDbConnectionContext(), parserContext.getPSDbConnectionContext())
    val msLevel = 2
    val msVarPtms = new ArrayBuffer[PtmDefinition]()
    val msFixedPtms = new ArrayBuffer[PtmDefinition]()
    val version = ""
    val taxonomies = new ArrayBuffer[String]()
    val ms1ErrorTol = extract(find("MSSearchSettings_peptol")).toDouble
    val maxMissedCleavages = extract(find("MSSearchSettings_missedcleave")).toInt
    val dbName = extract(find("MSSearchSettings_db"))
    val minMsChargeState = extract(find("MSChargeHandle_mincharge")).toInt
    val maxMsChargeState = extract(find("MSChargeHandle_maxcharge")).toInt
    val ms1ErrorTolUnit = omssaLoader.toleranceUnit(extract(find("MSSearchSettings_pepppm/value")))
    val inputFileType = omssaLoader.spectrumFileTypes(extract(find("MSSpectrumFileType")).toInt)
    val inputFilePath = parseProperties.get(OmssaParseParams.PEAK_LIST_FILE_PATH).toString
    allowNtermMethionineCleavage = extract(find("MSSearchSettings_nmethionine/value")).equals("true")
    
    val msmstol = extract(find("MSSearchSettings_msmstol")).toDouble
    val msmstolUnit = omssaLoader.toleranceUnit(extract(find("MSSearchSettings_msmsppm/value")))
    val msmsChargeState = extract(find("MSChargeHandle_maxproductcharge")).toInt
    val ionTypes = new ArrayBuffer[String]()

    searchSettingsReference.filter(e => e.contains("MSSearchSettings_fixed/MSMod/*>")).foreach(e => omssaLoader.getPtmDefinitions(extract(e).toLong).foreach(msFixedPtms += _))
    searchSettingsReference.filter(e => e.contains("MSSearchSettings_variable/MSMod/*>")).foreach(e => omssaLoader.getPtmDefinitions(extract(e).toLong).foreach(msVarPtms += _))
    searchSettingsReference.filter(e => e.contains("MSSearchSettings_taxids_E/*>")).foreach(taxonomies += extract(_))
    searchSettingsReference.filter(e => e.contains("MSIonType/*")).foreach(e => omssaLoader.ionTypes(extract(e).toInt).foreach(ionTypes += _.toString))
    val usedEnzymes = msiSearchProvider.getEnzymesByName(searchSettingsReference.filter(element => element.contains("MSEnzymes/*>")).map(e => omssaLoader.enzymes.get(extract(e).toInt).getOrElse("")).toSeq)

    val fastaFilePath = if(parseProperties.get(OmssaParseParams.FASTA_FILE_PATH).isDefined) parseProperties.get(OmssaParseParams.FASTA_FILE_PATH).get.toString + File.pathSeparator + dbName else ""
    val usedSeqSDb = seqDbProvider.getSeqDatabase(dbName, fastaFilePath)

    seqDatabase = null
    if (usedSeqSDb != None) seqDatabase = usedSeqSDb.get
    else {
      logger.warn("Sequence DB used for identification is not referenced in system ... First load data in repository ")
      seqDatabase = new SeqDatabase(
        id = SeqDatabase.generateNewId,
        name = dbName,
        filePath = fastaFilePath,
        sequencesCount = nbSequencesInFastaFile,
        searchedSequencesCount = nbSequencesInFastaFile,
        version = version,
        releaseDate = new java.util.Date,
        properties = None,
        searchProperties = None
      )
    }

    val allChargeStates = new ArrayBuffer[String]()
    for (i <- minMsChargeState to maxMsChargeState) allChargeStates += i.toString()
    var chargeStates = ""
    if (allChargeStates.size > 0) chargeStates = allChargeStates.reduceLeft(_ + ", " + _)
    var strTaxonomies = ""
    if (taxonomies.size > 0) strTaxonomies = taxonomies.reduceLeft(_ + ", " + _)

    val searchSettings = SearchSettings(
      id = SearchSettings.generateNewId(),
      softwareName = "OMSSA",
      softwareVersion = parseProperties.get(OmssaParseParams.OMSSA_VERSION).getOrElse("").toString, // not in omx file
      taxonomy = parseProperties.get(OmssaParseParams.FASTA_TAXONOMIES).getOrElse("").toString, // the mark up exists for this data, but is not used at this moment (check for MSSearchSettings_taxids)
      maxMissedCleavages = maxMissedCleavages,
      ms1ChargeStates = chargeStates,
      ms1ErrorTol = ms1ErrorTol,
      ms1ErrorTolUnit = ms1ErrorTolUnit,
      isDecoy = false,
      usedEnzymes = usedEnzymes,
      variablePtmDefs = msVarPtms.toArray,
      fixedPtmDefs = msFixedPtms.toArray,
      seqDatabases = Array(seqDatabase),
      instrumentConfig = null // not instanciated at this moment
    )
    
    if (msLevel == 2) {
      searchSettings.msmsSearchSettings = Some(
        new MSMSSearchSettings(
          ms2ChargeStates = msmsChargeState.toString,
          ms2ErrorTol = msmstol,
          ms2ErrorTolUnit = msmstolUnit)
      )
    }

    searchSettingsReference.filter(element => element.contains("/MSInFile_infile/")).foreach(element => { setPeaklist(extract(element)) })
    
    //Create MSISearch regrouping all these information
    msiSearch = MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = omxFile.getName(),
      searchSettings = searchSettings,
      peakList = peaklist,
      date = new java.util.Date(omxFile.lastModified()), // the lastModified date of the file will not always correspond to the search date
      title = omxFile.getName(),
      resultFileDirectory = omxFile.getParentFile().getAbsolutePath(),
      jobNumber = 0,
      userName = "",
      userEmail = "",
      queriesCount = nbSpectra,
      searchedSequencesCount = nbSequencesInFastaFile
    )
  }

  private def setPeaklist(peaklistPath: String) = {
    var peaklistType = "mgf"; // mgf is the default value
    omssaLoader.spectrumFileTypes.foreach { fileType => if (peaklistPath.matches("." + fileType._2 + "$")) peaklistType = fileType._2 } // look at the file extension
    peaklist = new Peaklist(
      id = Peaklist.generateNewId,
      fileType = peaklistType,
      path = if(peaklistPath != "") peaklistPath else parseProperties.get(OmssaParseParams.PEAK_LIST_FILE_PATH).getOrElse("").toString,
      rawFileIdentifier = parseProperties.get(OmssaParseParams.RAW_FILE_PATH).getOrElse("").toString,
      msLevel = 2
    )
  }
  
  /**
   * The purpose of this function is to create an object that will contain all the omssa settings in Json format
   * Because MSISearch object can not contain all of them, and because these settings can be important
   * @return a hash table with all the settings in their original values
   */
  lazy val omssaSettingsInHashTable: Map[String, String] = {
    var settingsMap = new HashMap[String, String]
    searchSettingsReference.filter(element => element.contains("*>")).foreach(e => {
      val kv = e.split("""/\*>""")
      settingsMap.put(kv(0).replace("*>", "").replaceAll("/", ".").replaceFirst(".", ""), kv(1))
    })
    settingsMap.toMap
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
      if (path.matches(".*MSSearchSettings_settingid$") || path.matches(".*MSOutFile_outfile$")) {
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
        if(!element.matches(".*MSInFile_infile.*")) {
	        throw new NotMatchingSearchSettingsException(mainErrorMessage + " : The setting '" + element + "' has at least one different setting")
	        return false
        }
      })
    if (searchSettingsReference.length != searchSettingsCandidate.length) {
      throw new NotMatchingSearchSettingsException(mainErrorMessage + " : The number of settings is different")
      return false
    }
    true
  }

}
