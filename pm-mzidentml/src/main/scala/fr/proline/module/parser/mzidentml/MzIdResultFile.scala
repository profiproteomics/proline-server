package fr.proline.module.parser.mzidentml

import java.io.File
import java.net.URL

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import com.typesafe.scalalogging.LazyLogging

import uk.ac.ebi.jmzidml.model.mzidml.{ Peptide => MzIdPeptide, _ }
import uk.ac.ebi.jmzidml.xml.io.MzIdentMLUnmarshaller

import fr.profi.chemistry.model.Enzyme
import fr.profi.chemistry.model.EnzymeCleavage
import fr.profi.chemistry.model.EnzymeProperties

import fr.profi.obo.PsiMs
import fr.profi.util.ms.MassTolUnit
import fr.profi.util.primitives._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.orm.msi.Scoring

object MzIdResultFile {
  def apply( fileLocationURL: URL,parserContext: ProviderDecoratedExecutionContext) : MzIdResultFile = {
    new MzIdResultFile(new File(fileLocationURL.toURI()) , parserContext)
  }
}

class MzIdResultFile(
  val fileLocation: File,
  val parserContext: ProviderDecoratedExecutionContext
) extends IResultFile with LazyLogging {
  
  private val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
  
  private lazy val mzIdUnmarshaller = new MzIdentMLUnmarshaller(fileLocation)
  private lazy val mzIdAnlSoftList = mzIdUnmarshaller.unmarshal(classOf[AnalysisSoftwareList])
  private lazy val mzIdProvider = mzIdUnmarshaller.unmarshal(classOf[Provider])
  private lazy val mzIdAuditCollection = mzIdUnmarshaller.unmarshal(classOf[AuditCollection])
  private lazy val mzIdAnlSampleCollection = mzIdUnmarshaller.unmarshal(classOf[AnalysisSampleCollection])
  private lazy val mzIdSeqCollection = mzIdUnmarshaller.unmarshal(classOf[SequenceCollection])
  private lazy val mzIdAnlCollection = mzIdUnmarshaller.unmarshal(classOf[AnalysisCollection])
  private lazy val mzIdAnlProtoCollection = mzIdUnmarshaller.unmarshal(classOf[AnalysisProtocolCollection])  
  private lazy val mzIdDataCollection = mzIdUnmarshaller.unmarshal(classOf[DataCollection])  
  private lazy val mzIdAnlData = mzIdDataCollection.getAnalysisData
  private lazy val mzIdSpecIdentList = mzIdAnlData.getSpectrumIdentificationList.get(0)
  private lazy val mzIdInputs = mzIdDataCollection.getInputs()
  
  /** IResultFile values **/
  val importProperties: Map[String, Any] = Map()
  val msLevel: Int = 2
  
  lazy val msiSearch: MSISearch = _parseMSISearch()
  private val msQueryByRef = new HashMap[String,MsQuery]()
    //Cache for value to be keep between load and get methods
  private var _targetResultSetOp : Option[ResultSet] = None
  private var _decoyResultSetOp : Option[ResultSet] = None
  
  // FIXME: this may cause some issues if msQueryByInitialId is accessed while msQueryByRef has not been initiated
  // TODO: load msQueryByRef even if a ResultSet has not been loaded yet
  lazy val msQueries: Array[MsQuery] = {
    
    logger.debug("Start going through MS queries...")
    
    var msQueryCount = 0
    for ( sIdentList <- mzIdAnlData.getSpectrumIdentificationList.asScala ) {
      for ( sIdentResult <- sIdentList.getSpectrumIdentificationResult.asScala ) {
        
        val spectrumID = sIdentResult.getSpectrumID()
        
        for ( sIdentItem <- sIdentResult.getSpectrumIdentificationItem().asScala ) {
          
          // Retrieve or create the MS Query only once
          val msQuery = if( msQueryByRef.contains(spectrumID) ) msQueryByRef(spectrumID)
          else {
            msQueryCount += 1
            
            // Try to parse the query initial id
            val initialId = if( spectrumID matches """query=\d+"""" ) {
              msQueryCount = -1
              spectrumID.split("=").last.toInt
            }
            
            // Else fallback to the msQueryCount
            else if( msQueryCount != -1 ) msQueryCount
            else throw new Exception("inconsistent spectrum IDs in the mzIdentML file")
            
            // Convert CV params into MS query properties
            val sIRCvParams = sIdentResult.getCvParam()
            val specTitle = findCvParamValue(sIRCvParams, PsiMs.SpectrumTitle).getOrElse(spectrumID)
            
            val msQueryPropsTermIds = Set(
              PsiMs.NumberOfPeptideSeqsComparedToEachSpectrum,
              PsiMs.MascotIdentityThreshold,
              PsiMs.MascotHomologyThreshold
            )
            
            val msQueryPropsOpt = if( filterCvParams(sIRCvParams,msQueryPropsTermIds).isEmpty ) None
            else {
              // TODO: parse other properties for other kind of search engines
              val msQueryDbSearchProps = Some( MsQueryDbSearchProperties(
                candidatePeptidesCount = findCvParamValue(sIRCvParams, PsiMs.NumberOfPeptideSeqsComparedToEachSpectrum).map(_.toInt).getOrElse(0),              
                mascotIdentityThreshold = findCvParamValue(sIRCvParams, PsiMs.MascotIdentityThreshold).map(_.toFloat),
                mascotHomologyThreshold = findCvParamValue(sIRCvParams, PsiMs.MascotHomologyThreshold).map(_.toFloat)
              ) )
              
              // FIXME: retrieve specific values for target and decoy searches (if separate searches)
              Some(
                MsQueryProperties(
                  targetDbSearch = msQueryDbSearchProps,
                  decoyDbSearch = msQueryDbSearchProps
                )
              )
            }
            
            val tmpMsQuery = new Ms2Query(
              id = Ms2Query.generateNewId,
              initialId = initialId,
              moz = sIdentItem.getExperimentalMassToCharge(),
              charge = sIdentItem.getChargeState(),
              spectrumTitle = specTitle,
              properties = msQueryPropsOpt
            )
            
            msQueryByRef(spectrumID) = tmpMsQuery
            
            tmpMsQuery
          }
        }
      }
    }
    
    logger.debug(msQueryByRef.size + " MS queries have been created !")
    
    msQueryByRef.values.toArray
  }
  
  private var _hasDecoyResultSet = false
  def hasDecoyResultSet: Boolean = _hasDecoyResultSet // FIXME: is it possible to infer this value ???
  val hasMs2Peaklist: Boolean = true
  
  //var instrumentConfig: Option[InstrumentConfig] = None
  //var peaklistSoftware: Option[PeaklistSoftware] = None
  
  /** Parsing file **/
    
  def close() { // IResultFile method called to release resources
    // TODO: close unmarshaller ???
  }
  
  private lazy val seqDbByRef: Map[String,SeqDatabase] = {
    
    val numSeqSearched = mzIdSpecIdentList.getNumSequencesSearched() 
    
    // Convert SearchDatabases into SeqDatabases
    mzIdInputs.getSearchDatabase().asScala.map { mzIdSearchDb =>
      mzIdSearchDb.getId() -> SeqDatabase(
        id = SeqDatabase.generateNewId,
        name = Option(mzIdSearchDb.getName).getOrElse(mzIdSearchDb.getId),
        filePath = mzIdSearchDb.getLocation(),
        sequencesCount = Option(mzIdSearchDb.getNumDatabaseSequences()).map(_.toInt).getOrElse(0),
        releaseDate = Option(mzIdSearchDb.getReleaseDate()).map(_.getTime()).getOrElse(new java.util.Date),
        version = Option(mzIdSearchDb.getVersion()).getOrElse(""),
        searchedSequencesCount = if (numSeqSearched == null) 0 else numSeqSearched.toInt
      )
    }.toMap
   
  }
  
  // FIXME: how to deal with custom user mods (without unimod id)
  private lazy val allPtmDefs = msiSearch.searchSettings.fixedPtmDefs ++ msiSearch.searchSettings.variablePtmDefs
  private lazy val ptmDefsByUnimodId = allPtmDefs.groupBy( _.unimodId )
  
  private def _parseMSISearch(): MSISearch = {
    // TODO: add this parameter to the getResultSet method instead of using a var field
    require(instrumentConfig.isDefined, "instrumentConfig must be provided first")
    
    logger.debug("Parse MSI Search")
    
    val seqDbs = mzIdInputs.getSearchDatabase().asScala.map( sd => seqDbByRef(sd.getId) ).toArray
    
    /** Parse database filters **/
    val specIdentProto = mzIdAnlProtoCollection.getSpectrumIdentificationProtocol.get(0)      
    val additionalParams = specIdentProto.getAdditionalSearchParams()
    val dbFilters = specIdentProto.getDatabaseFilters()
    
    val taxoNameOpt = Option( dbFilters ).flatMap(
      _.findFilterParamValue( filterId = PsiMs.DBFilterTaxonomy, paramId = PsiMs.TaxonomyScientificName )
    )
    // A specification of how a nucleic acid sequence database was translated for searching.
    //specIdentProto.getDatabaseTranslation()
    //specIdentProto.getSearchType()
    //specIdentProto.getThreshold()
    //specIdentProto.getName()
    
    /*** Parse enzymes ***/
    val mzIdEnzymes = specIdentProto.getEnzymes()
    val areEnzymesIndependant = mzIdEnzymes.isIndependent()
    
    val enzymes = new ArrayBuffer[Enzyme]
    var enzymeNumber = 0
    mzIdEnzymes.getEnzyme().asScala.map { mzidEnzyme =>
      enzymeNumber += 1
      
      val siteRegexpOpt = Option(mzidEnzyme.getSiteRegexp)
      val cleavage = if (siteRegexpOpt.isDefined) {
        
        val SiteRegexExtractor = """.*?([A-Z]+)[^A-Z]*([A-Z]*).*""".r
        val SiteRegexExtractor(residues,restrictiveResidues) = siteRegexpOpt.get
        
        EnzymeCleavage(
          id = EnzymeCleavage.generateNewId(),
          site = "C-term", // FIXME: parse the cleavage site (C-term vs N-term)
          residues = residues,
          restrictiveResidues = if(restrictiveResidues.isEmpty ) None else Some(restrictiveResidues)
        )
      } else {
        EnzymeCleavage(
          id = EnzymeCleavage.generateNewId(),
          site = "C-term", // FIXME: parse the cleavage site (C-term vs N-term)
          residues = "",
          restrictiveResidues =  None 
        )
      }
      
      val enzymeNameOpt = Option(mzidEnzyme.getEnzymeName()).flatMap { mzIdEnzName =>
        val name = if (mzIdEnzName.getCvParam().size() > 0) mzIdEnzName.getCvParam().get(0).getName
        else if (mzIdEnzName.getUserParam().size() > 0) {
          val firstUserParam = mzIdEnzName.getUserParam().get(0)
          Option(firstUserParam.getValue).getOrElse(firstUserParam.getName)
        }
        else null
        
        Option(name)
      }
      
      val enzymeName = enzymeNameOpt
        .orElse(Option(mzidEnzyme.getName))
        .orElse(siteRegexpOpt)
        .getOrElse(s"Enzyme #$enzymeNumber")
      
      logger.info("Used enzyme is: " + enzymeName)
      
      enzymes += Enzyme(
        id = Enzyme.generateNewId(),
        name = enzymeName,
        enzymeCleavages = Array(cleavage),
        cleavageRegexp = Option(mzidEnzyme.getSiteRegexp),
        isIndependant = if(areEnzymesIndependant == null) false else areEnzymesIndependant,
        isSemiSpecific = false,
        properties = Some(
          EnzymeProperties(
            ctermGain = Option( mzidEnzyme.getCTermGain ),
            ntermGain = Option( mzidEnzyme.getNTermGain ),
            //minDistance = Option( mzidEnzyme.getMinDistance ), // FIXME: throws NPE
            maxMissedCleavages = Option( mzidEnzyme.getMissedCleavages )
          )
        )
      )
    }
    
    /*** Parse PTMs ***/
    val fixedPtms = new ArrayBuffer[PtmDefinition]()
    val varPtms = new ArrayBuffer[PtmDefinition]()
    
    for( mzIdSearchMod <- specIdentProto.getModificationParams().getSearchModification().asScala ) {
      
      val firstCvParam = mzIdSearchMod.getCvParam().asScala.head
      val modName = firstCvParam.getName
      val unimodId = firstCvParam.getAccession().split(":").last.toInt
      require( unimodId != 1001460, "can't deal with unknown modifications" )
      //val modId = if( unimodId != 1001460 ) unimodId else (massDelta* 100).toInt
      val resOpt = mzIdSearchMod.getResidues().asScala.headOption
      val residue = if(resOpt.isEmpty || resOpt.get.isEmpty || resOpt.get == "." ) '\u0000' else resOpt.get.charAt(0)
      val modSpecRulesCvParamsOpt = mzIdSearchMod.getSpecificityRules().asScala.headOption.map(_.getCvParam())
      
      // Parse PTM location
      // FIXME: enable me when ABRF study is over (files converted from PepXML contain weird specificities)
      //val location = PtmLocation.ANYWHERE
      val location = if (modSpecRulesCvParamsOpt.isEmpty) PtmLocation.ANYWHERE
      else {
        val cvParams = modSpecRulesCvParamsOpt.get
        
        if( findCvParam(cvParams, PsiMs.ModificationSpecificityPeptideNterm).isDefined ) PtmLocation.ANY_N_TERM
        else if( findCvParam(cvParams, PsiMs.ModificationSpecificityPeptideCterm).isDefined ) PtmLocation.ANY_C_TERM
        else if( findCvParam(cvParams, PsiMs.ModificationSpecificityProteinNterm).isDefined ) PtmLocation.PROT_N_TERM
        else if( findCvParam(cvParams, PsiMs.ModificationSpecificityProteinCterm).isDefined ) PtmLocation.PROT_C_TERM
        else PtmLocation.ANYWHERE
      }
      
      val ptmDefOpt = ptmProvider.getPtmDefinition(modName, residue, location)
      val ptmDef = if( ptmDefOpt.isDefined ) ptmDefOpt.get
      else {
        logger.warn(s"can't retrieve PTM definition for modification named '$modName'")
        
        val massDelta = mzIdSearchMod.getMassDelta()
        val precursorEvidence = PtmEvidence(
          ionType = IonTypes.Precursor,
          composition = "",
          monoMass = massDelta.toDouble,
          averageMass = massDelta.toDouble
        )
        
        PtmDefinition(
          id = PtmDefinition.generateNewId(),
          location = location.toString,
          names = PtmNames(shortName = modName, fullName = modName),
          ptmEvidences = Array(precursorEvidence),
          residue = residue,
          unimodId = unimodId
        )
      }
      
      if( mzIdSearchMod.isFixedMod() ){
        fixedPtms += ptmDef
      } else {
        varPtms += ptmDef
      }
    }
    
    val analysisSoft = if( specIdentProto.getAnalysisSoftware() != null )
      specIdentProto.getAnalysisSoftware
    else
      mzIdAnlSoftList.getAnalysisSoftware.asScala.head
    
    val additionalUserParams = additionalParams.getUserParam()
    val searchSettings = if( findUserParam(additionalUserParams,"output_pepxmlfile").isDefined ) {
      
      logger.debug("Create SearchSettings using 'output_pepxmlfile' section")
      
      val pepXmlParams = new PepXmlUserParams(additionalUserParams)
      val ms1ErrorTol = pepXmlParams.peptideMassTolerance
      val ms1ErrorTolUnit = MassTolUnit.PPM.toString
      
      val msmsSearchSettingsOpt = Some(
        MSMSSearchSettings(
          // TODO: parse ms2ChargeStates
          ms2ChargeStates = "",
          ms2ErrorTol = pepXmlParams.fragmentBinTol,
          ms2ErrorTolUnit = MassTolUnit.Da.toString
        )
      )
      
      SearchSettings(
        id = SearchSettings.generateNewId(),
        softwareName = analysisSoft.getName(),
        softwareVersion = analysisSoft.getVersion(),
        taxonomy = taxoNameOpt.getOrElse(""),
        maxMissedCleavages = pepXmlParams.allowedMissedCleavage,
        // TODO: parse ms1ChargeStates
        ms1ChargeStates = "",
        ms1ErrorTol = ms1ErrorTol,
        ms1ErrorTolUnit = ms1ErrorTolUnit,
        isDecoy = pepXmlParams.targetDecoyApproachOpt.getOrElse(false), // TODO: can we infer this information ?
        usedEnzymes = enzymes.groupBy(_.name).map(_._2.head).toArray,
        variablePtmDefs = varPtms.toArray,
        fixedPtmDefs = fixedPtms.toArray,
        seqDatabases = seqDbs,
        instrumentConfig = instrumentConfig.getOrElse(null),
        fragmentationRuleSet = this.fragmentationRuleSet

      )
      
    } else {
      logger.debug("Create SearchSettings")
      
      val parentTolParam = specIdentProto.getParentTolerance().getCvParam().get(0)
      val parentTol = parentTolParam.getValue().toDouble
      val parentTolUnit = mzIdTolToProfiTol(parentTolParam.getUnitName)

      val fragmentTolParamOpt = if (specIdentProto.getFragmentTolerance() == null) None
      else specIdentProto.getFragmentTolerance().getCvParam().asScala.headOption
      
      val msmsSearchSettingsOpt = fragmentTolParamOpt.map { fragmentTolParam =>
        MSMSSearchSettings(
          // TODO: parse ms2ChargeStates (they may be stored in AdditionalSearchParams)
          ms2ChargeStates = "",
          ms2ErrorTol = fragmentTolParam.getValue().toDouble,
          ms2ErrorTolUnit = mzIdTolToProfiTol(fragmentTolParam.getUnitName)
        )
      }
      
      val maxMC = enzymes.flatMap(_.properties.get.maxMissedCleavages ).headOption.getOrElse(0)
      
      SearchSettings(
        id = SearchSettings.generateNewId(),
        softwareName = analysisSoft.getName(),
        softwareVersion = analysisSoft.getVersion(),
        taxonomy = taxoNameOpt.getOrElse(""),
        maxMissedCleavages = maxMC,
        // TODO: parse ms1ChargeStates (they may be stored in AdditionalSearchParams)
        ms1ChargeStates = "",
        ms1ErrorTol = parentTol,
        ms1ErrorTolUnit = parentTolUnit,
        isDecoy = false, // TODO: can we infer this information ?
        usedEnzymes = enzymes.groupBy(_.name).map(_._2.head).toArray,
        variablePtmDefs = varPtms.toArray,
        fixedPtmDefs = fixedPtms.toArray,
        seqDatabases = seqDbs,
        instrumentConfig = instrumentConfig.getOrElse(null),
        fragmentationRuleSet = this.fragmentationRuleSet
      )
    }
    
    /*val protIdentProto = apc.getProteinDetectionProtocol
    protIdentProto.getAnalysisParams()
    if( protIdentProto.getAnalysisSoftware() != null )
      println( protIdentProto.getAnalysisSoftware().getSoftwareName() )
    protIdentProto.getThreshold()*/
    
    // FIXME: how to handle multiple source files ???
    /*val mzIdSourceFile = mzIdInputs.getSourceFile().get(0)
    val sourceFilePath = new java.net.URI( mzIdSourceFile.getLocation() ).getPath()
    val resultFileLocation = new java.io.File( sourceFilePath )*/
    
    val mzIdSpectraData = mzIdInputs.getSpectraData().get(0)
    val rawFileIdentifierOpt = Option(mzIdSpectraData.getName).orElse {
      Option(mzIdSpectraData.getLocation()).map { location =>
        new java.io.File(location).getName
      }
    }.map(_.split("\\.").head)
    
    val peaklist = Peaklist(
      id = Peaklist.generateNewId,
      fileType = Option(mzIdSpectraData.getFileFormat).map(_.getCvParam().getValue()).getOrElse(""),
      path = mzIdSpectraData.getLocation(),
      rawFileIdentifier = rawFileIdentifierOpt.getOrElse(""),
      msLevel = 2 // TODO: parse from SearchType
    )
    
    val queriesCount = mzIdSpecIdentList.getSpectrumIdentificationResult().size()
    
    logger.debug("MSI search created !")
    
    MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = fileLocation.getName(),
      searchSettings = searchSettings,
      peakList = peaklist,
      title = "",
      date = new java.util.Date(), // FIXME: retrieve the right date
      resultFileDirectory = fileLocation.getParentFile().getAbsolutePath(),
      queriesCount = queriesCount,
      jobNumber = 0,
      userName = "",
      userEmail = "",
      searchedSequencesCount = seqDbs.map(_.searchedSequencesCount).sum
    )
    
  }
  
  private lazy val peptideByRef: Map[String,Peptide] = {
    
    logger.debug("Get peptide by reference")
    
    val peptideByKey = new HashMap[String,Peptide]()
    
    mzIdSeqCollection.getPeptide().asScala.map { mzIdPep =>
      val pepId = Peptide.generateNewId()
      val newPep = _mzIdPepToPeptide(pepId,mzIdPep)
      val pepKey = newPep.uniqueKey
      val existingPepOpt = peptideByKey.get(newPep.uniqueKey)
      
      val pep = if( existingPepOpt.isDefined ) {
        existingPepOpt.get
      } else {
        peptideByKey += pepKey -> newPep
        newPep
      }

      mzIdPep.getId() -> pep
      
    }.toMap
    
    /*for( (k,peps) <- peptides.groupBy(_.uniqueKey) ) {
      if( peps.length > 1 ) {
        println(k)
        for( p<- peps) println(p)
      }
    }*/
  }
  
  private def _mzIdPepToPeptide(
    pepId: Long,
    mzIdPeptide: MzIdPeptide  
  ): Peptide = {
    
    val pepSeq = mzIdPeptide.getPeptideSequence()
    val seqLen = pepSeq.length
    
    // TODO: check that the MzIdPeptide contains fixed PTMs
    val locatedPtms = new ArrayBuffer[LocatedPtm](0)
    
    for ( mzIdMod <- mzIdPeptide.getModification().asScala ) {
      
      //println(pepSeq)
      //println( mzIdMod.getResidues() )
      
      // Retrieve PTM definition
      val unimodSplitted = mzIdMod.getCvParam().asScala.head.getAccession().split(":")
      var unimodId = -1
      try{
        if(unimodSplitted.length>1)
          unimodId = unimodSplitted.last.toInt
      } catch {
          case nbe : NumberFormatException => unimodId = -1
      }

      //val unimodId = mzIdMod.getCvParam().head.getAccession().split(":").last.toInt
      require( unimodId != -1, "can't deal with undefined modifications (No unimod id)" )
      require( unimodId != 1001460, "can't deal with unknown modifications" )
      
      //val modId = if( unimodId != 1001460 ) unimodId else (mzIdMod.getMonoisotopicMassDelta() * 100).toInt
      val putativePtmDefsOpt = ptmDefsByUnimodId.get(unimodId)
      require( putativePtmDefsOpt.nonEmpty, "can't find unimod PTM with id = " + unimodId )
      
      val putativePtmDefs = putativePtmDefsOpt.get
      val mzIdSeqPos = mzIdMod.getLocation().toInt
      // Set seqPos to -1 if C-term, else set it to mzIdSeqPos
      val seqPos = if (mzIdSeqPos == seqLen + 1) -1 else mzIdSeqPos
      
      val ptmDef = if (putativePtmDefs.length == 1) putativePtmDefs.head
      else {
        val resOpt = mzIdMod.getResidues().asScala.headOption
  
        val residue = if (seqPos == 0 || seqPos == -1) '\u0000' // No residue for N-term/C-term mods
        else if (resOpt.isEmpty) pepSeq.charAt(mzIdSeqPos - 1) // Infer residue from sequence
        else if (resOpt.get.isEmpty || resOpt.get == ".") '\u0000' // No residue if empty string or "."
        else resOpt.get.charAt(0) // Extract residue character from string
        
        val ptmDefs = putativePtmDefs.filter(_.residue == residue)
        val ptmDefsCount = ptmDefs.length
        require( ptmDefsCount < 2, s"multiple PTMs found for for unimodId=$unimodId residue=$residue location=$mzIdSeqPos")
        require( ptmDefsCount == 1, s"can't find PTM for unimodId=$unimodId residue=$residue location=$mzIdSeqPos" )
        
        ptmDefs.head
      }
      
      /*println(pepSeq)
      println(unimodId)
      println(mzIdSeqPos)
      println(seqPos)
      println(ptmDef)*/
      
      locatedPtms += LocatedPtm(
        ptmDef = ptmDef,
        seqPos = seqPos
      )
      
    }

    new Peptide(
      id = pepId,
      sequence = pepSeq,
      ptms = locatedPtms.toArray
    )
  }
  
  private def mzIdTolToProfiTol(mzIdTol: String): String = {
    mzIdTol.toLowerCase() match {
      case "dalton" => MassTolUnit.Da.toString
      case "ppm" => MassTolUnit.PPM.toString()
      case "parts per million" => MassTolUnit.PPM.toString()
      // TODO: add percent unit to the MassTolUnit enum
      case "percent" => "percent"
        //TODO  : use specified mzIdTol instead of error ?! 
      case _ => throw new Exception("unhandled tolerance unit: "+ mzIdTol)
    }
  }
  
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
  
  def _loadResultSet(wantDecoy: Boolean) {
    logger.debug("Load the data into a ResultSet...")
    
    // Important: initialize msQueries !
    val msq = this.msQueries
    
    logger.debug("Number of MS queries: "+msq.length)
    
    case class MzIdSequenceMatch( id: String, dbSeqRef: String, sequenceMatch: SequenceMatch )
    
    val newRsId = ResultSet.generateNewId()
    
    val mzIdSeqMatchById = new HashMap[String,MzIdSequenceMatch]()
    val mzIdSeqMatchesByDbSeqRef = new HashMap[String,ArrayBuffer[MzIdSequenceMatch]]()
   
    // Get the list of PeptideEvidence elements matching the wantDecoy provided parameter
    var seqMatchesCount = 0
    for(mzIdPepEvidence <- mzIdSeqCollection.getPeptideEvidence().asScala) {
      val isDecoy = mzIdPepEvidence.isIsDecoy()
      if (!this._hasDecoyResultSet && isDecoy) this._hasDecoyResultSet = true
      
      if (wantDecoy == isDecoy) {
      
        val peptide = peptideByRef.get(mzIdPepEvidence.getPeptideRef())
        val (start,end) = if( mzIdPepEvidence.getStart() != null ) {
          (mzIdPepEvidence.getStart().toInt,mzIdPepEvidence.getEnd().toInt)
        } else {
          ( 1, peptide.get.sequence.length )
        }
        
        val seqMatch = new SequenceMatch(
          start = start,
          end = end,
          residueBefore = mzIdPepEvidence.getPre().charAt(0),
          residueAfter = mzIdPepEvidence.getPost().charAt(0),
          isDecoy = isDecoy,
          peptide = peptide,
          bestPeptideMatch = None, // set below in a separate loop
          resultSetId = newRsId
        )
        
        seqMatchesCount += 1
        
        // TODO: import the following values
        // TODO: add frame to SequenceMatch or ProteinMatch OM ???
        /*      
        mzIdPepEvidence.getDBSequenceRef()
        mzIdPepEvidence.getFrame()
        mzIdPepEvidence.getTranslationTableRef()
        */
        
        val mzIdSeqMatch = MzIdSequenceMatch( mzIdPepEvidence.getId(), mzIdPepEvidence.getDBSequenceRef, seqMatch)
        
        mzIdSeqMatchById += mzIdSeqMatch.id -> mzIdSeqMatch
        mzIdSeqMatchesByDbSeqRef.getOrElseUpdate(mzIdSeqMatch.dbSeqRef, new ArrayBuffer[MzIdSequenceMatch]) += mzIdSeqMatch
      }
    }
    
    logger.debug(s"$seqMatchesCount sequence matches have been created !")
    
    val pepMatchesByMzIdSeqMatchId = new HashMap[String,ArrayBuffer[PeptideMatch]]()
    val pepMatches = new ArrayBuffer[PeptideMatch]()

    // Get the list of SpectrumIdentification elements
    // TODO: how to handle multiple SpectrumIdentifications
    for ( sIdentList <- mzIdAnlData.getSpectrumIdentificationList.asScala ) {
      for ( sIdentResult <- sIdentList.getSpectrumIdentificationResult.asScala ) {
        
        val spectrumID = sIdentResult.getSpectrumID()
        
        for ( sIdentItem <- sIdentResult.getSpectrumIdentificationItem().asScala ) {
          
          val msQuery = msQueryByRef(spectrumID)
          
          // Retrieve the corresponding sequence matches
          val pepMatchMzIdSeqMatches = sIdentItem.getPeptideEvidenceRef().asScala
            .withFilter( ref => mzIdSeqMatchById.contains(ref.getPeptideEvidenceRef) )
            .map( ref => mzIdSeqMatchById(ref.getPeptideEvidenceRef) )
          
          if (pepMatchMzIdSeqMatches.nonEmpty) {
            
            // FIXME: handle PMF data (null peptideRef ???)
            val peptide = peptideByRef(sIdentItem.getPeptideRef())
            val deltaMoz = sIdentItem.getExperimentalMassToCharge() - sIdentItem.getCalculatedMassToCharge()
            
            val firstSeqMatch = pepMatchMzIdSeqMatches.head.sequenceMatch
            val missedCleavages = PeptideMatch.countMissedCleavages(
              peptide.sequence,
              Some(firstSeqMatch.residueBefore),
              Some(firstSeqMatch.residueAfter),
              Array(msiSearch.searchSettings.usedEnzymes.head) // FIXME: handle multiple enzymes
            )
            
            // Get some CV params for this PSM
            val sIDCvParams = sIdentItem.getCvParam()
            
            // Try to extract percolator score
            // TODO: parse other percolator values and all score information (put them in serialized properties)
            val percolatorPepOpt = findCvParamValue(sIDCvParams, PsiMs.PercolatorPEP)

            val( pepMatchScore, scoreType ) = if (percolatorPepOpt.isDefined) {
              (- 10 * math.log10(percolatorPepOpt.get.toDouble), Scoring.Type.PERCOLATOR_PEP_LOG_SCALED.toString)
            } else {
              
              // Extract the score value for a given search engine
              val sIDParamGroup = sIdentItem.getParamGroup()
              
              val scoreParamOpt = ScoreParamName.values.flatMap( termId => findParam(sIDParamGroup, termId.toString) ).headOption
              require( scoreParamOpt.isDefined, s"can't find a score value in this spectrum identification item (id=${sIdentItem.getId})")
              
              val scoreParam = scoreParamOpt.get
              val scoreParamValue = scoreParam.getValue.toDouble
              
              ScoreParamName.withName(scoreParam.getName) match {
                case ScoreParamName.COMET_EVALUE => {
                  (-math.log10(scoreParamValue),Scoring.Type.COMET_EVALUE_LOG_SCALED.toString)
                }
                case ScoreParamName.MASCOT_SCORE => {
                  (scoreParamValue,Scoring.Type.MASCOT_IONS_SCORE.toString)
                }
                case ScoreParamName.MSGF_EVALUE => {
                  (-math.log10(scoreParamValue),Scoring.Type.MSGF_EVALUE_LOG_SCALED.toString)
                }
                case ScoreParamName.OMSSA_EVALUE => {
                  (-math.log10(scoreParamValue),Scoring.Type.OMSSA_EVALUE.toString)
                }
                case ScoreParamName.PEPTIDE_SHAKER_SCORE => {
                  (scoreParamValue,Scoring.Type.PEPTIDE_SHAKER_PSM_SCORE.toString)
                }
                case ScoreParamName.SEQUEST_EXPECT => {
                  (-math.log10(scoreParamValue),Scoring.Type.SEQUEST_EXPECT_LOG_SCALED.toString)
                }
                case ScoreParamName.XTANDEM_EVALUE => {
                  (-math.log10(scoreParamValue),Scoring.Type.XTANDEM_EXPECT_LOG_SCALED.toString)
                }
              }
            }
            
            // --- Convert CV params into peptide match properties ---
            val matchedPeaksCount = findCvParamValue(sIDCvParams, PsiMs.NumberOfMatchedPeaks).map(_.toInt).getOrElse {
              // TODO: check that we do not count the same peak multiple times
              sIdentItem.getFragmentation.getIonType.asScala.map( _.getFragmentArray.asScala.head.getValues.asScala.length ).sum
            }
            
            // TODO: store following CvParams and UserParams in object trees
            /*
            <cvParam accession="MS:1001172" name="Mascot:expectation value" cvRef="PSI-MS"
                value="22.9206170903222"/>

            <cvParam cvRef="MS" accession="MS:1001362" name="number of unmatched peaks" value="13"/>
            <cvParam cvRef="MS" accession="MS:1001329" name="OMSSA:pvalue" value="0.012255454611558"/>
            <cvParam cvRef="MS" accession="MS:1001328" name="OMSSA:evalue" value="145.116838055463319"/>

            <userParam name="xcorr" value="0.496" type="xsd:float"/>
            <userParam name="deltacn" value="0.000" type="xsd:float"/>
            <userParam name="deltacnstar" value="0.000" type="xsd:float"/>
            <userParam name="spscore" value="3.9" type="xsd:float"/>
            <userParam name="sprank" value="7" type="xsd:float"/>
            <userParam name="expect" value="7.30E+00" type="xsd:float"/>

            <cvParam cvRef="MS" accession="MS:1002049" name="MS-GF:RawScore" value="74"/>
            <cvParam cvRef="MS" accession="MS:1002050" name="MS-GF:DeNovoScore" value="92"/>
            <cvParam cvRef="MS" accession="MS:1002052" name="MS-GF:SpecEValue" value="9.772923E-18"/>
            <cvParam cvRef="MS" accession="MS:1002053" name="MS-GF:EValue" value="5.5625553E-11"/>
            <userParam name="IsotopeError" value="2" type="xsd:float"/>
            */
            
            val mascotEvalueOpt = findCvParamValue(sIDCvParams, PsiMs.MascotExpectationValue)
            val mascotPropsOpt = mascotEvalueOpt.map { eValue => 
              PeptideMatchMascotProperties( expectationValue = eValue.toDouble )
            }

            val omssaPValueOpt = findCvParamValue(sIDCvParams, PsiMs.OMSSAPvalue).map(_.toDouble)
            val omssaPropsOpt = omssaPValueOpt.map { pValue => 
              PeptideMatchOmssaProperties(
                pValue = pValue.toDouble,
                correctedCharge = msQuery.charge // FIXME: compute the corrected charge as in OMSSA parser ???
              )
            }
            
            // Search for PhosphoRS information
            val phosphoRsScoreCvParams = filterCvParams(sIDCvParams,Set(PsiMs.PhosphoRSScore))
            val phosphoRsScoresAsStr = phosphoRsScoreCvParams.map(_.getValue).mkString(";")
            val ptmSitePropsOpt = Some(PeptideMatchPtmSiteProperties(phosphoRsString = Some(phosphoRsScoresAsStr)))
            
            // TODO: add properties for other search engines or use object trees ???
            val pepMatchProps = PeptideMatchProperties(
              mascotProperties = mascotPropsOpt,
              omssaProperties = omssaPropsOpt,
              ptmSiteProperties = ptmSitePropsOpt
            )
            
            // Convert the SpectrumIdentificationItem element into a PeptideMatch
            val pepMatch = new PeptideMatch(
              id = PeptideMatch.generateNewId,
              rank = sIdentItem.getRank(),
              score = pepMatchScore.toFloat,
              scoreType = PeptideMatchScoreType.withName(scoreType),
              charge = msQuery.charge,
              deltaMoz = deltaMoz.toFloat, // exp - calc
              isDecoy = wantDecoy,
              isValidated = sIdentItem.isPassThreshold(),
              peptide = peptide,
              missedCleavage = missedCleavages,
              // TODO: check this is the value we want to set here
              fragmentMatchesCount = matchedPeaksCount,
              msQuery = msQuery,
              properties = Some(pepMatchProps),
              resultSetId = newRsId
            )
            
            pepMatches += pepMatch
            
            for( mzIdSeqMatch <- pepMatchMzIdSeqMatches ) {
              pepMatchesByMzIdSeqMatchId.getOrElseUpdate(mzIdSeqMatch.id, new ArrayBuffer[PeptideMatch] ) += pepMatch
            }
          }
          
          // TODO: import the following values
          /*
          sIdentItem.getCalculatedPI()
          sIdentItem.getFragmentation()
          sIdentItem.getSampleRef()
          sIdentItem.getCvParam()
          sIdentItem.getUserParam()
          */
          
        } // end of spectrum identification items
      } // end of spectrum identification results
    } // end of spectrum identification list
    
    logger.debug(pepMatches.length + " peptide matches have been created !")
    
    // Update the bestPeptideMatch attribute of each SequenceMatch
    for( (mzIdSeqMatchId,pepMatches) <- pepMatchesByMzIdSeqMatchId ) {
      val mzIdSeqMatch = mzIdSeqMatchById(mzIdSeqMatchId)
      mzIdSeqMatch.sequenceMatch.bestPeptideMatch = Some( pepMatches.maxBy(_.score) )
    }
    
    // Convert DBSequence elements into ProteinMatch entities
    val protMatches = mzIdSeqCollection.getDBSequence().asScala.withFilter(s => mzIdSeqMatchesByDbSeqRef.contains(s.getId)).map { mzIdDbSeq =>
      
      val mzIdSeqMatches = mzIdSeqMatchesByDbSeqRef(mzIdDbSeq.getId)
      val pepMatchesCount = mzIdSeqMatches.flatMap( s => pepMatchesByMzIdSeqMatchId(s.id) ).distinct.length
      
      val proteinOpt = Option( mzIdDbSeq.getSeq() ).map( new Protein( _ ) )
      val seqDb = seqDbByRef(mzIdDbSeq.getSearchDatabaseRef())
      
      val distinctMzIdSeqMatches = mzIdSeqMatches.groupBy { mzSm =>
        val seqMatch = mzSm.sequenceMatch
        s"${seqMatch.peptide.get.id}%${seqMatch.start}%${seqMatch.end}"
      }.map { _._2.head }.toArray
      
      val dbSeqCvParams = mzIdDbSeq.getCvParam()
      val protMatchDesc = findCvParamValue(dbSeqCvParams, PsiMs.ProteinDescription).getOrElse("")
      val protMatchTaxonId = findCvParamValue(dbSeqCvParams, PsiMs.TaxonomyNCBITaxID).map(_.toInt).getOrElse(0)
      
      ProteinMatch(
        id = ProteinMatch.generateNewId,
        accession = mzIdDbSeq.getAccession(),
        description = protMatchDesc,
        sequenceMatches = distinctMzIdSeqMatches.map(_.sequenceMatch),
        peptideMatchesCount = pepMatchesCount,
        isDecoy = wantDecoy, // TODO: can we compute this value ???
        protein = proteinOpt,
        taxonId = protMatchTaxonId,
        // FIXME: remove the scoreType when the MSIdb ALLOWS NULL for this field
        scoreType = Scoring.Type.MASCOT_STANDARD_SCORE.toString,
        seqDatabaseIds = Array(seqDb.id),
        resultSetId = newRsId
      )
    }.toArray
     
    logger.debug(protMatches.length + " protein matches have been created !")
    
    val loadedRS = ResultSet(
      id = newRsId,
      name = msiSearch.title,
      peptides = peptideByRef.values.toArray.distinct,
      peptideMatches = pepMatches.toArray,
      proteinMatches = protMatches,
      isDecoy = wantDecoy,
      isSearchResult = true,
      isValidatedContent = false, // FIXME: it may not be always the case ?
      msiSearch = Some(msiSearch),
      properties = None
    )
    
    if(wantDecoy)
      _decoyResultSetOp = Some(loadedRS)
    else
      _targetResultSetOp = Some(loadedRS)
  }
  
  def eachSpectrum( onEachSpectrum: Spectrum => Unit ): Unit = {
    
    for ( msQuery <- msQueries.sortBy(_.initialId) ) {
      
      msQuery match {
        case ms1Query: Ms1Query => {}
        case ms2Query: Ms2Query => {
          
          // Retrieve spectrum title and instrument config id
          val spectrumTitle = ms2Query.spectrumTitle
          val fragmentationRuleSetIdOpt = if (fragmentationRuleSet.isDefined) Some(fragmentationRuleSet.get.id) else None
      
          val specTitleFieldMapOpt = if (peaklistSoftware.isEmpty) None
          else peaklistSoftware.get.specTitleParsingRule.map(_.parseTitle(spectrumTitle))
          
          val specTitleFieldMap = specTitleFieldMapOpt.getOrElse(Map.empty[SpectrumTitleFields.Value, String])
      
          // TODO: put in primitives utils
          def toIntOrZero(v: Any): Int = try { toInt(v) } catch { case e: Throwable => 0 }
          def toFloatOrZero(v: Any): Float = try { toFloat(v) } catch { case e: Throwable => 0f }
      
          val titleFields = SpectrumTitleFields
      
          val spec = new Spectrum(
            id = Spectrum.generateNewId,
            title = spectrumTitle,
            precursorMoz = ms2Query.moz,
            precursorCharge = ms2Query.charge,
            firstCycle = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_CYCLE, 0)),
            lastCycle = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_CYCLE, 0)),
            firstScan = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_SCAN, 0)),
            lastScan = toIntOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_SCAN, 0)),
            firstTime = toFloatOrZero(specTitleFieldMap.getOrElse(titleFields.FIRST_TIME, 0f)),
            lastTime = toFloatOrZero(specTitleFieldMap.getOrElse(titleFields.LAST_TIME, 0f)),
            // TODO: add peaks if available or infer them from fragment matches
            mozList = None,
            intensityList = None,
            peaksCount = 0,
            fragmentationRuleSetId = fragmentationRuleSetIdOpt,
            peaklistId = msiSearch.peakList.id
          )
          
          onEachSpectrum(spec)
        }
      }
    }
    
  }
  
  def eachSpectrumMatch( wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit ): Unit = {
    
  }
  
}