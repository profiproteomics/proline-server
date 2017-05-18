package fr.proline.module.parser.xtandem

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.xml.sax.helpers.DefaultHandler
import com.typesafe.scalalogging.LazyLogging
import fr.profi.chemistry.model.MolecularConstants
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.model.msi.IonTypes
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.model.msi.MsQuery
import fr.proline.core.om.model.msi.Peptide
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.PeptideMatchProperties
import fr.proline.core.om.model.msi.PeptideMatchScoreType
import fr.proline.core.om.model.msi.PeptideMatchXtandemProperties
import fr.proline.core.om.model.msi.Protein
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.SequenceMatch
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msi.SpectrumProperties
import fr.proline.core.om.model.msi.SpectrumTitleFields
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ProteinEmptyFakeProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import javax.xml.parsers.SAXParser
import javax.xml.parsers.SAXParserFactory

class XtandemParser(val xtandemFile: File, val parserContext: ProviderDecoratedExecutionContext, val importProperties: Map[String, Any]) extends DefaultHandler with IResultFile with LazyLogging {

  // append user properties if any
  private val properties = new HashMap[String, Any]
  if(importProperties != null) properties ++= importProperties
  // and add default values
  val defaultProteinParsingRule = "^([^ ]+).*" // default parsing rule is 'extract all until first space'
  if(!properties.isDefinedAt("protein.parsing.rule")) {
    logger.info("Using default parsing rule: "+defaultProteinParsingRule)
    properties.put("protein.parsing.rule", defaultProteinParsingRule)
  } else {
    logger.info("Using given parsing rule: "+properties.get("protein.parsing.rule").toString)
  }
  
  // private values
  private val msQueriesList = new ArrayBuffer[MsQuery]
  private val spectrumList = new ArrayBuffer[Spectrum] // will only be used in the eachSpectrum method
    //Cache for value to be keep between load and get methods
  private var _targetResultSetOp : Option[ResultSet] = None
  private var _decoyResultSetOp : Option[ResultSet] = None

  // public values
  val fileLocation: File = xtandemFile
  val msLevel: Int = 2
  def msQueries = msQueriesList.toArray
  
  // set Providers
  private lazy val pepProvider = parserContext.getProvider(classOf[IPeptideProvider])
  private lazy val protProvider = if (parserContext.getProvider(classOf[IProteinProvider]) == null ||
    parserContext.getProvider(classOf[IProteinProvider]).isInstanceOf[ProteinEmptyFakeProvider]) {
    ProteinFakeProvider //Fake provider should at least create Fake Proteins, not None
  } else {
    parserContext.getProvider(classOf[IProteinProvider])
  }
  
  private var resultBioml: XTBioml = null
  private lazy val xtandemSettings = new XtandemSettings(resultBioml, parserContext, instrumentConfig, peaklistSoftware)
  lazy val msiSearch: MSISearch = xtandemSettings.msiSearchOpt.get
//  lazy val ionSeries = xtandemSettings.ionSeries
  lazy val hasDecoyResultSet = xtandemSettings.hasDecoyResultSet
  lazy val hasMs2Peaklist = xtandemSettings.hasMs2Peaklist
  
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
    
    // parse the xml file
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var saxParser: SAXParser = factory.newSAXParser()
    var manager = new XtandemHandler()
    var startTime: Long = System.currentTimeMillis()
    saxParser.parse(fileLocation, manager)
    logger.info("Xtandem file read in " + (System.currentTimeMillis() - startTime) + " milliseconds")
    resultBioml = manager.bioml

    val peptideByUniqueKey = new HashMap[String, Peptide]
    val peptideMatchesByUniqueKey = new HashMap[String, ArrayBuffer[PeptideMatch]]
    val proteinMatchesPerUniqueKey = new HashMap[Tuple2[String, Long], ProteinMatch]
    val sequenceMatchesPerUniqueKey = new HashMap[Tuple2[String, Long], ArrayBuffer[SequenceMatch]]
    
    // get parsing rules if any
    val parsingRules = if(this.peaklistSoftware.isDefined) this.peaklistSoftware.get.specTitleParsingRule else None
    
    // get search settings
    val rsProps = xtandemSettings.resultSetProperties

    // for each GroupModel (spectrum)
    resultBioml.groupModelList.foreach(groupItem => {
      groupItem.groupSupportList.foreach(spectrumItem => {

        // get title, there should be only one match
        var spectrumTitle = spectrumItem.note.info
        // xtandem adds RTINSECONDS to the title if it finds it in the spectrum header
        if(spectrumTitle.contains("RTINSECONDS=")) {
          spectrumTitle = spectrumTitle.substring(0, spectrumTitle.indexOf("RTINSECONDS="));
        }
        // instantiate query
        // problem with xtandem: not all queries are written in the output file :(
        val currentQuery = new Ms2Query(
            id = Ms2Query.generateNewId(),
            initialId = groupItem.id,
            moz = (groupItem.mh + (groupItem.z-1) * MolecularConstants.PROTON_MASS) / groupItem.z,
            charge = groupItem.z,
            spectrumTitle = spectrumTitle)
        msQueriesList += currentQuery
        
        // keep spectrum too
        spectrumItem.gamlTraceList.foreach(trace => { // should be only one item ?
          val mozList = trace.gamlXdata.gamlValues.info.split(" ").map(_.toDouble)
          val intensityList = trace.gamlYdata.gamlValues.info.split(" ").map(_.toFloat)
          val parsedTitle = parsingRules.map(_.parseTitle(spectrumTitle)).getOrElse(Map.empty[SpectrumTitleFields.Value, String])
          val spectrumPropOp =  {
            val spectrumProp = new SpectrumProperties()
            spectrumProp.rtInSeconds = Some(groupItem.rt.toFloat)   
            Some(spectrumProp)
          } 
        
          spectrumList += new Spectrum(
              id = Spectrum.generateNewId(),
              title = currentQuery.spectrumTitle,
              precursorMoz = currentQuery.moz, 
              precursorCharge = currentQuery.charge,
              firstCycle = parsedTitle.getOrElse(SpectrumTitleFields.FIRST_CYCLE, "0").toInt,
              lastCycle = parsedTitle.getOrElse(SpectrumTitleFields.LAST_CYCLE, "0").toInt,
              firstScan = parsedTitle.getOrElse(SpectrumTitleFields.FIRST_SCAN, "0").toInt,
              lastScan = parsedTitle.getOrElse(SpectrumTitleFields.LAST_SCAN, "0").toInt,
              firstTime = parsedTitle.getOrElse(SpectrumTitleFields.FIRST_TIME, (groupItem.rt/60).toString).toFloat,
              lastTime = parsedTitle.getOrElse(SpectrumTitleFields.LAST_TIME, (groupItem.rt/60).toString).toFloat,
              mozList = Some(mozList), 
              intensityList = Some(intensityList),
              peaksCount = mozList.length,
              instrumentConfigId = if (instrumentConfig.isDefined) instrumentConfig.get.id else 0,
              peaklistId = msiSearch.peakList.id,
              properties = spectrumPropOp)
        })
        
        // for each protein
        spectrumItem.proteinList.foreach(proteinItem => {
          // fix long accession names
          val userPattern = properties.get("protein.parsing.rule").get.toString
          val accession = proteinItem.note.info match {
            case userPattern.r(accession) => accession // user pattern, may not work
            case defaultProteinParsingRule.r(accession) => accession // default pattern: extract string until first space character
            case _ => proteinItem.note.info // keep description if everything else failed (or maybe accession ?)
          }
          // from this point, use accession instead of proteinItem.label
          // for each Domain (peptide)
          proteinItem.peptide.domainList.foreach(peptideItem => {
            // when "include reverse" parameter is set to yes, tag ":reversed" is added to the protein description
            val isDecoy = proteinItem.note.info.endsWith(":reversed")
            if(isDecoy == wantDecoy) {
            
              // define uniqueKey
              val key = peptideItem.seq+"%"+peptideItem.aaMarkupList.map(aa => aa.typeMU.toString+(aa.at-peptideItem.start+1)).mkString("")
              // get or create peptide
              val peptide = peptideByUniqueKey.getOrElse(key, {
                val locatedPtms = getLocatedPtms(peptideItem)
                val peptideFromDatabase = {
                  val tmpPeptide = {
                    try { pepProvider.getPeptide(peptideItem.seq, locatedPtms) }
                    catch { case e: javax.persistence.NonUniqueResultException => None } // only possible during junit tests
                  }
                  if(!tmpPeptide.isDefined || tmpPeptide.get == null) {
                    new Peptide(sequence = peptideItem.seq, ptms = locatedPtms)
                  } else
                    tmpPeptide.get
                }
                peptideByUniqueKey += (key -> peptideFromDatabase)
                peptideFromDatabase
              })
              
              // new SequenceMatch
              val residueBefore = if(peptideItem.pre.isEmpty) '-' else peptideItem.pre.toCharArray. last
              val residueAfter = if(peptideItem.post.isEmpty) '-' else peptideItem.post.toCharArray.head
              val sequenceMatch = new SequenceMatch(
                  start = peptideItem.start, 
                  end = peptideItem.end, 
                  isDecoy = isDecoy,
                  peptide = Some(peptide),
                  residueBefore = if(residueBefore == '[') '-' else residueBefore,
                  residueAfter = if(residueBefore == ']') '-' else residueAfter)
              
              // we may also keep nb match per ion series and score ion series
              val xtandemProperties = new PeptideMatchXtandemProperties(
                  expectationValue = peptideItem.expectValue, // important for validation 
                  nextScore = peptideItem.nextScore, // I have no idea what it is and how it's calculated (but it's always close to hyperscore)
                  ionSeriesMatches = peptideItem.fragmentMatches.map(p => p.serie -> p.nbMatches).toMap, // may be useful for FragmentMatchGenerator
                  ionSeriesScores = peptideItem.fragmentMatches.map(p => p.serie -> p.score).toMap) // may be useful for FragmentMatchGenerator
              // new PeptideMatch
              if(!peptideMatchesByUniqueKey.isDefinedAt(key)) peptideMatchesByUniqueKey.put(key, new ArrayBuffer[PeptideMatch])
              // only add the peptide match if it's a "real new" one
              val newPeptideMatch = new PeptideMatch(
                  id = PeptideMatch.generateNewId(),
                  rank = 1, // X!Tandem only keeps best matches
                  score = peptideItem.hyperScore.toFloat, // also keep Evalue somewhere !
                  scoreType = PeptideMatchScoreType.XTANDEM_HYPERSCORE,
                  charge = currentQuery.charge,
//                  deltaMoz = ((peptideItem.mh + (currentQuery.charge - 1) * MolecularConstants.PROTON_MASS) / currentQuery.charge - currentQuery.moz).toFloat,
                  deltaMoz = peptideItem.delta.toFloat / currentQuery.charge,
                  isDecoy = isDecoy,
                  peptide = peptide,
                  missedCleavage = PeptideMatch.countMissedCleavages(peptide.sequence, Some(residueBefore), Some(residueAfter), msiSearch.searchSettings.usedEnzymes), 
                  fragmentMatchesCount = peptideItem.fragmentMatches.map(_.nbMatches).sum, 
                  properties = Some(new PeptideMatchProperties(xtandemProperties = Some(xtandemProperties))),
                  msQuery = currentQuery)
              if(!peptideMatchesByUniqueKey(key).exists(pm => { pm.msQuery.initialId == newPeptideMatch.msQuery.initialId && pm.peptide.uniqueKey == newPeptideMatch.peptide.uniqueKey })) {
                peptideMatchesByUniqueKey(key) += newPeptideMatch
              }

              // define the unique key for the protein
              val seqDatabaseOpt = msiSearch.searchSettings.seqDatabases.filter(_.filePath.equals(XtandemSettings.getFastaFile(proteinItem.fileMarkup.URL).getPath)).headOption.get
              val proteinAccessKey = new Tuple2(accession, seqDatabaseOpt.id)
              // get or create ProteinMatch and store it with a unique key
              if(!proteinMatchesPerUniqueKey.isDefinedAt(proteinAccessKey)) {
                // get protein first
                val protein = protProvider.getProtein(accession, seqDatabaseOpt).getOrElse(new Protein(id = Protein.generateNewId(), sequence = proteinItem.peptide.info))
                // create a protein match
                proteinMatchesPerUniqueKey.put(proteinAccessKey, new ProteinMatch(
                    id = ProteinMatch.generateNewId(),
                    accession = accession,
                    description = proteinItem.note.info, 
                    peptideMatchesCount = 0, 
                    scoreType = PeptideMatchScoreType.XTANDEM_HYPERSCORE.toString(),
                    isDecoy = isDecoy,
                    protein = if(protein.sequence.isEmpty()) None else Some(protein),
                    seqDatabaseIds = Array(seqDatabaseOpt.id)))
                sequenceMatchesPerUniqueKey.put(proteinAccessKey, new ArrayBuffer[SequenceMatch])
              }
              // add the sequence match to the list of sequence matches for this protein match
              sequenceMatchesPerUniqueKey(proteinAccessKey).append(sequenceMatch)
            }
          })
        })
      })
    })
    
    // At the end, parse all SequenceMatches and attribute BestPeptideMatch
    val bestPeptideMatchPerPeptideUniqueKey = new HashMap[String, PeptideMatch]
    // get the best peptideMatch for each Peptide
    peptideMatchesByUniqueKey.foreach { case (key, peptideMatches) => {
      val bestPeptideMatch = peptideMatches.sortBy(_.score).last
      // store the best PeptideMatch on the Proline UniqueKey instead of the XTandemParser unique key
      bestPeptideMatchPerPeptideUniqueKey(peptideMatches.head.peptide.uniqueKey) = bestPeptideMatch
    }}
    // set best peptideMatch for each sequenceMatch
    sequenceMatchesPerUniqueKey.foreach  { case (proteinAccessKey, sequenceMatches) => {
      val sequenceMatchesWithBestPeptideMatch = new ArrayBuffer[SequenceMatch]
      while(sequenceMatches.length > 0) {
        val sequenceMatch = sequenceMatches.remove(0) // remove the sequenceMatch from the array
        sequenceMatch.bestPeptideMatch = Some(bestPeptideMatchPerPeptideUniqueKey(sequenceMatch.peptide.get.uniqueKey))
        sequenceMatchesWithBestPeptideMatch += sequenceMatch // put it in the new array
      }
      val uniqueSequenceMatches = sequenceMatchesWithBestPeptideMatch.distinct.toArray
      // set sequenceMatches for this proteinMatch
      proteinMatchesPerUniqueKey(proteinAccessKey).sequenceMatches = uniqueSequenceMatches
      // also set the right peptide match count per protein
      proteinMatchesPerUniqueKey(proteinAccessKey).peptideMatchesCount = uniqueSequenceMatches.size
    }}
    
    // just before the creation of the RS, loop on ProteinMatches and merge those with same AC
    val proteinMatches = new HashMap[String, ProteinMatch]
    // loop on each protein matches
    proteinMatchesPerUniqueKey.foreach{ case (key, proteinMatch) => {
      val accession = key._1
      val seqDbId = key._2
      if(proteinMatches.contains(accession)) {
        // update this protein match
        val finalProteinMatch = proteinMatches.get(accession).get
        // add seqDbId if not already present
        if(!finalProteinMatch.seqDatabaseIds.contains(seqDbId)) {
          finalProteinMatch.seqDatabaseIds = finalProteinMatch.seqDatabaseIds ++ Array(seqDbId)
        }
        // add sequence matches
        proteinMatch.sequenceMatches.foreach(seqMatch => {
          if(!finalProteinMatch.sequenceMatches.contains(seqMatch)) {
            finalProteinMatch.sequenceMatches = finalProteinMatch.sequenceMatches ++ Array(seqMatch)
          }
        })
      } else {
        // first occurence of the protein match, add it to the list
        proteinMatches.put(accession, proteinMatchesPerUniqueKey(key))
      }
    }}
    
    logger.debug("XTandem file parsed, generating final ResultSet")

    // return the result set
    val loadedRS =  new ResultSet(
      id = ResultSet.generateNewId(),
      name = msiSearch.title,
      peptides = peptideByUniqueKey.values.toArray,
      peptideMatches = peptideMatchesByUniqueKey.flatten(_._2).toArray,
      proteinMatches = proteinMatches.values.toArray,
      isDecoy = wantDecoy, 
      isSearchResult = true,
      isValidatedContent = false, 
      msiSearch = Some(msiSearch),
      properties = Some(rsProps)) // store all raw parameters
    
    if(wantDecoy)
      _decoyResultSetOp = Some(loadedRS)
    else
      _targetResultSetOp = Some(loadedRS)
  }
  
  // not used anymore
  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {}
  
  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {
    spectrumList.foreach(spectrum => {
      onEachSpectrum(spectrum)
    })
  }
  
  def close() {}
  
  private def getLocatedPtms(peptide: XTDomain): Array[LocatedPtm] = {
    val locatedPtms: ArrayBuffer[LocatedPtm] = new ArrayBuffer[LocatedPtm]
    for (aam <- peptide.aaMarkupList) {
      val ptmResidue: Char = aam.typeMU
      val ptmPosition: Int = aam.at // WARNING: it's the position on the protein, not the position on the peptide !!
      val ptmMass: Double = aam.modified

      var _ptm: Option[PtmDefinition] = None
      // First  : search among fixedPTM
      _ptm = msiSearch.searchSettings.fixedPtmDefs.filter(ptmDef => {
        (ptmDef.residue == '\0' || ptmDef.residue == ptmResidue) && ptmDef.ptmEvidences.count(e => { scala.math.abs(ptmMass - e.monoMass) <= XtandemPtmVerifier.ptmMonoMassMargin }) > 0
      }).headOption
      // Second  : search among variablePTM
      if (!_ptm.isDefined) {
        _ptm = msiSearch.searchSettings.variablePtmDefs.filter(ptmDef => {
          (ptmDef.residue == '\0' || ptmDef.residue == ptmResidue) && ptmDef.ptmEvidences.count(e => { scala.math.abs(ptmMass - e.monoMass) <= XtandemPtmVerifier.ptmMonoMassMargin }) > 0
        }).headOption
      }
      if (_ptm.isDefined) {
        _ptm.get.ptmEvidences.filter(e => e.ionType.equals(IonTypes.Precursor) && scala.math.abs(ptmMass - e.monoMass) <= XtandemPtmVerifier.ptmMonoMassMargin).foreach(e => {
          if (_ptm.get.location matches ".+N-term$") {
            locatedPtms += new LocatedPtm(definition = _ptm.get, seqPosition = 0, monoMass = e.monoMass, averageMass = e.averageMass, 
                composition = e.composition, isNTerm = true, isCTerm = false)
          } else if (_ptm.get.location matches ".+C-term$") {
            locatedPtms += new LocatedPtm(definition = _ptm.get, seqPosition = -1, monoMass = e.monoMass, averageMass = e.averageMass, 
                composition = e.composition, isNTerm = false, isCTerm = true)
          } else {
            locatedPtms += new LocatedPtm(definition = _ptm.get, seqPosition = ptmPosition - peptide.start + 1, monoMass = e.monoMass, averageMass = e.averageMass, 
                composition = e.composition, isNTerm = false, isCTerm = false)
          }
        })
      } else {
        logger.debug("PTM not found for residue '"+ptmResidue+"' at position '"+ptmPosition+"' and mass '"+ptmMass+"'")
      }
    }
    locatedPtms.toArray
  }
  
}
