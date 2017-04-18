package fr.proline.module.parser.xtandem

import java.io.File
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import org.xml.sax.helpers.DefaultHandler
import fr.proline.core.om.model.msi.IResultFile
import com.typesafe.scalalogging.LazyLogging
import fr.proline.core.om.model.msi.Spectrum
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.IonTypes
import fr.proline.core.om.model.msi.MsQuery
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.MSMSSearchSettings
import fr.proline.core.om.model.msi.Peaklist
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.SearchSettings
import fr.proline.core.om.model.msi.SeqDatabase
import fr.proline.core.om.model.msi.SpectrumTitleFields
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ProteinEmptyFakeProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.model.msi.SpectrumProperties
import javax.xml.parsers.SAXParserFactory
import javax.xml.parsers.SAXParser
import javax.xml.parsers.ParserConfigurationException
import org.xml.sax.SAXException
import fr.proline.core.om.model.msi.Peptide
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.ResultSetProperties
import fr.proline.core.om.model.msi.XTandemImportProperties
import scala.collection.mutable.HashMap
import fr.proline.core.om.model.msi.Ms2Query
import fr.profi.chemistry.model.MolecularConstants
import fr.profi.chemistry.model.Enzyme
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.SequenceMatch
import fr.proline.core.om.model.msi.PeptideMatchScoreType
import fr.proline.core.om.model.msi.Protein
import fr.proline.core.om.model.msi.PeptideMatchProperties
import fr.proline.core.om.model.msi.PeptideMatchXtandemProperties

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
  var hasDecoyResultSet: Boolean = false
  var hasMs2Peaklist: Boolean = true
  val msLevel: Int = 2
  def msQueries = msQueriesList.toArray
  var msiSearch: MSISearch = null
  val ionSeries = new ArrayBuffer[String]
  
  // set Providers
  private lazy val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
  private lazy val pepProvider = parserContext.getProvider(classOf[IPeptideProvider])
  private lazy val seqDbProvider = parserContext.getProvider(classOf[ISeqDatabaseProvider])
  private lazy val protProvider = if (parserContext.getProvider(classOf[IProteinProvider]) == null ||
    parserContext.getProvider(classOf[IProteinProvider]).isInstanceOf[ProteinEmptyFakeProvider]) {
    ProteinFakeProvider //Fake provider should at least create Fake Proteins, not None
  } else {
    parserContext.getProvider(classOf[IProteinProvider])
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
    
    // parse the xml file
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var saxParser: SAXParser = factory.newSAXParser()
    var manager = new XtandemHandler()
    var startTime: Long = System.currentTimeMillis()
    saxParser.parse(fileLocation, manager)
    logger.info("Xtandem file read in " + (System.currentTimeMillis() - startTime) + " milliseconds")
    val resultBioml = manager.bioml

    val peptideByUniqueKey = new HashMap[String, Peptide]
    val peptideMatchesByUniqueKey = new HashMap[String, ArrayBuffer[PeptideMatch]]
    val proteinMatchesPerUniqueKey = new HashMap[Tuple2[String, Long], ProteinMatch]
    val sequenceMatchesPerUniqueKey = new HashMap[Tuple2[String, Long], ArrayBuffer[SequenceMatch]]
    
    // get parsing rules if any
    val parsingRules = if(this.peaklistSoftware.isDefined) this.peaklistSoftware.get.specTitleParsingRule else None
    
    // get search settings
    val rsProps = extractMsiSearchAndProperties(resultBioml)

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
              val seqDatabaseOpt = msiSearch.searchSettings.seqDatabases.filter(_.filePath.equals(getFastaFile(proteinItem.fileMarkup.URL).getPath)).headOption.get
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

  private def extractMsiSearchAndProperties(resultBioml: XTBioml): ResultSetProperties = {
    // parse all settings
    val settings = new HashMap[String, String]
    resultBioml.groupParametersList.foreach(group => {
      group.noteList.foreach(note => {
        settings.put(note.label, note.info)
      })
    })
    
    // get ion series requested
    if(settings.isDefinedAt("scoring, a ions")) ionSeries += "a"
    if(settings.isDefinedAt("scoring, b ions")) ionSeries += "b"
    if(settings.isDefinedAt("scoring, c ions")) ionSeries += "c"
    if(settings.isDefinedAt("scoring, x ions")) ionSeries += "x"
    if(settings.isDefinedAt("scoring, y ions")) ionSeries += "y"
    if(settings.isDefinedAt("scoring, z ions")) ionSeries += "z"
    
    // get fasta file(s)
    val fastaFiles = new ArrayBuffer[SeqDatabase]
    val nbProteinUsed = settings.getOrElse("modelling, total proteins used", "0").toString.toInt
    settings.foreach { case (key, value) => {
      if(key.startsWith("list path, sequence source #")) {
        val fastaFile = getFastaFile(value.toString)
        val seqDatabase = seqDbProvider.getSeqDatabase(fastaFile.getName, fastaFile.getPath)
        if(seqDatabase.isDefined)
          fastaFiles += seqDatabase.get
        else
          fastaFiles += new SeqDatabase(
              id = SeqDatabase.generateNewId(),
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
    
    // get enzymes
    val isSemiSpecific = settings.getOrElse("protein, cleavage semi", "no").toString.equals("yes")
    val usedEnzymes = XTandemEnzymeVerifier.extractEnzymesFromXTandemString(parserContext, settings.getOrElse("protein, cleavage site", "").toString.toUpperCase.replaceAll("[X]", "_"), isSemiSpecific)
    logger.debug("Enzymes: "+usedEnzymes.map(_.name).mkString(", "))
    
    // get PTMs
    val refinedResults = settings.getOrElse("refine", "no").equals("yes")
    val fixedPtmDefs = XtandemPtmVerifier.getFixedPtms(
        ptmProvider, 
        settings.getOrElse("protein, C-terminal residue modification mass", "").toString, 
        settings.getOrElse("protein, N-terminal residue modification mass", "").toString, 
        settings.getOrElse("residue, modification mass", "").toString, 
        settings.getOrElse("refine, modification mass", "").toString, 
        refinedResults)
    val variablePtmDefs = XtandemPtmVerifier.getVariablePtms(
        ptmProvider, 
        settings.getOrElse("residue, potential modification mass", "").toString, 
        settings.getOrElse("refine, potential modification mass", "").toString, 
        settings.getOrElse("refine, potential C-terminus modifications", "").toString, 
        settings.getOrElse("refine, potential N-terminus modifications", "").toString, 
        settings.getOrElse("protein, quick acetyl", "yes").toString.equals("yes"), 
        settings.getOrElse("protein, quick pyrolidone", "yes").toString.equals("yes"), 
        refinedResults)
    
    // set SearchSettings
    // XTandem only allows to set maximum charge state, so we have to turn 4 into "1+, 2+, 3+, 4+"
    val ms1ChargeStates = List.range(1, settings.getOrElse("spectrum, maximum parent charge", "4").toInt + 1).map(_+"+").mkString(", ")
    val ms1ErrorTolMinusOrPlus = settings.getOrElse("spectrum, parent monoisotopic mass error minus", settings.getOrElse("spectrum, parent monoisotopic mass error plus", 0.0))
    val ms1ToleranceUnit = settings.getOrElse("spectrum, parent monoisotopic mass error units", "Da")
    val ms2ToleranceUnit = settings.getOrElse("spectrum, fragment monoisotopic mass error units", "Da")
    val searchSettings = new SearchSettings(
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
        instrumentConfig = instrumentConfig.getOrElse(null), // not available at this moment
        msmsSearchSettings = Some(
          new MSMSSearchSettings(
            ms2ChargeStates = ms1ChargeStates, // not an actual setting, using MS1 value instead
            ms2ErrorTol = settings.getOrElse("spectrum, fragment monoisotopic mass error", 0).toString.toDouble,
            ms2ErrorTolUnit = if(ms1ToleranceUnit.equals("Daltons")) "Da" else "ppm")))

    // set MSISearch
    val inputFile = new File(settings.getOrElse("spectrum, path", "").toString) // cannot be empty !!
    val outputFile = new File(settings.getOrElse("output, path", "output.xml").toString) // cannot be empty !!
    val searchStartTime = if(settings.isDefinedAt("process, start time")) {
      // date is like "2015:04:27:15:41:28"
      val items = settings("process, start time").toString.split(":")
      items(0)+"/"+items(1)+"/"+items(2)+" "+items(3)+":"+items(4)+":"+items(5)
    } else ""
    msiSearch = new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = outputFile.getName,
      searchSettings = searchSettings,
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
      date = new java.util.Date(searchStartTime),
      title = outputFile.getName,
      resultFileDirectory = outputFile.getParent,
      jobNumber = 0,
      userName = "",
      userEmail = "",
      queriesCount = settings.getOrElse("total spectra used", "0").toString.toInt,
      searchedSequencesCount = nbProteinUsed)
    
    hasMs2Peaklist = settings.getOrElse("output, spectra", "yes").equals("yes")
    hasDecoyResultSet = searchSettings.isDecoy

    val rsProps = new ResultSetProperties()
    val rsImportProperties = new XTandemImportProperties
    rsImportProperties.setRawSettings(Some(settings.toMap))
    rsProps.setXtandemImportProperties(Some(rsImportProperties))
    
    logger.debug("XTandem search settings gathered")
    
    rsProps
  }
  
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
    
  private def getFastaFile(value: String): File = new File(value.replaceAll(".pro$", "")) // xtandem may append ".pro" to a fasta file
}
