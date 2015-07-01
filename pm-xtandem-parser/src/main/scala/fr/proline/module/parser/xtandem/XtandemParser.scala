/**
 * XTandemParser.scala
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Get datas from XML file to put in Xtandem classes, then create Proline objects 
 * Using SAX Parser, datas are collected in XtandemClasses with tree node structure, 
 * then datas in this tree are used to create database related Proline objects as ResultSet, SearchSettings, MSISearch
 */

package fr.proline.module.parser.xtandem

//Proline
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.repository.DriverType
import fr.proline.core.dal.ContextFactory
import fr.proline.context.BasicExecutionContext
import fr.proline.core.om.provider.msi.impl.{ ORMResultSetProvider, SQLPTMProvider, SQLResultSetProvider }
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.io._
import java.util.Date
import java.io.ByteArrayOutputStream
import com.typesafe.scalalogging.slf4j.Logging

//This class allows to separate file path, name and extension
class Filename(str: String, sep: Char, ext: Char) {

  var fullPath: String = str
  var pathSeparator: Char = sep
  var extensionSeparator: Char = ext

  def extension(): String = {
    val dot: Int = fullPath.lastIndexOf(extensionSeparator)
    return fullPath.substring(dot + 1)
  }
  // gets filename without extension
  def filename(): String = {
    val dot: Int = fullPath.lastIndexOf(extensionSeparator)
    val sep: Int = fullPath.lastIndexOf(pathSeparator)
    return fullPath.substring(sep + 1, dot)
  }

  def path(): String = {
    val sep: Int = fullPath.lastIndexOf(pathSeparator)
    var resultPath = ""
    if (sep != -1) {
      resultPath = fullPath.substring(0, sep)
    }
    return resultPath
  }
}

class XtandemParser(  val xtandemFile : File, 
                      val parserContext: ProviderDecoratedExecutionContext
                      ) extends DefaultHandler with IResultFile with Logging {

  val fileLocation: File = xtandemFile
  // Inherited methods
  val importProperties: Map[String,Any] = null
  val msLevel: Int = 2
  var msQueriesList : ArrayBuffer[MsQuery] =  new ArrayBuffer[MsQuery]
  var msQueries = msQueriesList.toArray
  val hasDecoyResultSet: Boolean = searchSettingIsDecoy 
  val hasMs2Peaklist: Boolean = true
  
  private var searchSettingIsDecoy: Boolean = false
  private var ms2Query: Ms2Query = null
  private var msiSearchForResultSet : MSISearch = null
  private var spectrumList : ArrayBuffer[Spectrum] = new ArrayBuffer[Spectrum]
  var resultBioml: XTBioml = new XTBioml()
  
  def getResultBioml : XTBioml = resultBioml  
  
  def getResultSet(wantDecoy: Boolean): ResultSet = {
    
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var parseur: SAXParser = factory.newSAXParser()
    var manager = new XtandemHandler()
    try {
      var startTime : Long = System.currentTimeMillis()
      parseur.parse(fileLocation, manager)
      var endTime : Long = System.currentTimeMillis()
      logger.info("XtandemHandler took " + (endTime - startTime) + " milliseconds")
      
      resultBioml = manager.bioml
    } catch {
      case e: ParserConfigurationException => logger.error(e.getMessage())
      case e: SAXException => logger.error(e.getMessage())
    }

    // Using MSI DataBase classes to send collected XML informations
    // Some invariable definition
    val ptmProvider: IPTMProvider = parserContext.getProvider(classOf[IPTMProvider])
    val ptmMonoMassMargin = 0.01
    var fixedPtmDefs: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
    var variablePtmDefs: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
    var fixedPtms: ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]] = new ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]]
    var variablePtms: ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]] = new ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]]
    
    //     val defaultPtms = XTandem use the following PTMs as defaut variable PTMs. For reading facilities, we create this new variable. Mass values are XTandem values( and not Proline values ).
    //	  (‘Q’,-17.02655, ANY_N_TERM)  => Gln->pyro-Glu,Pyro-glu from Q
    //    (‘C’,-17.02655, ANY_N_TERM)  => Ammonia-loss,Loss of ammonia
    //    (‘E’, -18.01056, ANY_N_TERM)  => Glu->pyro-Glu,Pyro-glu from E
    variablePtmDefs.append(ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'Q', PtmLocation.ANY_N_TERM).get)
    variablePtmDefs.append(ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'C', PtmLocation.ANY_N_TERM).get)
    variablePtmDefs.append(ptmProvider.getPtmDefinition(-18.01056, ptmMonoMassMargin, 'E', PtmLocation.ANY_N_TERM).get)
    
    var locatedPtmIsNTerm : Boolean = false;
    var locatedPtmIsCTerm : Boolean = false;
    var locatedPtmSeqPosition : Int = 0; 

    var seqDatabases: ArrayBuffer[SeqDatabase] = new ArrayBuffer[SeqDatabase]
    var seqDatabaseIdsArray: Array[Long] = null
    var seqDatabaseFileNames : String = ""
    var seqDatabaseFileNamesPath: String = ""
    var seqDatabase : SeqDatabase = null
    
    var proteinMatches: ArrayBuffer[ProteinMatch] = new ArrayBuffer[ProteinMatch]
    var peptides: ArrayBuffer[Peptide] = new ArrayBuffer[Peptide]
    var peptideMatches: ArrayBuffer[PeptideMatch] = new ArrayBuffer[PeptideMatch]
    var peptide : Peptide = null
    var newPeptideMatch : PeptideMatch = null
    var protein : Protein = null
    var proteinList : ArrayBuffer[Protein] = new ArrayBuffer[Protein]

    val ptmEvidence = new PtmEvidence(
      ionType = IonTypes.Precursor,
      composition = "",
      monoMass = 0.0,
      averageMass = 0.0)

    // Get Group Parameters variables
    var searchSettingSoftwareVersion: String = "Unknown software version"
    var searchSettingTaxonomy: String = "Unknown taxon"  // possible values of taxon for Xtandem file are listed in input.xml or taxonomy.xml files which are given for X!Tandem search
    var searchSettingMaxMissedCleavages: Int = -1
        var searchSettingMs1ChargeStates: String = "4"   // default value in Xtandem = 4
    var searchSettingMs1ErrorTolMinus: Double = 0.0  // (what value) X!Tandem inserts a default value for this parameter if it doesn't exist or is set to 0.0
    var searchSettingMs1ErrorTolPlus: Double = 0.0  
    var searchSettingMs1ErrorTolUnit: String = "Xtandem default unit"
    var searchSettingMs2ChargeStates: String = "4"   // default value in Xtandem = 4
    var searchSettingMs2ErrorTol: Double = 0.0  
    var searchSettingMs2ErrorTolUnit: String = "Xtandem default unit"
    var msiSearchResultFileName: String = "output.xml"

    // Define (mass, residu, position) tuple for a fixed and variable PTMs. 
    //Possible positions are : PtmLocation.ANYWHERE, PROT_N_TERM, PROT_C_TERM, ANY_N_TERM, ANY_C_TERM
    var refineParamIsYes: Boolean = false
    
    var usedEnzymes : ArrayBuffer[Enzyme] = new ArrayBuffer[Enzyme]
    var isSemiSpecific : Boolean = false  // Yes if semi specific input parameters are yes : "protein, cleavage semi" or "refine, cleavage semi"
    var inputParametersEnzymeCount : Int = 0

    var peaklistFilePathNameExt: Filename = null
    var dbDomainSeqList : ArrayBuffer[String] = new ArrayBuffer()
    var seqMatch : SequenceMatch = null

    //GroupParameters variables
    for (gp <- resultBioml.groupParametersList) {
      var residueModificationMassCount = 1
      var sequenceSourceCount = 1
      
      //Note variables
      for (note <- gp.noteList) {
        val dbGroupParametersNoteLabel: String = note.label
        val dbGroupParametersNoteInfo: String = note.info
        // ERROR : (unavailable parameters) We first manage case where we can't continue parsing of XML file
        if (dbGroupParametersNoteLabel.equals("output, sort results by")) {
          if(dbGroupParametersNoteInfo.equals("protein") || dbGroupParametersNoteInfo.isEmpty){
            logger.error("Xtandem Parser does not manage \"sort result by protein\" X!Tandem File")
//            throw new RuntimeException("Xtandem Parser does not manage \"sort result by protein\" X!Tandem File")
          }
          else if(!dbGroupParametersNoteInfo.equals("spectrum")) logger.error("Parameter \'sort results by\' should be \'spectrum\' to be manage by Xtandemm Parser")  //This case shouldn't be appear

        } else if (dbGroupParametersNoteLabel.equals("spectrum, path") && !dbGroupParametersNoteInfo.isEmpty) {
          val FPATH: String = dbGroupParametersNoteInfo
          peaklistFilePathNameExt = new Filename(FPATH, '/', '.')

        } else if (dbGroupParametersNoteLabel.equals("process, version") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingSoftwareVersion = dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("protein, taxon") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingTaxonomy = dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("scoring, maximum missed cleavage sites") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMaxMissedCleavages = augmentString(dbGroupParametersNoteInfo).toInt

        } else if (dbGroupParametersNoteLabel.equals("spectrum, fragment monoisotopic mass error") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs2ErrorTol = augmentString(dbGroupParametersNoteInfo).toDouble

        } else if (dbGroupParametersNoteLabel.equals("spectrum, fragment monoisotopic mass error units") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs2ErrorTolUnit = if(dbGroupParametersNoteInfo.equals("Daltons")) "Da" else /*ppm*/dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("spectrum, parent monoisotopic mass error minus") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs1ErrorTolMinus = augmentString(dbGroupParametersNoteInfo).toDouble

        } else if (dbGroupParametersNoteLabel.equals("spectrum, parent monoisotopic mass error plus") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs1ErrorTolPlus = augmentString(dbGroupParametersNoteInfo).toDouble

        } else if (dbGroupParametersNoteLabel.equals("spectrum, parent monoisotopic mass error units") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs1ErrorTolUnit = if(dbGroupParametersNoteInfo.equals("Daltons")) "Da" else /*ppm*/dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("output, path") && !dbGroupParametersNoteInfo.isEmpty) {
          msiSearchResultFileName = dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("spectrum, maximum parent charge") && dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs1ChargeStates = dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("scoring, include reverse") && !dbGroupParametersNoteInfo.isEmpty) {
          if (dbGroupParametersNoteInfo.equals("yes") || dbGroupParametersNoteInfo.equals("only"))
            searchSettingIsDecoy = true

        } else if (dbGroupParametersNoteLabel.contains("residue, modification mass") && !dbGroupParametersNoteInfo.isEmpty) { //<note type="input" label="residue, modification mass">57.022@C</note>
          // Protein fixed anywhere ptms
          if (dbGroupParametersNoteLabel.equals("residue, modification mass")) {
            val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
            for (i <- 0 until commaParts.length) {
              val atSignParts: Array[String] = commaParts(i).split("@")
              fixedPtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
            }

          } else if (dbGroupParametersNoteLabel.equals("residue, modification mass ".concat(residueModificationMassCount.toString()))) {
            // If it contains several fixed PTMs in several lines
            val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
            for (i <- 0 until commaParts.length) {
              val atSignParts: Array[String] = commaParts(i).split("@")
              fixedPtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
            }
            residueModificationMassCount += 1
          }

        } else if (dbGroupParametersNoteLabel.equals("residue, potential modification mass") && !dbGroupParametersNoteInfo.isEmpty) {
          // Protein variable anywhere PTMs
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
          }
        } else if (dbGroupParametersNoteLabel.equals("protein, C-terminal residue modification mass") && !dbGroupParametersNoteInfo.isEmpty && augmentString(dbGroupParametersNoteInfo).toDouble != 0.0) { // 0.0 is the default value for this paramater in XTandem
          // Protein fixed C-term PTMs
          fixedPtms.append(Tuple3(augmentString(dbGroupParametersNoteInfo).toDouble, '\0', PtmLocation.PROT_C_TERM))

        } else if (dbGroupParametersNoteLabel.equals("protein, N-terminal residue modification mass") && !dbGroupParametersNoteInfo.isEmpty && augmentString(dbGroupParametersNoteInfo).toDouble != 0.0) {
          // Protein fixed N-term PTM
          fixedPtms.append(Tuple3(augmentString(dbGroupParametersNoteInfo).toDouble, '\0', PtmLocation.PROT_N_TERM))

        } else if (dbGroupParametersNoteLabel.equals("refine") && dbGroupParametersNoteInfo.equals("yes")) {
           // Refine search is done
          refineParamIsYes = true

        } else if (refineParamIsYes && dbGroupParametersNoteLabel.equals("refine, modification mass") && !dbGroupParametersNoteInfo.isEmpty) {
          // Protein fixed anywhere PTM
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            fixedPtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
          }

        } else if (refineParamIsYes && dbGroupParametersNoteLabel.equals("refine, potential modification mass") && !dbGroupParametersNoteInfo.isEmpty) { // variable ptms
          // Protein variable anywhere PTM
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
          }

        } else if (refineParamIsYes && dbGroupParametersNoteLabel.equals("refine, potential C-terminus modifications") && !dbGroupParametersNoteInfo.isEmpty) { // variable ptms
          // Protein variable C-term PTM
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            val residue = if (atSignParts(1).charAt(0) == '[') '\0' else atSignParts(1).charAt(0)
            variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, residue, PtmLocation.ANY_C_TERM))
          }

        } else if (refineParamIsYes && dbGroupParametersNoteLabel.equals("refine, potential N-terminus modifications") && !dbGroupParametersNoteInfo.isEmpty) { // variable ptms
          // Protein variable N-term PTM
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            val residue = if (atSignParts(1).charAt(0) == '[') '\0' else atSignParts(1).charAt(0)
            variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, residue, PtmLocation.ANY_N_TERM))
          }
        } else if ((dbGroupParametersNoteLabel.equals("protein, cleavage semi") || dbGroupParametersNoteLabel.equals("refine, cleavage semi")) // TODO ? In Xtandem's xml results file, we can't distingue if semi cleavage is after standard research or refine research 
                    && dbGroupParametersNoteInfo.equals("yes")) { 
          isSemiSpecific = true

        } else if (dbGroupParametersNoteLabel.equals("protein, cleavage site") && !dbGroupParametersNoteInfo.isEmpty) {  // Format [RK]|{P}, [[X]|[D], ..]
          val msiSearchProvider = new SQLMsiSearchProvider(parserContext.getUDSDbConnectionContext(), parserContext.getMSIDbConnectionContext(), parserContext.getPSDbConnectionContext())
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",")
          inputParametersEnzymeCount = commaParts.length
          var residue : String = ""
          var restrictiveResidue : String = ""
          var site : String = ""
          val allEnzymesArray = msiSearchProvider.getAllEnzymes()
          
          for (i <- 0 until commaParts.length) {
            residue = ""
            restrictiveResidue = ""
            site = ""
            val pipeParts: Array[String] = commaParts(i).split("\\|")
            
            if(pipeParts.length ==2) {
              val leftResidue : String = pipeParts(0).substring(1,pipeParts(0).length-1)
              val rightResidue : String =  pipeParts(1).substring(1,pipeParts(1).length-1)
              
              if( (pipeParts(0).length==3 && pipeParts(0).substring(0,1).equals("{") && pipeParts(0).substring(pipeParts(0).length-1,pipeParts(0).length).equals("}")) 
                  || pipeParts(0).equals("[X]"))
              {
                site = "N-term"
                residue = rightResidue
                if(!leftResidue.equals("X")) restrictiveResidue = leftResidue
                
              } else if( (pipeParts(1).length==3 && pipeParts(1).substring(0,1).equals("{") && pipeParts(1).substring(pipeParts(1).length-1,pipeParts(1).length).equals("}")) 
                  || pipeParts(1).equals("[X]"))
              {
                site = "C-term"
                residue = leftResidue
                if(!rightResidue.equals("X")) restrictiveResidue = rightResidue
                
              } else {
                logger.error("Enzyme : can't etablish site of enzyme")  
              }
            } else {
              logger.error("More then 2 residues are found. Format should be for exemple [KR]|{P} for trypsin")
            }
            
            val enzyme = findEnzyme(allEnzymesArray, residue, restrictiveResidue, site, isSemiSpecific)
            if(!enzyme.isEmpty) {
              usedEnzymes += enzyme.get
            }
          }
        } else if (dbGroupParametersNoteLabel.equals("list path, sequence source #".concat(sequenceSourceCount.toString()))) {

//          if(sequenceSourceCount == 1 ) {
          val FPATH: String = dbGroupParametersNoteInfo
          seqDatabaseFileNames = new Filename(FPATH, '/', '.').filename()
          seqDatabaseFileNamesPath = new Filename(dbGroupParametersNoteInfo, '/', '.').path + "/" + seqDatabaseFileNames
                    
          seqDatabase = new SeqDatabase(
            id = SeqDatabase.generateNewId(),
            name = seqDatabaseFileNames, 
            filePath = seqDatabaseFileNamesPath,
            sequencesCount = -1, 
            releaseDate = new Date())

          seqDatabases.append(seqDatabase)

          seqDatabaseIdsArray = new Array[Long](seqDatabases.size)
          for (i <- 0 until seqDatabases.size) {
            seqDatabaseIdsArray(i) = seqDatabases(i).id
//            logger.debug("IY - XtandemParser.scala - seqDatabaseIdsArray(" + i + " ) = " + seqDatabaseIdsArray(i))
          }
          
          sequenceSourceCount += 1
        }
      }
    } //End "for groupParameters gp" loop


//    logger.debug("fixedPtms.length = " + fixedPtms.length )
    // Searching PTM in Database for each PTM given in parameters
    fixedPtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { fixedPtmDefs.append(_ptm.get); logger.warn("fixedPtmDefs : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3 + " ... is " + _ptm.get.names.shortName)}
      else { /*_ptm = Some(null) */ logger.warn("Can not identify ptm with : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3) }
    })
//    logger.debug("variablePtms.length = " + variablePtms.length )
    variablePtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { variablePtmDefs.append(_ptm.get); logger.warn("variablePtmDefs : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3 + " ... is " + _ptm.get.names.shortName)}
      else { /*_ptm = Some(null) */ logger.warn("Can not identify ptm with : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3) }
    })
//    logger.debug("IY - XtandemParser.scala -fixedPtmDefs.length = " + fixedPtmDefs.length )
//    fixedPtmDefs.foreach(fpd => {
//      logger.debug("IY - XtandemParser.scala - fpd.ptmEvidences.head.mono_mass = " + fpd.ptmEvidences.head.monoMass + ", fpd.residue = " + fpd.residue + ", fpd.names.shortName" + fpd.names.shortName + ", fpd.location" + fpd.location )
//    })
//    logger.debug("variablePtmDefs.length = " + variablePtmDefs.length )
//    variablePtmDefs.foreach(fpd => {
//      logger.debug("IY - XtandemParser.scala - fpd.ptmEvidences.head.mono_mass = " + fpd.ptmEvidences.head.monoMass + ", fpd.residue = " + fpd.residue + ", fpd.names.shortName = " + fpd.names.shortName + ", fpd.location = " + fpd.location )
//    })

    val peaklist = new Peaklist(
      id = Peaklist.generateNewId(),
      fileType = peaklistFilePathNameExt.extension,
      path = peaklistFilePathNameExt.path,
      rawFileIdentifier = peaklistFilePathNameExt.filename,
      msLevel = msLevel)
    
    //GroupModel variables
    for (gm <- resultBioml.groupModelList) {
      val dbGroupModelId: Int = gm.id
      val dGroupModelMh: Double = gm.mh
      val dbGroupModelZ: Int = gm.z

      //GroupSupport variables
      for (gs <- gm.groupSupportList) {
        val dbGroupSupportNote: XTNote = gs.note //NONE
        var spectrumTitle: String = "No spectrum title"
        
        if (dbGroupSupportNote != null) {
          spectrumTitle = dbGroupSupportNote.info
        }
        
        ms2Query = new Ms2Query(
          id = Ms2Query.generateNewId(),
          initialId = dbGroupModelId,
          moz = dGroupModelMh / dbGroupModelZ,
          charge = dbGroupModelZ,
          spectrumTitle = spectrumTitle
          )
        
        msQueriesList += ms2Query

        //GAMLTrace variables
        for (gamlTrace <- gs.gamlTraceList) {
          val dbGAMLTraceXdata: XTGAMLXdata = gamlTrace.gamlXdata
          val dbGAMLTraceYdata: XTGAMLYdata = gamlTrace.gamlYdata

          //GAMLXdata variables
          val dbGAMLXdataUnits: String = dbGAMLTraceXdata.units // can be : score, mass_to_charge_ratio, numbers of ions
          val dbGAMLXdataValues: XTGAMLValues = dbGAMLTraceXdata.gamlValues

          //GAMLXdataValues variables
          val dbGAMLXdataValuesInfo: String = dbGAMLXdataValues.info // values are separated by space

          //GAMLYdata variables
          val dbGAMLYdataValues: XTGAMLValues = dbGAMLTraceYdata.gamlValues //NONE

          //GAMLYdataValues variables
          val dbGAMLYdataValuesInfo: String = dbGAMLYdataValues.info // values are separated by space

          if (dbGAMLXdataUnits.equals("MASSTOCHARGERATIO")) {
            val mozValues: String = dbGAMLXdataValuesInfo
            var mozListTempParts: Array[String] = mozValues.split(" ")
            var mozListParts: Array[Double] = new Array[Double](mozListTempParts.length)

            for (i <- 0 until mozListTempParts.length) {
              mozListParts(i) = augmentString(mozListTempParts(i)).toDouble
            }

            val intensityList: String = dbGAMLYdataValuesInfo
            var intensityListTempParts: Array[String] = intensityList.split(" ")
            var intensityListParts: Array[Float] = new Array[Float](intensityListTempParts.length)

            for (i <- 0 until intensityListParts.length) {
              intensityListParts(i) = augmentString(intensityListTempParts(i)).toFloat
            }

            spectrumList+= new Spectrum(
              id = Spectrum.generateNewId(),
              title = spectrumTitle,
              precursorMoz = augmentString(gamlTrace.gamlAttributeList(0).info).toDouble,  // (ok?) <GAML:attribute type="M+H">2082.91</GAML:attribute>
              precursorCharge = augmentString(gamlTrace.gamlAttributeList(1).info).toInt,  // <GAML:attribute type="charge">1</GAML:attribute>
              mozList = Some(mozListParts), //
              intensityList = Some(intensityListParts),
              peaksCount = mozListTempParts.length,
              instrumentConfigId = if (instrumentConfig.isDefined) instrumentConfig.get.id else 0, // (?) val instConfigId = if (instrumentConfig != null) instrumentConfig.id else 0
              peaklistId = peaklist.id // (ok) <note label="Description">Label: W581, Spot_Id: 159751, Peak_List_Id: 184490, MSMS Job_Run_Id: 14047, Comment:  </note>
              )
          }
        }

        //Protein variables
        for (p <- gs.proteinList) {
          val dbProteinLabel: String = p.label
          val dbProteinSumI: Double = p.sumI
          val dbProteinNote: XTNote = p.note
          val dbProteinFileMarkup: XTFileMarkup = p.fileMarkup
          val dbProteinPeptide: XTPeptide = p.peptide
          val dbProteinPeptideInfo: String = p.peptide.info
          val dbProteinPeptideEnd : Int = p.peptide.end

          //Note variables
          val dbProteinNoteLabel: String = dbProteinNote.label
          val dbProteinNoteInfo: String = dbProteinNote.info

          //VDS : Try to find seqDB for current Protein... removing ".pro" extention !
          //FileMarkup variables
          val dbProteinFileMarkupURL: String = if(dbProteinFileMarkup.URL.endsWith(".pro"))dbProteinFileMarkup.URL.substring(0, dbProteinFileMarkup.URL.length()-4) else dbProteinFileMarkup.URL
          
          val currentProtMatchSeqDBIdsBuilder = Array.newBuilder[Long]
          seqDatabases.foreach(seqDB=>{
            if(seqDB.filePath.equals(dbProteinFileMarkupURL)){
              currentProtMatchSeqDBIdsBuilder += seqDB.id
            }
          })
          var currentProtMatchSeqDBIds = currentProtMatchSeqDBIdsBuilder.result
          if(currentProtMatchSeqDBIds.length <1){
           currentProtMatchSeqDBIds = seqDatabaseIdsArray
           logger.trace(" ===== NOT FOUND SEQ DB ! ")
          }
          
          protein = new Protein(
            id = Protein.generateNewId(),
            sequence = dbProteinPeptideInfo
          )

          var newProteinMatch = new ProteinMatch(
            accession = dbProteinLabel,
            description = dbProteinNoteLabel,
            id = ProteinMatch.generateNewId(),
            seqDatabaseIds = currentProtMatchSeqDBIds,
            scoreType = "xtandem:hyperscore",
            protein = Some(protein))

          for (dbPeptideDomain <- dbProteinPeptide.domainList) {
            //Domain variables
            val dbDomainId: String = dbPeptideDomain.id
            var dbDomainStart: Int = dbPeptideDomain.start
            val dbDomainEnd: Int = dbPeptideDomain.end
            val dbDomainMh: Double = dbPeptideDomain.mh
            val dbDomainDelta: Double = dbPeptideDomain.delta
            val dbDomainHyperScore: Double = dbPeptideDomain.hyperScore
            val dbDomainPre: String = dbPeptideDomain.pre
            val dbDomainPost: String = dbPeptideDomain.post
            val dbDomainSeq: String = dbPeptideDomain.seq
            
                      // Create SequenceMatch and ProteinMatch, if necessary, for current Matched Protein
          seqMatch = new SequenceMatch(
            start = dbDomainStart,
            end = dbDomainEnd,
            residueBefore = if(dbDomainStart == 1) '-' else dbDomainPre.charAt(dbDomainPre.length()-1),
            residueAfter = if(dbDomainEnd == dbProteinPeptideEnd) '-' else dbDomainPost.charAt(0),
            isDecoy = false
//            peptide = Some(peptide),
//            bestPeptideMatch = Some(newPeptideMatch)
//            resultSetId = bestPepMatch.resultSetId
          )
            
            //AAMarkup variables 
            val locatedPtms: ArrayBuffer[LocatedPtm] = new ArrayBuffer[LocatedPtm]
            for (aam <- dbPeptideDomain.aaMarkupList) {
              val dbAAMarkupType: Char = aam.typeMU
              var dbAAMarkupAt: Int = aam.at
              val dbAAMarkupModified: Double = aam.modified

              var _ptm: PtmDefinition = null
              // First  : search among fixedPTM
              fixedPtmDefs.foreach(ptm => {
                ptm.ptmEvidences.foreach(e => {
                  if (scala.math.abs(dbAAMarkupModified - e.monoMass) <= ptmMonoMassMargin) {
                    if (ptm.residue == '\0' || ptm.residue == dbAAMarkupType) _ptm = ptm
                  }
                })
              })

              // Second  : search among variablePTM
              if (_ptm == null) {
                variablePtmDefs.foreach(ptm => {
                  ptm.ptmEvidences.foreach(e => {
                    if (scala.math.abs(dbAAMarkupModified - e.monoMass) <= ptmMonoMassMargin) {
                      if (ptm.residue == '\0' || ptm.residue == dbAAMarkupType) _ptm = ptm
                    }
                  })
                })
              }

              if (_ptm != null) { 
//                println("IY - XtandemParser.scala - _ptm.names.shortName = " + _ptm.names.shortName + ", _ptm.location = " + _ptm.location + ", dbAAMarkupAt = " + dbAAMarkupAt)
//                println("IY - XtandemParser.scala - dbDomainStart = " + dbDomainStart + ", dbAAMarkupAt = " + dbAAMarkupAt + ", dbAAMarkupAt - dbDomainStart = " + (dbAAMarkupAt - dbDomainStart))
//                if(_ptm.location.contains("N-term")) { dbAAMarkupAt = 0 ; println("Now dbAAMarkupAt = " + dbAAMarkupAt)}
//                else if(_ptm.location.contains("C-term")) { dbAAMarkupAt = -1 ; println("Now dbAAMarkupAt = " + dbAAMarkupAt)}
//                if(dbAAMarkupAt==67 ) dbAAMarkupAt=0
//                logger.debug("IY - _ptm.location = " + _ptm.location)
                if( _ptm.location matches ".+N-term$" ) {
                  locatedPtmIsNTerm = true
                  locatedPtmSeqPosition = 0 
//                  println("IY : N-term : "+dbDomainId+": "+dbAAMarkupAt +" "+ dbDomainStart)
                } else if( _ptm.location matches ".+C-term$" ) {
                  locatedPtmIsCTerm = true
                  locatedPtmSeqPosition = -1 
//                  println("IY : C-term : "+dbDomainId+": "+dbAAMarkupAt +" "+ dbDomainStart)
                } else {
                  locatedPtmIsNTerm = false
                  locatedPtmIsCTerm = false
                  locatedPtmSeqPosition = dbAAMarkupAt - dbDomainStart
//                  println("IY : "+dbDomainId+": "+dbAAMarkupAt +" "+ dbDomainStart)
                }
                _ptm.ptmEvidences.foreach(e => {
                  locatedPtms += new LocatedPtm(
                    definition = _ptm,
                    seqPosition = locatedPtmSeqPosition ,
                    monoMass = e.monoMass,
                    averageMass = e.averageMass,
                    composition = e.composition, 
                    isNTerm = locatedPtmIsNTerm, 
                    isCTerm = locatedPtmIsCTerm)
                
                }) 
              } else {
                logger.error("Missing PTM definiton : dbDomainId = " + dbDomainId + " , _ptm = " + _ptm + " , dbAAMarkupType = " + dbAAMarkupType + " , dbAAMarkupModified = " + dbAAMarkupModified + " , dbAAMarkupAt = " + dbAAMarkupAt)
              }
            }

            if(!dbDomainSeqList.contains(dbDomainSeq)) {  // Creating new peptide only if there is a new sequence, otherwise pgAdmin return duplicated sequence value
              peptide = new Peptide(
                sequence = dbDomainSeq,
                ptms = locatedPtms.toArray,
                calculatedMass = dbDomainMh)
//              logger.debug("IY - XtandemParser.scala - Creating new peptide. Seq = " + dbDomainSeq + ", peptide = " + peptide)
              peptides.append(peptide)
              dbDomainSeqList.append(dbDomainSeq)
            } else {
              peptide = peptides.find(e => e.sequence.equals(dbDomainSeq)).get

//              logger.debug("IY - XtandemParser.scala - Ignoring new peptide. Seq = " + dbDomainSeq + ", peptide = " + peptide)
            }
            locatedPtms.clear()

            // Complete seqMatch parameters before add to ProteinMatch
            seqMatch.peptide = Some(peptide)

            val dbDomainIdParts: Array[String] = dbDomainId.split("\\.")
            var dbDomainIdPartsInt: Array[Int] = new Array[Int](dbDomainIdParts.length)
            for (i <- 0 until dbDomainIdParts.length) {
              dbDomainIdPartsInt(i) = augmentString(dbDomainIdParts(i)).toInt
            }

            var peptideMatchIsDecoy: Boolean = false
            if (dbProteinNoteInfo.contains("reversed")) peptideMatchIsDecoy = true

//            logger.debug("IY - XtandemParser.scala - ms2Query.id = " + ms2Query.id)
            newPeptideMatch = new PeptideMatch(
              id = PeptideMatch.generateNewId(),
              rank = dbDomainIdPartsInt(1), /*3 dans DomainId<domain id=987.3.1 ...> */
              score = dbDomainHyperScore.toFloat,
              charge = dbGroupModelZ,
              scoreType = PeptideMatchScoreType.XTANDEM_HYPERSCORE,
              deltaMoz = dbDomainDelta.toFloat,
              isDecoy = peptideMatchIsDecoy,
              peptide = peptide,
              msQuery = ms2Query
              )
            peptideMatches += newPeptideMatch
            seqMatch.bestPeptideMatch =  Some(newPeptideMatch)
          }
          
//        proteinMatches += newProteinMatch
          val newSeqMatches = new ArrayBuffer[SequenceMatch]()
          if (newProteinMatch.sequenceMatches != null) newSeqMatches ++= newProteinMatch.sequenceMatches
          newSeqMatches += seqMatch
          newProteinMatch.sequenceMatches = newSeqMatches.toArray

          var proteinMatchAlreadyExists = false
          var indexProtMatch = 0
          while(indexProtMatch < proteinMatches.length && !proteinMatchAlreadyExists){
        	val pm =  proteinMatches(indexProtMatch)
        	indexProtMatch +=1
            if(pm.accession == newProteinMatch.accession) {
              //ProteinMatch found, just update necessary properties
              newProteinMatch.seqDatabaseIds.foreach( seqDbId =>{
            	  if(!pm.seqDatabaseIds.contains(seqDbId)){
            	    val finalSeqDB = new ArrayBuffer[Long]()
    	    		finalSeqDB ++= pm.seqDatabaseIds
    	    		finalSeqDB += seqDbId
    	    		pm.seqDatabaseIds = finalSeqDB.toArray
            	  }
              })
              
              newProteinMatch.sequenceMatches.foreach( seqMatch =>{
            	  if(!pm.sequenceMatches.contains(seqMatch)){
            	    val finalSeqMatches = new ArrayBuffer[SequenceMatch]()
    	    		finalSeqMatches ++= pm.sequenceMatches
    	    		finalSeqMatches += seqMatch
    	    		pm.sequenceMatches = finalSeqMatches.toArray
            	  }
              })
              

              proteinMatchAlreadyExists = true
            } //End if accession exist 
          }

          if(!proteinMatchAlreadyExists){
            proteinMatches += newProteinMatch
//            logger.debug("IY 18/06 - XtandemParser - add newProteinMatch "+newProteinMatch.accession)
          }
//          else {
//            logger.debug("IY 18/06 - XtandemParser - newProteinMatch " + newProteinMatch.accession + " n'a pas été enregistré dans proteinMatches !")
//          }
          
        }
      } // end of GroupSupport loop
    } //End "for groupModel gm" loop
    var compteur = 0
    proteinMatches.foreach(pm => {
//      logger.debug("IY 02/06 - XtandemParser - pm.sequenceMatches.length = " + pm.sequenceMatches.length)
//      logger.debug("IY 02/06 - XtandemParser - pm.sequenceMatches = " + pm.sequenceMatches)
//      logger.debug("IY 02/06 - XtandemParser - pm.accession = " + pm.accession)
//      logger.debug("IY 11/06 - XtandemParser - pm.protein= " + pm.protein)
      if(pm.sequenceMatches.length > 1 ) compteur +=1
    })
//    logger.debug("IY 16/06 - XtandemParser - Il y a " + compteur + " sequenceMatch de longueur superieur à 1 !")
    msQueries = msQueriesList.toArray

//    logger.debug("IY - XtandemParser.scala - peptideMatches = " + peptideMatches )
    val searchSettings = new SearchSettings(
      id = SearchSettings.generateNewId(),
      softwareName = "XTandem",
      softwareVersion = searchSettingSoftwareVersion,
      taxonomy = searchSettingTaxonomy,
      maxMissedCleavages = searchSettingMaxMissedCleavages,
      ms1ChargeStates = searchSettingMs1ChargeStates,
      ms1ErrorTol = searchSettingMs1ErrorTolMinus,  // TODO Only minus value are given to SearchSettings !! manage minus and plus values !!
      ms1ErrorTolUnit = searchSettingMs1ErrorTolUnit,
      isDecoy = searchSettingIsDecoy,
      usedEnzymes = usedEnzymes.toArray,
      variablePtmDefs = variablePtmDefs.toArray,
      fixedPtmDefs = fixedPtmDefs.toArray,
      seqDatabases = seqDatabases.toArray,
      instrumentConfig = instrumentConfig.getOrElse(null)
      )

    if (msLevel == 2) {
      searchSettings.msmsSearchSettings = Some(
        new MSMSSearchSettings(
          ms2ChargeStates = searchSettingMs2ChargeStates,
          ms2ErrorTol = searchSettingMs2ErrorTol,
          ms2ErrorTolUnit = searchSettingMs2ErrorTolUnit)
      )
    }
    
    msiSearchForResultSet = new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = msiSearchResultFileName,
//      submittedQueriesCount = resultBioml.groupModelList.length,
      searchSettings = searchSettings,
      peakList = peaklist,
      date = new java.util.Date(),
      title = msiSearchResultFileName,
      queriesCount = resultBioml.groupModelList.length)

//    logger.info("IY - XtandemParser - msiSearchForResultSet.peaklist = " + msiSearchForResultSet)
    
//    logger.info("IY - XtandemParser - resultBioml.groupModelList.length = " + resultBioml.groupModelList.length)
    val resultSet = new ResultSet(
      peptides = peptides.toArray,
      peptideMatches = peptideMatches.toArray,
      proteinMatches = proteinMatches.toArray,
      isDecoy = false,
//      isNative = true,
      id = ResultSet.generateNewId(),
      name = msiSearchForResultSet.title,
      msiSearch = Some(msiSearchForResultSet),
      properties = Some(new ResultSetProperties()),
      isSearchResult = true, 
      isValidatedContent = false
      )
//    logger.info("IY - XtandemParser - resultSet = " + resultSet)
//    peptideMatches.foreach(pm => {
//      pm.peptide.ptms.foreach(p => {
//        if(p.definition.location matches ".+N-term$") logger.debug("NTERM: "+pm.peptide.sequence+" ptm="+pm.peptide.ptmString+" Position="+p.seqPosition)
//      })
//    })
    resultSet
  }

  

  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {
    spectrumList.foreach(spectra => {
       onEachSpectrum(spectra) 
    })
  }

  lazy val msiSearch: MSISearch = msiSearchForResultSet
  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {}
  def close() {}
  
  	// Find and return first found enzyme in a enzyme array 
	def findEnzyme(allEnzymesArray : Array[Enzyme], residue : String, restrictiveResidue : String, site : String, semiSpecific : Boolean) : Option[Enzyme] = {
	  
	  allEnzymesArray.foreach( enz => {
      if(enz.enzymeCleavages.length == 1
         && residue.length() == enz.enzymeCleavages.head.residues.length()
         && enz.enzymeCleavages.head.restrictiveResidues != None
         && restrictiveResidue.length() == enz.enzymeCleavages.head.restrictiveResidues.get.length()
         && residue.toUpperCase.sorted.equals(enz.enzymeCleavages.head.residues.toUpperCase.sorted)
         && restrictiveResidue.toUpperCase.sorted.equals(enz.enzymeCleavages.head.restrictiveResidues.get.toUpperCase.sorted)
         && site.toUpperCase().equals(enz.enzymeCleavages.head.site.toUpperCase())
         && semiSpecific == enz.isSemiSpecific
         ) {
//        logger.debug("Match found ! Enzyme is  = "+ enz.name + 
//                ", residues = " + enz.enzymeCleavages.head.residues +
//                ", restrictiveResidues = " + enz.enzymeCleavages.head.restrictiveResidues +
//                ", site = " + enz.enzymeCleavages.head.site +
//                ", isSemiSpecific = " + enz.isSemiSpecific )
       
        return Some(enz)
      }
    })
    None
	}
}