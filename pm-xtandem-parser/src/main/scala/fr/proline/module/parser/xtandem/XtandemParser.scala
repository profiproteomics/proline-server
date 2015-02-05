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
import _root_.fr.proline.core.om.model.msi._
import _root_.fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import _root_.fr.proline.core.om.provider.msi.IPTMProvider
import _root_.fr.proline.repository.DriverType
import _root_.fr.proline.core.dal.ContextFactory
import _root_.fr.proline.context.BasicExecutionContext
import _root_.fr.proline.core.om.provider.msi.impl.{ ORMResultSetProvider, SQLPTMProvider, SQLResultSetProvider }  // getPTMDefinition
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider  //getEnzyme

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

//Scala, Java
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

class XtandemParser(  val xtandemFilePath : String, 
                      val parserContext: ProviderDecoratedExecutionContext
                      ) extends DefaultHandler with  IResultFile with Logging {

  val fileLocation: File = new File(xtandemFilePath)
  val importProperties: Map[String,Any] = null
  val msLevel: Int = 2
  
  
  private var searchSettingIsDecoy: Boolean = false
  private var ms2Query: Ms2Query = null
  private var msiSearchForResultSet : MSISearch = null
  private var msQueries: HashMap[Int, Ms2Query] = HashMap()
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
    val ptmMonoMassMargin = 0.03
    var fixedPtmDefs: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
    var variablePtmDefs: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
    var fixedPtms: ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]] = new ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]]
    var variablePtms: ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]] = new ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]]
    
    //     val defaultPtms = XTandem apply the following PTMs as defaut variable PTMs. For reading facilities, we create this new variable. Mass values are XTandem values( and not Proline values ).
    //	  (‘Q’,-17.02655, ANY_N_TERM)  => Gln->pyro-Glu,Pyro-glu from Q
    //    (‘C’,-17.02655, ANY_N_TERM)  => Ammonia-loss,Loss of ammonia
    //    (‘E’, -18.01056, ANY_N_TERM)  => Glu->pyro-Glu,Pyro-glu from E
    variablePtmDefs.append(ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'Q', PtmLocation.ANY_N_TERM).get)
    variablePtmDefs.append(ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'C', PtmLocation.ANY_N_TERM).get)
    variablePtmDefs.append(ptmProvider.getPtmDefinition(-18.01056, ptmMonoMassMargin, 'E', PtmLocation.ANY_N_TERM).get)

    var seqDatabases: ArrayBuffer[SeqDatabase] = new ArrayBuffer[SeqDatabase]
    var seqDatabasesLocal: ArrayBuffer[SeqDatabase] = new ArrayBuffer[SeqDatabase]
    var seqDatabaseIdsArray: Array[Long] = null
    var proteinMatches: ArrayBuffer[ProteinMatch] = new ArrayBuffer[ProteinMatch]
    var peptides: ArrayBuffer[Peptide] = new ArrayBuffer[Peptide]
    var peptideMatches: ArrayBuffer[PeptideMatch] = new ArrayBuffer[PeptideMatch]

    val ptmEvidence = new PtmEvidence(
      ionType = IonTypes.Precursor,
      composition = "",
      monoMass = 0.0,
      averageMass = 0.0)

    // Get Group Parameters variables
    var seqDatabaseSequencesCount: Int = -1
    var searchSettingSoftwareVersion: String = "Unknown software version"
    var searchSettingTaxonomy: String = "Unknown taxon"
    var searchSettingMaxMissedCleavages: Int = -1
    var searchSettingMs1ChargeStates: String = "Unknown MS1 charge states"
    var searchSettingMs1ErrorTol: Double = 0.0
    var searchSettingMs1ErrorTolUnit: String = " Unknown MS1 error unit"
    var MSISearchResultFileName: String = "Unkown result file name"

    // Define (mass, residu, position) tuple for a fixed and variable PTMs. 
    //Possible positions are : PtmLocation.ANYWHERE, PROT_N_TERM, PROT_C_TERM, ANY_N_TERM, ANY_C_TERM
    var refineParamIsYes: Boolean = false
    
    var usedEnzymes : ArrayBuffer[Enzyme] = new ArrayBuffer[Enzyme]
    var isSemiSpecific : Boolean = false  // Yes if semi specific input parameters are yes : "protein, cleavage semi" or "refine, cleavage semi"
    var inputParametersEnzymeCount : Int = 0;

    var peaklistFilePathNameExt: Filename = null
    var dbProteinFileMarkupURLList: ArrayBuffer[String] = new ArrayBuffer()

    //GroupParameters variables
    for (gp <- resultBioml.groupParametersList) {
      var residueModificationMassCount = 1
      //Note variables
      for (note <- gp.noteList) {
        val dbGroupParametersNoteLabel: String = note.label
        val dbGroupParametersNoteInfo: String = note.info

        if (dbGroupParametersNoteLabel.equals("output, sort results by")) {
          if(dbGroupParametersNoteInfo.equals("protein") || dbGroupParametersNoteInfo.isEmpty) logger.error("Xtandem Parser does not manage protein sorted Xtandem File")
          else if(!dbGroupParametersNoteInfo.equals("spectrum")) logger.error("Parameter \'sort results by\' should be \'spectrum\' to be manage by Xtandemm Parser")  //This case shouldn't be appear

        } else if (dbGroupParametersNoteLabel.equals("modelling, total proteins used") && !dbGroupParametersNoteInfo.isEmpty) {
          seqDatabaseSequencesCount = augmentString(dbGroupParametersNoteInfo).toInt

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
          searchSettingMs1ErrorTol = augmentString(dbGroupParametersNoteInfo).toDouble

        } else if (dbGroupParametersNoteLabel.equals("spectrum, fragment monoisotopic mass error units") && !dbGroupParametersNoteInfo.isEmpty) {
          searchSettingMs1ErrorTolUnit = dbGroupParametersNoteInfo

        } else if (dbGroupParametersNoteLabel.equals("output, path") && !dbGroupParametersNoteInfo.isEmpty) {
          MSISearchResultFileName = dbGroupParametersNoteInfo

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
            variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, residue, PtmLocation.PROT_C_TERM))
          }

        } else if (refineParamIsYes && dbGroupParametersNoteLabel.equals("refine, potential N-terminus modifications") && !dbGroupParametersNoteInfo.isEmpty) { // variable ptms
          // Protein variable N-term PTM
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            val residue = if (atSignParts(1).charAt(0) == '[') '\0' else atSignParts(1).charAt(0)
            variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, residue, PtmLocation.PROT_N_TERM))
          }
        } else if ((dbGroupParametersNoteLabel.equals("protein, cleavage semi") || dbGroupParametersNoteLabel.equals("refine, cleavage semi")) 
                    && dbGroupParametersNoteInfo.equals("yes")) { // Refine modifications
          isSemiSpecific = true

        } else if (dbGroupParametersNoteLabel.equals("protein, cleavage site") && !dbGroupParametersNoteInfo.isEmpty) {  // Format [RK]|{P}, [[X]|[D], ..]
          val msiSearchProvider = new SQLMsiSearchProvider(parserContext.getUDSDbConnectionContext(), parserContext.getMSIDbConnectionContext(), parserContext.getPSDbConnectionContext())
          val commaParts: Array[String] = dbGroupParametersNoteInfo.split(",")
          inputParametersEnzymeCount = commaParts.length
          var residues : String = ""
          var restrictiveResidues : String = ""
          var site : String = "C-term"
          for (i <- 0 until commaParts.length) {
            
            val pipeParts: Array[String] = commaParts(i).split("\\|") // "[A]|[B]" become pipeParts[0] = "[A]", pipeParts[1] = "[B]"

            // Tests to affect residues, restrictiveResidues, site variables
            for(j <- 0 until pipeParts.length){
              if( pipeParts(j).length >2
                  && pipeParts(j).substring(0,1).equals("{") 
                  && pipeParts(j).substring(pipeParts(j).length-1,pipeParts(j).length).equals("}")){
                if (!restrictiveResidues.equals("X")) {
                  restrictiveResidues = pipeParts(j).substring(1, pipeParts(j).length()-1)
                }
                
              } else if (pipeParts(j).length >2 
                        &&pipeParts(j).substring(0,1).equals("[") 
                        && pipeParts(j).substring(pipeParts(j).length-1,pipeParts(j).length).equals("]")) {
                if (residues.equals("")) {
                  residues = pipeParts(j).substring(1, pipeParts(j).length()-1)
                } else if (j==1) {
                  site = "N-term"
                }
                
              } else {
                logger.error("Parsing error in Xtandem xml output file : parameter \"protein, cleavage site\"")
              }
            }
            
            val allEnzymesArray = msiSearchProvider.getAllEnzymes()
            
            allEnzymesArray.foreach( enz => {
              if( usedEnzymes.length == 0   // Get first found enzyme 
                 && enz.enzymeCleavages.length == 1
                 && residues.length() == enz.enzymeCleavages.head.residues.length()
                 && restrictiveResidues.length() == enz.enzymeCleavages.head.restrictiveResidues.get.length()
                 && residues.toUpperCase.sorted.equals(enz.enzymeCleavages.head.residues.toUpperCase.sorted)
                 && restrictiveResidues.toUpperCase.sorted.equals(enz.enzymeCleavages.head.restrictiveResidues.get.toUpperCase.sorted)
                 && site.toUpperCase().equals(enz.enzymeCleavages.head.site.toUpperCase())
                 && isSemiSpecific == enz.isSemiSpecific
                 ) {
                
//                logger.info("Match found ! Enzyme is  = "+ enz.name + 
//                        ", residues = " + enz.enzymeCleavages.head.residues +
//                        ", restrictiveResidues = " + enz.enzymeCleavages.head.restrictiveResidues +
//                        ", site = " + enz.enzymeCleavages.head.site +
//                        ", isSemiSpecific = " + enz.isSemiSpecific )
                usedEnzymes += enz
              }
            })
          }
        }
      }
    } //End "for groupParameters gp" loop


    // Searching PTM in Database for each PTM given in parameters
    fixedPtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { fixedPtmDefs.append(_ptm.get)}
//      else { /*_ptm = Some(null) */ logger.warn("Can not identify ptm with : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3) }
    })
    variablePtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { variablePtmDefs.append(_ptm.get)}
//      else { /*_ptm = Some(null) */ logger.warn("Can not identify ptm with : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3) }
    })

    val peaklist = new Peaklist(
      id = Peaklist.generateNewId(),
      fileType = peaklistFilePathNameExt.extension,
      path = peaklistFilePathNameExt.path,
      rawFileName = "", // ? peaklistFilePathNameExt.filename(),
      msLevel = 2)

    //GroupModel variables
    for (gm <- resultBioml.groupModelList) {
      val dbGroupModelId: Int = gm.id
      val dGroupModelMh: Double = gm.mh
      val dbGroupModelZ: Int = gm.z // ORF : peptideCharge

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
          moz = dGroupModelMh / dbGroupModelZ, // (ok ?) groupModelMh Calcul de masse/charge ?
          charge = dbGroupModelZ,
          spectrumTitle = spectrumTitle
          )
        
        msQueries.put(dbGroupModelId,ms2Query)

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
              instrumentConfigId = 0L, // (?) val instConfigId = if (instrumentConfig != null) instrumentConfig.id else 0
              peaklistId = 0L // (ok) <note label="Description">Label: W581, Spot_Id: 159751, Peak_List_Id: 184490, MSMS Job_Run_Id: 14047, Comment:  </note>
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

          //Note variables
          val dbProteinNoteLabel: String = dbProteinNote.label
          val dbProteinNoteInfo: String = dbProteinNote.info

          //FileMarkup variables
          val dbProteinFileMarkupURL: String = dbProteinFileMarkup.URL

          val fileNameToParse = new Filename(dbProteinFileMarkupURL, '/', '.')

          val seqDatabase = new SeqDatabase(
            id = SeqDatabase.generateNewId(),
            name = fileNameToParse.filename(),
            filePath = fileNameToParse.path(),
            sequencesCount = -1, 
            releaseDate = new Date())

          // Test if fasta file name already exist to avoid redundancies
          if (dbProteinFileMarkupURLList.contains(dbProteinFileMarkupURL) == false) {
            dbProteinFileMarkupURLList.append(dbProteinFileMarkupURL)
            seqDatabases.append(seqDatabase)
          }
          seqDatabasesLocal.append(seqDatabase)

          seqDatabaseIdsArray = new Array[Long](seqDatabasesLocal.size)
          for (i <- 0 until seqDatabasesLocal.size) {
            seqDatabaseIdsArray(i) = seqDatabasesLocal(i).id
          }
          seqDatabasesLocal.clear

          proteinMatches += new ProteinMatch(
            accession = dbProteinLabel,
            description = dbProteinNoteLabel,
            id = ProteinMatch.generateNewId(),
            seqDatabaseIds = seqDatabaseIdsArray)

          for (dbPeptideDomain <- dbProteinPeptide.domainList) {
            //Domain variables
            val dbDomainId: String = dbPeptideDomain.id
            val dbDomainDelta: Double = dbPeptideDomain.delta
            val dbDomainHyperScore: Double = dbPeptideDomain.hyperScore
            val dbDomainSeq: String = dbPeptideDomain.seq

            //AAMarkup variables 
            val locatedPtms: ArrayBuffer[LocatedPtm] = new ArrayBuffer[LocatedPtm]
            for (aam <- dbPeptideDomain.aaMarkupList) {
              val dbAAMarkupType: Char = aam.typeMU
              val dbAAMarkupAt: Int = aam.at
              val dbAAMarkupModified: Double = aam.modified

              var _ptm: PtmDefinition = null
              fixedPtmDefs.foreach(ptm => {
                ptm.ptmEvidences.foreach(e => {
                  if (scala.math.abs(dbAAMarkupModified - e.monoMass) <= ptmMonoMassMargin) {
                    if (ptm.residue == '\0') _ptm = ptm
                    else if (ptm.residue == dbAAMarkupType) _ptm = ptm
                  }
                })
              })

              if (_ptm == null) {
                variablePtmDefs.foreach(ptm => {
                  ptm.ptmEvidences.foreach(e => {
                    if (scala.math.abs(dbAAMarkupModified - e.monoMass) <= ptmMonoMassMargin) {
                      if (ptm.residue == '\0') _ptm = ptm
                      else if (ptm.residue == dbAAMarkupType) _ptm = ptm
                    }
                  })
                })
              }

              if (_ptm != null) {
                _ptm.ptmEvidences.foreach(e => {
                  locatedPtms += new LocatedPtm(
                    definition = _ptm,
                    seqPosition = dbAAMarkupAt,
                    monoMass = e.monoMass,
                    averageMass = e.averageMass,
                    composition = e.composition)
                })
              } else {
                logger.error("Missing PTM definiton : dbDomainId = " + dbDomainId + " , _ptm = " + _ptm + " , dbAAMarkupType = " + dbAAMarkupType + " , dbAAMarkupModified = " + dbAAMarkupModified + " , dbAAMarkupAt = " + dbAAMarkupAt)
              }
            }

            val peptide = new Peptide(
              sequence = dbDomainSeq,
              ptms = locatedPtms.toArray,
              calculatedMass = dbProteinSumI)
            peptides.append(peptide)
            locatedPtms.clear()

            val dbDomainIdParts: Array[String] = dbDomainId.split("\\.")
            var dbDomainIdPartsInt: Array[Int] = new Array[Int](dbDomainIdParts.length)
            for (i <- 0 until dbDomainIdParts.length) {
              dbDomainIdPartsInt(i) = augmentString(dbDomainIdParts(i)).toInt
            }

            var peptideMatchIsDecoy: Boolean = false
            if (dbProteinNoteInfo.contains("reversed")) peptideMatchIsDecoy = true

            peptideMatches += new PeptideMatch(
              id = PeptideMatch.generateNewId(),
              rank = dbDomainIdPartsInt(1), /*3 dans DomainId<domain id=987.3.1 ...> */
              score = dbDomainHyperScore.toFloat,
              scoreType = "xtandem:hyperscore",
              deltaMoz = dbDomainDelta.toFloat,
              isDecoy = peptideMatchIsDecoy,
              peptide = peptide,
              msQuery = ms2Query
              )
          }
        }
      } // end of GroupSupport loop
    } //End "for groupModel gm" loop

    val searchSettings = new SearchSettings(
      id = SearchSettings.generateNewId(),
      softwareName = "X!Tandem ",
      softwareVersion = searchSettingSoftwareVersion,
      taxonomy = searchSettingTaxonomy,
      maxMissedCleavages = searchSettingMaxMissedCleavages,
      ms1ChargeStates = searchSettingMs1ChargeStates,
      ms1ErrorTol = searchSettingMs1ErrorTol,
      ms1ErrorTolUnit = searchSettingMs1ErrorTolUnit,
      isDecoy = searchSettingIsDecoy,
      usedEnzymes = usedEnzymes.toArray,
      variablePtmDefs = variablePtmDefs.toArray,
      fixedPtmDefs = fixedPtmDefs.toArray,
      seqDatabases = seqDatabases.toArray,
      instrumentConfig = null
      )

    msiSearchForResultSet = new MSISearch(
      id = MSISearch.generateNewId(),
      resultFileName = MSISearchResultFileName,
      submittedQueriesCount = resultBioml.groupModelList.length,
      searchSettings = searchSettings,
      peakList = peaklist,
      date = new java.util.Date(),
      title = MSISearchResultFileName,
      queriesCount = resultBioml.groupModelList.length)

    logger.info("resultBioml.groupModelList.length = " + resultBioml.groupModelList.length)
    val resultSet = new ResultSet(
      peptides = peptides.toArray,
      peptideMatches = peptideMatches.toArray,
      proteinMatches = proteinMatches.toArray,
      isDecoy = false,
      isNative = true,
      id = ResultSet.generateNewId(),
      name = msiSearchForResultSet.title,
      msiSearch = Some(msiSearchForResultSet)
      )
    resultSet
  }
  
  val hasDecoyResultSet: Boolean = searchSettingIsDecoy 
  val hasMs2Peaklist: Boolean = true
  val msQueryByInitialId: Map[Int, MsQuery] =
  {
    val msQueryMapBuilder = scala.collection.immutable.Map.newBuilder[Int, MsQuery]
    for (query <- msQueries) {
      msQueryMapBuilder += (query._1 -> query._2)
    }
    msQueryMapBuilder.result()
  }
  
    def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {
      spectrumList.foreach(spectra => {
         onEachSpectrum(spectra) 
      })
  }
  
  val msiSearch: MSISearch = msiSearchForResultSet
  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {}
  def close() {}
}

/*
for GroupParameters
new Peaklist
for groupModel
---| for groupSupport
------| new Ms2query
------| for gamlTrace
---------| new Spectrum
------| for proteinList
---------| new SeqDataBase
---------| new ProteinMatch
---------| for dbPeptideDomain
------------| for aaMarkupList
---------------| new LocatedPtm
------------| new Peptide
------------| new PeptideMatch
new SearchSettings
new MSISearch
new ResultSet
//*/