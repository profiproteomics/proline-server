/**
 * XTandemParserTest.scala
 * @author	Ibrahim YAPICI
 * @email	iyapici@unistra.fr
 * @description Tests for : 
 * - XTandemPreParsing : input file existence, markups structure/existence, existence of PTM and enzymes,  in Proline DB)
 * - XTandemParser : get datas to put in classes and send them to Proline database 
 */

package fr.proline.module.parser.xtandem
//Proline
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.repository.DriverType
import fr.proline.context.BasicExecutionContext
import fr.proline.core.om.provider.msi.impl.{ ORMResultSetProvider, SQLPTMProvider, SQLResultSetProvider }
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import org.junit.Before
import org.junit.After
import org.junit.Test
import org.junit.Assert._
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._
import scala.collection.mutable.ArrayBuffer
import java.io._
import java.util.Date
import java.io.ByteArrayOutputStream
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.dal.BuildDbConnectionContext
import fr.proline.core.dal.BuildMsiDbConnectionContext
import scala.collection.mutable.HashMap

class XTandemParserTest extends AbstractMultipleDBTestCase {
  val driverType = DriverType.H2
  var parserContext: ProviderDecoratedExecutionContext = null

  /***   CONNECTION TO DB   ***/
  
  @Before
  def init() {
        logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    logger.info("Initializing Dbs")
    psDBTestCase.loadDataSet("/default_datasets/Unimod_Dataset.xml")
    pdiDBTestCase.loadDataSet("/default_datasets/Proteins_Dataset.xml")
    msiDBTestCase.loadDataSet("/default_datasets/Init_Dataset.xml")

    logger.info("PS, PDI and MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false
    val pdiDbCtx = BuildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true) // default: true
    val psDbCtx = BuildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, true) // default: false
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)

    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    assertNotNull(parserContext)
  }

  
  @After
  override def tearDown() {
    super.tearDown()
  }
    
  var file: File = _
    // small : output.2014_11_18_11_22_40.t.xml
    // big : (81 Mo) output.2014_11_18_11_46_01.t.xml
  
  @Test
  def preParsingTest {
    // test if structure is wrong or needed markups and labels are missing

    try {
      file = new File(getClass.getResource("/xtandemResultFile/output.2014_11_18_11_22_40.t.xml").toURI)
    } catch {      
      case e: Throwable =>{ 
    	  val msg = "Error : Input file error " + e.getMessage()
    	  logger.error(msg) 
    	  fail(msg) //Fail du test si erreur.
      }
    }
    
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var parseur: SAXParser = factory.newSAXParser()
    var manager = new XtandemPreParsing(file)
    try {
      parseur.parse(file, manager)
    } catch {
      case e: ParserConfigurationException => var msg = "FileTest ParserConfigurationException : " + e.getMessage(); logger.error(msg);fail(msg)
      case e: SAXException => var msg = "FileTest SAXException : " + e.getMessage(); logger.error(msg);fail(msg)
      case e: Throwable => var msg = "FileTest Throwable : " + e.getMessage(); logger.error(msg);fail(msg)
    }

//    assert(manager.isMarkUpTestOK == true)
  }

  @Test
  def xtandemParserTest {
    logger.info("Start xtandemParserTest")
    var startTime : Long = System.currentTimeMillis()
//    logger.info("startTime")

    val myXtandemParser = new XtandemParser(new File(getClass.getResource("/xtandemResultFile/output.2015_04_08_13_00_45.t.xml").toURI), parserContext, null)  // output.2015_04_08_13_00_45.t
//    logger.info("endTime")
    var endTime : Long = System.currentTimeMillis()

//    logger.info("XtandemParser took " + (endTime - startTime) + " milliseconds")

    // Let's test if useful values in xml file are in Xtandem classes
    val resultSet = myXtandemParser.getResultSet(false)
    
    assertNotNull(resultSet)
    println("resultSet.peptideMatches.length = " + resultSet.peptideMatches.length)  // Number of <domain> markup
//    println("resultSet.name = " + resultSet.name)
//    resultSet.getProteins().foreach(protein => {
//    println("protein = " + protein)  
//    })
    
      // Display datas in XTandemClasses
//    myXtandemParser.resultBioml.groupModelList.foreach(gm => {
//      println("gm.typeMU = " + gm.typeMU + " , gm.id = " + gm.id + " , gm.mh = " + gm.mh + " , gm.z = " + gm.z)
//      
//      gm.groupSupportList.foreach(gs => {
//        println("gs.typeMU = " + gs.typeMU + " , gs.note.info = " + gs.note.info + " , gs.note.label = " + gs.note.label)
//        
//        gs.proteinList.foreach(protein => {
//           println("protein.label = " + protein.label + ", protein.sumI = " + protein.sumI)
//        })
//        
//        gs.gamlTraceList.foreach(trace => {
//          println("trace.gamlXdata.gamlValues.info = " + trace.gamlXdata.gamlValues.info)
//          println("trace.gamlYdata.gamlValues.info = " + trace.gamlYdata.gamlValues.info)
//        })
//        
//      })
//      println(" ")
//    })
    logger.info("End xtandemParserTest")
    
  }
  
//  @Test
//  def xtandemABUParserTest {
//    logger.info("Start xtandemABUParserTest")
//    var startTime : Long = System.currentTimeMillis()
//
//    val myXtandemParser = new XtandemParser(new File(getClass.getResource("/xtandemResultFile/QEKAC141027_133.raw.mzDB.t.xml").toURI), parserContext)
//    var endTime : Long = System.currentTimeMillis()
//
//    try {
//    val rs = myXtandemParser.getResultSet(false)
//    assertNotNull(rs)
//    println("resultSet.peptideMatches.length = " + rs.peptideMatches.length)  // Number of <domain> markup
//    
//    println("ABU ms1ChargeStates="+rs.msiSearch.get.searchSettings.ms1ChargeStates)
//    println("ABU ms2ChargeStates="+rs.msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ChargeStates)
////    println("ABU properties="+rs.properties.toString)
//    
//    rs.peptideMatches.filter(_.msQuery.initialId==475).foreach(pm => {
//      println("ABU id="+pm.id+" peptide="+pm.peptide.sequence+" ptms="+pm.peptide.ptmString)
//      println("ABU ptms="+pm.peptide.ptms.map(_.toReadableString()).mkString(" ; "))
//    })
//    
//    rs.peptides.filter(_.sequence.equals("GSSHHHHHHSSGLVPR")).foreach(p => println("ABU peptide to store: "+p.sequence+" "+p.ptmString))
////    rs.peptideMatches.filter(_.peptide.sequence.equals("FTAVQALSVIESSK")).foreach(pm => {
//    var nbOk = 0
//    rs.peptideMatches.foreach(pm => {
//      var ppm = ((pm.deltaMoz * 1000000) / ((pm.peptide.calculatedMass + (pm.charge * 1.007825)) / pm.charge));
////      if(ppm > 5 || ppm < -5) ppm = (((pm.deltaMoz - 1/pm.charge)* 1000000) / ((pm.peptide.calculatedMass + (pm.charge * 1.007825)) / pm.charge));
////      if(ppm > 5 || ppm < -5) ppm = (((pm.deltaMoz - 1/pm.charge)* 1000000) / ((pm.peptide.calculatedMass + (pm.charge * 1.007825)) / pm.charge));
//      if(ppm >= -5 && ppm <= 5) nbOk += 1
////      println(pm.msQuery.initialId+", "+pm.peptide.sequence+", "+pm.peptide.ptmString+", "+pm.deltaMoz+", "+pm.peptide.calculatedMass+", "+pm.msQuery.moz+", "+pm.charge+", "+ppm)
//    })
//    println("Total of good peptideMatches: "+nbOk+"/"+rs.peptideMatches.size)
//    
//    } catch {
//      case e: Exception => logger.error("ABU error", e)
//    }
//    logger.info("End xtandemABUParserTest")
//    
//  }

  @Test
  def XtandemResultFileVerifierTest {
    logger.info("Start XtandemResultFileVerifierTest")
    val file : File = new File(getClass.getResource("/xtandemResultFile/output.2014_11_18_11_46_01.t.xml").toURI)
    val myXtandemResultFileVerifier = new XtandemResultFileVerifier
    myXtandemResultFileVerifier.setParserContext(parserContext)
    
    logger.info("XtandemResultFileVerifierTest - XtandemResultFileVerifierTest-XtandemResultFileVerifierTest")
    myXtandemResultFileVerifier.getPtmDefinitions(file, null).foreach(ptm => {
      logger.debug("ptm = " + ptm)
    })
    
    myXtandemResultFileVerifier.getEnzyme(file, null).foreach(enz => {
      logger.debug("enz = " + enz)
    })
    logger.info("End XtandemResultFileVerifierTest")
  }
  
  @Test
  def XtandemResultFileProviderTest {
	  val file : File = new File(getClass.getResource("/xtandemResultFile/output.2014_11_18_11_46_01.t.xml").toURI)
    logger.info("Start XtandemResultFileProviderTest")
    val myXtandemResultFileProvider = new XtandemResultFileProvider
    myXtandemResultFileProvider.setParserContext(parserContext)
    try {
    myXtandemResultFileProvider.getResultFile(file, null, parserContext).getResultSet(false)
    } catch {
      case e: Exception =>
        logger.error("Error on XtandemResultFileProviderTest", e)
        throw e
    }
    logger.info("End XtandemResultFileProviderTest")
  }
  
//  @Test after clear DB
  def twoEnzymesFound {  // we found 'None' enzyme instead 'Trypsin' in DB , that generating failure
    // Find following two enzymes writen in Xtandem input file : Trypsin, Asp-N_ambic
    var trypsinFound : Boolean = false
    var AspNAmbicFound : Boolean = false
    var enzymeCount : Int = 0
    val file : File = new File(getClass.getResource("/xtandemResultFile/output.test.2Enzymes.xml").toURI)
    val myXtandemResultFileVerifier = new XtandemResultFileVerifier
    myXtandemResultFileVerifier.setParserContext(parserContext)
    myXtandemResultFileVerifier.getEnzyme(file, null).foreach(enz => {
      logger.debug("enz = " + enz)
      if(enz.name.toLowerCase().equals("Trypsin".toLowerCase)) trypsinFound = true
      else if(enz.name.toLowerCase().equals("Asp-N_ambic".toLowerCase)) AspNAmbicFound = true
      enzymeCount +=1
    })
    println("enzymeCount = " + enzymeCount)
    println("trypsinFound = " + trypsinFound)
    println("AspNAmbicFound = " + AspNAmbicFound)
    assertEquals(enzymeCount,2)
    assertEquals(trypsinFound,true)  // TODO : we found 'None' enzyme instead 'Trypsin' that generating failure  
    assertEquals(AspNAmbicFound,true)
  }
  
//  @Test
  def sixPtmFound {
    // Find following six PTM writen in different parameters in Xtandem input file :
    // Iodoacetamide derivative, Oxidation or Hydroxylation, Pyro-glu from Q ,Loss of ammonia, Pyro-glu from E, Acetylation
    
    var IodoacetamideDerivativeFound : Boolean = false
    var OxidationOrHydroxylationFound : Boolean = false
    var PyroGluFromQFound : Boolean = false
    var LossOfAmmoniaFound : Boolean = false
    var PyroGluFromEFound : Boolean = false
    var AcetylationFound : Boolean = false
    var ptmCount : Int = 0
    val file : File = new File(getClass.getResource("/xtandemResultFile/output.test.2PTM.xml").toURI)
    val myXtandemResultFileVerifier = new XtandemResultFileVerifier
    myXtandemResultFileVerifier.setParserContext(parserContext)
    myXtandemResultFileVerifier.getPtmDefinitions(file, null).foreach(ptm => {
      logger.debug("ptm = " + ptm)

      if(ptm.names.fullName.toLowerCase().equals("Iodoacetamide derivative".toLowerCase)) IodoacetamideDerivativeFound = true
      if(ptm.names.fullName.toLowerCase().equals("Oxidation or Hydroxylation".toLowerCase)) OxidationOrHydroxylationFound = true
      if(ptm.names.fullName.toLowerCase().equals("Pyro-glu from Q".toLowerCase)) PyroGluFromQFound = true
      if(ptm.names.fullName.toLowerCase().equals("Loss of ammonia".toLowerCase)) LossOfAmmoniaFound = true
      if(ptm.names.fullName.toLowerCase().equals("Pyro-glu from E".toLowerCase)) PyroGluFromEFound = true
      if(ptm.names.fullName.toLowerCase().equals("Acetylation".toLowerCase)) AcetylationFound = true
      ptmCount +=1
    })
    assertEquals(ptmCount,6)
    assertEquals(IodoacetamideDerivativeFound,true)
    assertEquals(OxidationOrHydroxylationFound,true)
    assertEquals(PyroGluFromQFound,true)
    assertEquals(LossOfAmmoniaFound,true)
    assertEquals(PyroGluFromEFound,true)
    assertEquals(AcetylationFound,true)
  }
    
  @Test
  def findEnzymeWrittenDifferently { 
    // Test that differently written enzymes return same result as enzyme
    // trypsin [KR]|{P} <=> [RK]|{P}
    
    // [KR]|{P} => Enzyme ?
    var enzyme1Found : String = ""
    var enzyme1Count : Int = 0
    
    var file : File = new File(getClass.getResource("/xtandemResultFile/output.test.EnzymeKRP.xml").toURI)
    var myXtandemResultFileVerifier = new XtandemResultFileVerifier
    myXtandemResultFileVerifier.setParserContext(parserContext)
    myXtandemResultFileVerifier.getEnzyme(file, null).foreach(enz => {
      logger.debug("enz = " + enz)
      enzyme1Found = enz.name
      enzyme1Count +=1
    })
    assertEquals(enzyme1Count,1)
    
    // [RK]|{P} => Enzyme ?
    var enzyme2Found : String = ""
    var enzyme2Count : Int = 0
    
    file = new File(getClass.getResource("/xtandemResultFile/output.test.EnzymeRKP.xml").toURI)
    myXtandemResultFileVerifier = new XtandemResultFileVerifier
    myXtandemResultFileVerifier.setParserContext(parserContext)
    myXtandemResultFileVerifier.getEnzyme(file, null).foreach(enz => {
      logger.debug("enz = " + enz)
      enzyme2Found = enz.name
      enzyme2Count +=1
    })
    assertEquals(enzyme2Count,1)
    
    // Enzyme("[KR]|{P}") ?<=> Enzyme("[RK]|{P}")
    assertEquals(enzyme1Found,enzyme2Found)
  }


// TODO Can uncomment following line for for more test : requires .xml test files, ask to IY
// Before uncomment tests, be sure that this cases are managed in XtandemParser.scala 
////  @Test
//  def noHistograms {
//    logger.info("Start twoEnzymes test")
//    val myXtandemParser = new XtandemParser(getClass.getResource("/xtandemResultFile/output.test.2Enzymes.xml").toURI, parserContext)
//    myXtandemParser.getResultSet(false)
//    logger.info("End twoEnzymes test")
//  }
//        
//  @Test
//  def noInputParameters {
//    logger.info("Start noInputParameters test")
//    val myXtandemParser = new XtandemParser(getClass.getResource("/xtandemResultFile/output.test.NoInputParameters.xml").toURI, parserContext)
//    myXtandemParser.getResultSet(false)
//    logger.info("End noInputParameters test")
//  }
//  
//  @Test
//  def sortByProtein {
//    logger.info("Start sortByProtein test")
////    val file : File = new File(getClass.getResource("/xtandemResultFile/output.test.2Enzymes.xml").toURI)
////    val myXtandemParser = new XtandemParser(getClass.getResource("/xtandemResultFile/output.test.SortResultByProtein.xml").toURI, parserContext)
//    val myXtandemParser = new XtandemParser(new File(getClass.getResource("/xtandemResultFile/output.test.SortResultByProtein.xml").toURI), parserContext)
//    myXtandemParser.getResultSet(false)
//    logger.info("End sortByProtein test")
//  }

  import fr.profi.util.regex.RegexUtils._
//  @Test
//  def xtandemParserABUTest {
//    logger.info("Start xtandemParserABUTest")
//    var startTime : Long = System.currentTimeMillis()
//
//    try {
//      val myXtandemParser = new XtandemParser(new File(getClass.getResource("/xtandemResultFile/QEKAC141027_84.raw.mzDB.t.xml").toURI), parserContext, null)
//      var endTime : Long = System.currentTimeMillis()
//  
//      // Let's test if useful values in xml file are in Xtandem classes
//      val resultSet = myXtandemParser.getResultSet(false)
//      
//      assertNotNull(resultSet)
//      println("resultSet.peptideMatches.length = " + resultSet.peptideMatches.length)  // Number of <domain> markup
//      
//      val regex = "###REV###".r
//      val (trs, drs) = fr.proline.core.algo.msi.TargetDecoyResultSetSplitter.split(resultSet, regex)
//      println("ABU target psm: "+trs.peptideMatches.size)
//      println("ABU decoy psm: "+drs.peptideMatches.size)
//      println("ABU decoy psm: "+trs.decoyResultSet.get.peptideMatches.size)
//      
////      resultSet.proteinMatches.foreach(pm => {
////        if(regex.findFirstIn(pm.accession).isDefined)
////          println("ABU match of protein "+pm.accession)
////      })
//      
//      logger.debug("ABU start")
////      val (decoyProtMatches, targetProtMatches) = resultSet.proteinMatches.partition { protMatch =>
////        protMatch.accession =~ regex
////      }
//      val (decoyProtMatches, targetProtMatches) = resultSet.proteinMatches.partition { protMatch =>
//        regex.findFirstIn(protMatch.accession).isDefined
//      }
//      logger.debug("ABU end")
////      val targetProtMatches = new ArrayBuffer[PeptideMatch]()
////      val decoyProtMatches = new ArrayBuffer[PeptideMatch]()
////      resultSet.proteinMatches.foreach(pm => {
////        if(regex.findFirstIn(pm.accession).isDefined)
////          decoyProtMatches ++ pm.accession
////        else
////          targetProtMatches ++ pm.accession
////      })
//      println("ABU nb targetProtMatches: "+targetProtMatches.size+" ; nb decoyProtMatches: "+decoyProtMatches.size)
//      
//      val targetPepIdSet = for (protMatch <- targetProtMatches; seqMatch <- protMatch.sequenceMatches) yield seqMatch.getPeptideId
//      val decoyPepIdSet = for (protMatch <- decoyProtMatches; seqMatch <- protMatch.sequenceMatches) yield seqMatch.getPeptideId
//      
//      println("ABU nb targetPepIdSet: "+targetPepIdSet.size+" ; nb decoyPepIdSet: "+decoyPepIdSet.size)
//      
//      val targetPepMatches = new ArrayBuffer[PeptideMatch]()
//      val decoyPepMatches = new ArrayBuffer[PeptideMatch]()
//      
//      for (pepMatch <- resultSet.peptideMatches) {
//        val pepId = pepMatch.peptide.id
//        if (targetPepIdSet.contains(pepId)) targetPepMatches += pepMatch
//        if (decoyPepIdSet.contains(pepId)) decoyPepMatches += pepMatch
//      }
//      println("ABU nb targetPepMatches: "+targetPepMatches.size+" ; nb decoyPepMatches: "+decoyPepMatches.size)
//      
//      val targetRS = this._buildResultSet(resultSet, targetProtMatches, targetPepMatches, false)
//      val decoyRS = this._buildResultSet(resultSet, decoyProtMatches, decoyPepMatches, true)
//      
//      targetRS.decoyResultSet = Some(decoyRS)
//      
//      
//      println("ABU target psm: "+targetRS.peptideMatches.size)
//      println("ABU decoy psm: "+decoyRS.peptideMatches.size)
//      println("ABU decoy psm: "+targetRS.decoyResultSet.get.peptideMatches.size)
//    
////      val pm = resultSet.peptideMatches.filter(_.msQuery.initialId==74).head
////      println("ABU peptide "+pm.peptide.sequence+" calcMass="+pm.peptide.calculatedMass+", expMoz="+pm.msQuery.moz)
//      
////      resultSet.peptideMatches.foreach(pm => {
////        println("ABU peptide at query "+pm.msQuery.initialId+" with ranks ("+pm.rank+", "+pm.cdPrettyRank+", "+pm.sdPrettyRank+"): "+pm.peptide.sequence+" and "+pm.peptide.readablePtmString+" with "+pm.missedCleavage)
////      })
////      resultSet.proteinMatches.foreach(pm => {
////        pm.sequenceMatches.filter(_.peptide.get.sequence.equals("AHNVSTSNNSPSTDNDSISK")).foreach(sm => {
////          println("ABU ProteinMatch '"+pm.accession+"' SequenceMatch "+sm.start+" to "+sm.end)
////        })
//////        println("ABU Protein '"+pm.accession+"' "+pm.description)
////      })
////      myXtandemParser.msQueries.foreach(msq => {
////        msq match {
////          case m: Ms2Query => println("ABU Query "+m.initialId+" Title "+m.spectrumTitle)
////        }
////      })
//      def onEachSpectrum(spectrum: Spectrum) = {
//        if(spectrum.title.startsWith("controllerType=0 controllerNumber=1 scan=2989 "))
//          println("ABU title "+spectrum.firstTime)
//      }
////      myXtandemParser.eachSpectrum(onEachSpectrum)
//  
//      logger.info("End xtandemParserABUTest")
//    } catch {
//      case e: Exception => {
//        e.printStackTrace()
//        throw e
//      }
//    }
//  }
  
  private def _buildResultSet(tmpRs: ResultSet,
                              protMatches: Array[ProteinMatch],
                              pepMatches: Seq[PeptideMatch],
                              isDecoy: Boolean): ResultSet = {

    val newPepMatches = new ArrayBuffer[PeptideMatch]()

    val rsId = ResultSet.generateNewId

    // Re-rank peptide matches
    pepMatches.groupBy(_.msQuery.id).foreach {
      case (msQueryId, msQueryPepMatches) =>
        val sortedPepMatches = msQueryPepMatches.sortBy(_.rank)
        var rank = 1
        for (sortedPepMatch <- sortedPepMatches) {
          val newRankedPepMatch = sortedPepMatch.copy(id = PeptideMatch.generateNewId, rank = rank, isDecoy = isDecoy, resultSetId = rsId)
          newPepMatches += newRankedPepMatch
          rank += 1
        }
    }

    val peptideByIds = newPepMatches.groupBy(_.peptide.id)

    val newProtMatches = protMatches.map { protMatch =>
      val seqMatches = protMatch.sequenceMatches.filter { seqMatch => peptideByIds.contains(seqMatch.peptide.get.id) }

      val newSeqMatch = seqMatches.map { seqMatch =>
        val pepMatches = peptideByIds.get(seqMatch.peptide.get.id).getOrElse(List())
        var bestPepMatch = pepMatches(0)
        for (i <- 1 until pepMatches.length) {
          val nextPepMatch = pepMatches(i)
          if ((bestPepMatch.score < nextPepMatch.score) || ((bestPepMatch.score == nextPepMatch.score) && (bestPepMatch.id < nextPepMatch.id)))
            bestPepMatch = nextPepMatch
        }

        seqMatch.copy(resultSetId = rsId, bestPeptideMatch = Some(bestPepMatch), isDecoy = isDecoy)
      }

      protMatch.copy(isDecoy = isDecoy, resultSetId = rsId, sequenceMatches = newSeqMatch)
    }

    // Build the result set: create a copy of the TMP one and replace some attributes
    tmpRs.copy(
      id = rsId,
      peptides = newPepMatches.map(_.peptide).distinct.toArray,
      peptideMatches = newPepMatches.toArray,
      proteinMatches = newProtMatches,
      isDecoy = isDecoy,
      decoyResultSetId = 0,
      decoyResultSet = None
    )
  }

  @Test
  def duplicatedPtms {
    logger.debug("Test 'duplicatedPtms'")
    val file : File = new File(getClass.getResource("/xtandemResultFile/ptmsDuplicated-output.2016_10_19_08_51_42.t.xml").toURI)
    val myXtandemResultFileVerifier = new XtandemResultFileVerifier
    myXtandemResultFileVerifier.setParserContext(parserContext)
    val ptms = myXtandemResultFileVerifier.getPtmDefinitions(file, null)
    val uniquePtmDefinitions = ptms.map(ptm => ptm.toReadableString() -> ptm).toMap
    logger.debug("Ptms count 1: " + myXtandemResultFileVerifier.getPtmDefinitions(file, null).size + " ptms, " + uniquePtmDefinitions.size + " unique ptms (numbers should be equal)")
    assert(ptms.size == uniquePtmDefinitions.size)
    logger.debug("Test 'duplicatedPtms' is ok")
  }
  
  @Test
  def parsingRuleOnAccessionNumbers {
    logger.info("Start parsingRuleOnAccessionNumbers")

    val file = new File(getClass.getResource("/xtandemResultFile/ptmsDuplicated-output.2016_10_19_08_51_42.t.xml").toURI)
    assert(testParsingRuleAndGetNumberOfAccessionWithTag(file, null, " ") == 0) // use default: everything until first space character
    assert(testParsingRuleAndGetNumberOfAccessionWithTag(file, "^(\\S+).*", " ") == 0) // similar as default
    assert(testParsingRuleAndGetNumberOfAccessionWithTag(file, "^([^\\|]+\\|[^\\|]+).*", " ") == 0) // everything until second pipe character
    assert(testParsingRuleAndGetNumberOfAccessionWithTag(file, "^(.+).*", " ") != 0) // everything
  }
  
  def testParsingRuleAndGetNumberOfAccessionWithTag(file: File, parsingRule: String, testTag: String): Int = {
    logger.info("Test parsing rule '"+parsingRule+"'")
    val properties: Map[String, Any] = if(parsingRule != null) Map[String, Any]("protein.parsing.rule" -> parsingRule) else null
    val parser = new XtandemParser(file, parserContext, properties)
    // Let's test if useful values in xml file are in Xtandem classes
    val rs = parser.getResultSet(false)
    // return number of accession numbers containing a space character
    val nb = rs.proteinMatches.filter(_.accession.contains(testTag)).size
    logger.info("Parsing rule '"+parsingRule+"' produced "+nb+" proteins containing the tag '"+testTag+"'")
    nb
  }
  
  // Other tests : XML File Structure is respected(use preParsingTest ?)
}