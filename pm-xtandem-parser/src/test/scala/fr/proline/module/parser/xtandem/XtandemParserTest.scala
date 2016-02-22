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
import fr.proline.core.dal.ContextFactory
import fr.proline.context.BasicExecutionContext
import fr.proline.core.om.provider.msi.impl.{ ORMResultSetProvider, SQLPTMProvider, SQLResultSetProvider }  // getPTMDefinition
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider  //getEnzyme
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry

//Test
import org.junit.Before
import org.junit.After
import org.junit.Test
import org.junit.Assert._

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import scala.collection.mutable.ArrayBuffer
import java.io._
import java.util.Date
import java.io.ByteArrayOutputStream

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

    val udsDbCtx = ContextFactory.buildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true) // default: true
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, true) // default: false
    val msiDbCtx = ContextFactory.buildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

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

    val myXtandemParser = new XtandemParser(new File(getClass.getResource("/xtandemResultFile/output.2015_04_08_13_00_45.t.xml").toURI), parserContext)  // output.2015_04_08_13_00_45.t
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
    myXtandemResultFileProvider.getResultFile(file, null, parserContext).getResultSet(false)
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
  
  @Test
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
  @Test
  def sortByProtein {
    logger.info("Start sortByProtein test")
//    val file : File = new File(getClass.getResource("/xtandemResultFile/output.test.2Enzymes.xml").toURI)
//    val myXtandemParser = new XtandemParser(getClass.getResource("/xtandemResultFile/output.test.SortResultByProtein.xml").toURI, parserContext)
    val myXtandemParser = new XtandemParser(new File(getClass.getResource("/xtandemResultFile/output.test.SortResultByProtein.xml").toURI), parserContext)
    myXtandemParser.getResultSet(false)
    logger.info("End sortByProtein test")
  }
  
  // Other tests : XML File Structure is respected(use preParsingTest ?)
}