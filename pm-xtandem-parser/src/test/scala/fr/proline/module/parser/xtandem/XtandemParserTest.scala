/**
 * XTandemParserTest.scala
 * @author	Ibrahim YAPICI
 * @email	iyapici@unistra.fr
 * @description Tests for : 
 * - XTandemPreParsing : input file existence, mark ups structure/existence, existence of PTM and enzymes,  in Proline DB)
 * - XTandemParser : get datas to put in classes and send them to Proline database 
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
import _root_.fr.proline.core.dal.AbstractMultipleDBTestCase

//Test
//import org.junit.After
import org.junit.Before
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
    //    logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    //    logger.info("Initializing Dbs")
    psDBTestCase.loadDataSet("/default_datasets/Unimod_Dataset.xml")
    pdiDBTestCase.loadDataSet("/default_datasets/Proteins_Dataset.xml")
    msiDBTestCase.loadDataSet("/default_datasets/Init_Dataset.xml")

//        logger.info("PS, PDI and MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    //    logger.info("UDS db succesfully initialized")

    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true) // default: true
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, true) // default: false
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)

    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    assertNotNull(parserContext)
  }

  // Tests variables 
  var isFileTestOK: Boolean = false
  var isUnknownMarkUpTestOK: Boolean = false
  var isUsefulLabelEnableTestOK: Boolean = false
  var isPTMsDefinedInDB : Boolean = true
  var isEnzymesDefinedInDB : Boolean = true

  var file: File = _
    // small : output.2014_11_18_11_22_40.t_sortedBySpectrumForErrorTest.xml
    // big : (81 Mo) output.2014_11_18_11_46_01.t.xml
    // big 3 : (260Mo) output.2015_01_30_15_49_47.t.xml
  
  @Test
  def preParsingTest {
      logger.info("Start preParsingTest")
      val filePath = "src\\test\\resources\\xtandemResultFile\\output.2014_11_18_11_46_01.t.xml" 
      
      val factory: SAXParserFactory = SAXParserFactory.newInstance()
      var parseur: SAXParser = factory.newSAXParser()
      var manager = new XtandemPreParsing(filePath, parserContext)
      file = new File(filePath)
      try {
        parseur.parse(file, manager)
      } catch {
        case e: ParserConfigurationException => logger.error("FileTest ParserConfigurationException : " + e.getMessage())
        case e: SAXException => logger.error("FileTest SAXException : " + e.getMessage())
      }
  
      assert(manager.isFileTestOK == true)
      assert(manager.isMarkUpTestOK == true)
      assert(manager.isPTMsDefinedInDB == true)
      assert(manager.isEnzymesDefinedInDB == true)
      logger.info("End preParsingTest")
  }

  @Test
  def xtandemParserTest {
    logger.info("Start xtandemParserTest")
    var startTime : Long = System.currentTimeMillis()
    logger.info("startTime")

    val myXtandemParser = new XtandemParser("src\\test\\resources\\xtandemResultFile\\output.2014_11_18_11_46_01.t.xml", parserContext)
    logger.info("endTime")
    var endTime : Long = System.currentTimeMillis()

    logger.info("XtandemParser took " + (endTime - startTime) + " milliseconds")
    
    // Let's test if useful values in xml file are in Xtandem classes 
    myXtandemParser.getResultSet(false)
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
    val filePath : String = "src\\test\\resources\\xtandemResultFile\\output.2014_11_18_11_46_01.t.xml"
    val myXtandemResultFileVerifier = new XtandemResultFileVerifier(filePath, parserContext)
    logger.info("XtandemResultFileVerifierTest - XtandemResultFileVerifierTest-XtandemResultFileVerifierTest")
    myXtandemResultFileVerifier.getPtmDefinitions(new File(filePath), null).foreach(ptm => {
      logger.debug("ptm = " + ptm)
    })
    
    myXtandemResultFileVerifier.getEnzyme(new File(filePath), null).foreach(enz => {
      logger.debug("enz = " + enz)
    })
    logger.info("End XtandemResultFileVerifierTest")
  }
  
  @Test
  def XtandemResultFileProviderTest {
    logger.info("Start XtandemResultFileProviderTest")
    val filePath : String = "src\\test\\resources\\xtandemResultFile\\output.2014_11_18_11_46_01.t.xml"
    val myXtandemResultFileProvider = new XtandemResultFileProvider(filePath, parserContext)
    myXtandemResultFileProvider.getResultFile(new File(filePath), null, parserContext).getResultSet(false)
    myXtandemResultFileProvider.getResultFile(new File(filePath), null, parserContext)
    logger.info("Start XtandemResultFileProviderTest")
  }
}