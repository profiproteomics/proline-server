package fr.proline.module.parser.mzidentml

import java.net.URL

import org.junit.AfterClass
import org.junit.Assert
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test

import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.BuildDbConnectionContext
import fr.proline.core.dal.BuildMsiDbConnectionContext
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.om.model.msi.Instrument
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.repository.DriverType

object ReadFileTest extends AbstractMultipleDBTestCase {

  val driverType = DriverType.H2
  var parserContext: ProviderDecoratedExecutionContext = null

  /***   CONNECTION TO DB   ***/

  @BeforeClass
  def init() {
    logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    logger.info("Initializing Dbs")
    msiDBTestCase.loadDataSet("/dbunit/datasets/msi-db_init_dataset.xml")
    psDBTestCase.loadDataSet("/dbunit/datasets/ps-db_init_dataset.xml")

    logger.info("PS and MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    //udsDBTestCase.loadCompositeDataSet(Array("/dbunit/datasets/uds-db_init_dataset.xml", "/default_datasets/UDS_Simple_Dataset.xml"))
    logger.info("UDS db succesfully initialized")

    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false

    val psDbCtx = BuildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, true) // default: false
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

    val executionContext = new BasicExecutionContext(udsDbCtx, null, psDbCtx, msiDbCtx, null)

    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    Assert.assertNotNull(parserContext)
  }
  
  @AfterClass
  override def tearDown() {
    if (parserContext != null) parserContext.closeAll()
    super.tearDown()
  }
  
}

class ReadFileTest {
  
  val parserContext = ReadFileTest.parserContext
  val mascotFilePath = "/result_files/mascotExample.mzid"
  val mgfPFilePath = "/result_files/mgfPlus.mzid"
  
  @Test
  def testReadMascotResultFile() {
   
    val inst = new Instrument(-1, "teqt", "src", None)
    val ic = new InstrumentConfig(-1, inst, "ms1A", "msnA", "ActType")
    val fileURL: URL = classOf[ReadFileTest].getResource(mascotFilePath)
    val rf = MzIdResultFile(fileURL, parserContext)
    rf.instrumentConfig = Some(ic)

    rf.getResultSet(false)
  }

  @Test
  def testReadMgfPlusResultFile() {
   
    val inst = new Instrument(-1, "teqt", "src", None)
    val ic = new InstrumentConfig(-1, inst, "ms1A", "msnA", "ActType")
    val fileURL: URL = classOf[ReadFileTest].getResource(mgfPFilePath)
    val rf = MzIdResultFile(fileURL, parserContext)
    rf.instrumentConfig = Some(ic)

    rf.getResultSet(false)
  }
}