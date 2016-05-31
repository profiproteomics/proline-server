package fr.proline.module.parser.mzidentml

import java.net.URL
import org.junit.Ignore
import org.junit.Test
import fr.proline.context.BasicExecutionContext
import fr.proline.core.om.model.msi.Instrument
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.PTMFakeProvider
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType
import fr.proline.core.dal.BuildMsiDbConnectionContext
import org.junit.Before
import org.junit.Assert
import fr.proline.core.dal.BuildDbConnectionContext
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider

@Test
class ReadFileTest extends AbstractMultipleDBTestCase {

  val driverType = DriverType.H2
  var parserContext: ProviderDecoratedExecutionContext = null

  val mascotFilePath = "/resultFiles/mascotExample.mzid"
  val mgfPFilePath = "/resultFiles/mgfPlus.mzid"

  /***   CONNECTION TO DB   ***/

  @Before
  def init() {
    logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    logger.info("Initializing Dbs")
    psDBTestCase.loadDataSet("/default_datasets/Unimod_Dataset.xml")
   msiDBTestCase.loadDataSet("/default_datasets/Init_Dataset.xml")

    logger.info("PS, PDI and MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false

    val psDbCtx = BuildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, true) // default: false
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

    val executionContext = new BasicExecutionContext(udsDbCtx, null, psDbCtx, msiDbCtx, null)

    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

     Assert.assertNotNull(parserContext)
  }
  
  @Test
  def testReadMascotResultFile() {
   
    val inst: Instrument = new Instrument(-1, "teqt", "src", None)
    val ic: InstrumentConfig = new InstrumentConfig(-1, inst, "ms1A", "msnA", "ActType")
    val fileURL: URL = classOf[ReadFileTest].getResource(mascotFilePath);
    val rf: MzIdResultFile = MzIdResultFile(fileURL, parserContext)
    rf.instrumentConfig = Some(ic)

    rf.getResultSet(false)

  }

@Test
  def testReadMgfPlusResultFile() {
   
    val inst: Instrument = new Instrument(-1, "teqt", "src", None)
    val ic: InstrumentConfig = new InstrumentConfig(-1, inst, "ms1A", "msnA", "ActType")
    val fileURL: URL = classOf[ReadFileTest].getResource(mgfPFilePath);
    val rf: MzIdResultFile = MzIdResultFile(fileURL, parserContext)
    rf.instrumentConfig = Some(ic)

    rf.getResultSet(false)

  }
}