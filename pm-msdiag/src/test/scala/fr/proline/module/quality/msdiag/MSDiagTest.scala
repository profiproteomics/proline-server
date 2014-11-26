package fr.proline.module.quality.msdiag

import org.junit.After
import org.junit.Before
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.repository.DriverType

@Test
class MSDiagTest extends AbstractMultipleDBTestCase with Logging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "STR_F063442_F122817_MergedRSMs" // this result file has retention times
  val targetRSId = 2
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = null
  var rsProvider: IResultSetProvider = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    //Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")
    psDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/ps-db.xml")
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs succesfully initialized !")

    val (execContext, rsProv) = buildSQLContext()
    executionContext = execContext
    rsProvider = rsProv
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }

  def buildSQLContext() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false)
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false)
    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    val rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    (parserContext, rsProvider)
  }
  
  @Test
  def testAllMethods {
    val msdiag = new MSDiag(targetRSId, executionContext)
    msdiag.getMethods.foreach(m => {
      logger.debug("MSDiag." + m + "()")
      try {
        msdiag.executeMethod(m) match {
          case msd: MSDiagOutput => { 
            logger.debug("MSDiag: " + msd.toString)
            logger.debug(msd.getMatrixAsText)
          }
          case msd: String => logger.debug("String: " + msd)
          case _ => logger.debug("Unexpected output type")
        }
      } catch {
        case e: Exception => logger.error(e.getMessage(), e)
      }
    })
  }
  
  @Test
  def testAvailableReports {
    val msdiag = new MSDiag(targetRSId, executionContext)
    msdiag.getAvailableReports.foreach(msd => {
      logger.debug("MSDiag: "+msd.toString)
    })
  }
  
  @Test
  def testUnexpectedMethod {
    // trying to execute a method that should not be accessible
    val msdiag = new MSDiag(targetRSId, executionContext)
    try {
      logger.debug("Remotely executing forbidden method")
      msdiag.executeMethod("getMethods")
      logger.error("Forbidden method did not failed !!")
    } catch {
      case e: Exception => logger.debug("Method not allowed OK")
    }
  }
  
  @Test
  def testUnknownMethod {
    // trying to execute a method that does not exist
    val msdiag = new MSDiag(targetRSId, executionContext)
    try {
      logger.debug("Remotely executing fake method")
      msdiag.executeMethod("\\")
      logger.error("Fake method did not failed !!")
    } catch {
      case e: Exception => logger.debug("FakeMethod not allowed OK")
    }
  }

  @Test
  def testVersion {
    val version = new fr.proline.module.quality.msdiag.Version()
    logger.debug("Module name : " + version.getModuleName)
    assert("PM-MSDiag" == version.getModuleName)
    logger.debug("Module version : " + version.getVersion)
    logger.debug("TEST [testVersion] OK: versionning is successful")
  }

}
