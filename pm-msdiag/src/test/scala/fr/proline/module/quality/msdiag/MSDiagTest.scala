package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.StrictLogging
import fr.proline.context.{BasicExecutionContext, IExecutionContext}
import fr.proline.core.dal.{AbstractMultipleDBTestCase, BuildMsiDbConnectionContext, BuildUdsDbConnectionContext}
import fr.proline.core.om.provider.{PeptideCacheExecutionContext, ProviderDecoratedExecutionContext}
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.repository.DriverType
import org.junit.{After, Before, Test}

@Test
class MSDiagTest extends AbstractMultipleDBTestCase with StrictLogging {

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
    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false)
    val executionContext = PeptideCacheExecutionContext(new BasicExecutionContext(1,udsDbCtx, msiDbCtx, null))
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    val rsProvider = new SQLResultSetProvider(PeptideCacheExecutionContext(parserContext))

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
