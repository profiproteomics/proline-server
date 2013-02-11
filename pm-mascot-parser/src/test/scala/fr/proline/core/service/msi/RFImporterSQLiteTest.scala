package fr.proline.core.service.msi

import java.io.File

import org.junit.{ After, AfterClass, Assert, Before, BeforeClass, Test }

import com.weiglewilczek.slf4s.Logging

import fr.proline.core.om.model.msi.ResultSet
import fr.proline.repository.DriverType
import fr.proline.util.StringUtils

@AfterClass
@BeforeClass
object RFImporterSQLiteTest extends Logging {

  // TODO: retrieve this value from the properties
  val MSI_SQLITE_MEMORY_LOCK_FILE = "msi_sqlite_shared_memory.lock"
  val UDS_SQLITE_MEMORY_LOCK_FILE = "uds_sqlite_shared_memory.lock"
  val PS_SQLITE_MEMORY_LOCK_FILE = "ps_sqlite_shared_memory.lock"
  val PDI_SQLITE_MEMORY_LOCK_FILE = "pdi_sqlite_shared_memory.lock"

  private def _deleteSQLiteLockFile(path: String) {

    if (!StringUtils.isEmpty(path)) {

      try {
        val sqliteLock = new File(path)

        if (sqliteLock.exists) {

          if (sqliteLock.delete()) {
            logger.debug("File [" + path + "] successfully deleted")
          } else {
            logger.warn("Unable to delete [" + path + ']')
          }

        }

      } catch {
        case ex: Exception => logger.error("Error deleteing [" + path + ']', ex)
      }

    }

  }

  @AfterClass
  @BeforeClass
  def deleteSQLiteLockFiles() {
    _deleteSQLiteLockFile(MSI_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(UDS_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(PS_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(PDI_SQLITE_MEMORY_LOCK_FILE)
  }

}

@Test
class RFImporterSQLiteTest extends AbstractRFImporterTest_ {

  val driverType = DriverType.SQLITE

  protected override def beforeAllTests() {}
  protected override def afterAllTests() {}

  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    RFImporterSQLiteTest.deleteSQLiteLockFiles()
    super.setUp()
    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    // Remove existing external DB connections

    //val udsSqlHelper = new SQLQueryHelper( dbManagerForTest.asInstanceOf[DatabaseManager].getMsiDbConnector(1) ).ezDBC
    //sys.error( "" + udsSqlHelper.selectInt("select count(*) from scoring") )

    ()
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  @Test
  def testRFIwithSQL() = {
    val (executionContext, rsProvider) = buildSQLContext()
    Assert.assertNotNull(executionContext)

    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFImporterSQLiteTest.this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)

    val importer = new ResultFileImporterSQLStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = Map.empty,
      acDecoyRegex = None)

    logger.debug(" --- run service ")
    val result = importer.runService()
    val id = importer.getTargetResultSetId
    logger.debug(" --- done " + result + " save with resultID " + id)

    Assert.assertTrue(result)
    Assert.assertNotNull(id)
    Assert.assertTrue(id > 0)

    val rsBackOp = rsProvider.getResultSet(id)
    Assert.assertTrue(rsBackOp.isDefined)
    val rsBack: ResultSet = rsBackOp.get
    Assert.assertNotNull(rsBack)

    //msiEzDBC.connection.close()
    //psEzDBC.connection.close()

    // Other verifs....

    executionContext.closeAll()
  }

  @Test
  def runRFIwithJPA() = {
    val (executionContext, rsProvider) = buildJPAContext()

    Assert.assertNotNull(executionContext)

    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFImporterSQLiteTest.this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)

    val importer = new ResultFileImporterJPAStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = Map.empty,
      acDecoyRegex = None)

    logger.debug(" --- run service ")
    val result = importer.runService()
    val id = importer.getTargetResultSetId
    logger.debug(" --- done " + result + " save with resultID " + id)

    Assert.assertTrue(result)
    Assert.assertNotNull(id)
    Assert.assertTrue(id > 0)

    val rsBackOp = rsProvider.getResultSet(id)
    Assert.assertTrue(rsBackOp.isDefined)
    val rsBack: ResultSet = rsBackOp.get
    Assert.assertNotNull(rsBack)

    //msiEzDBC.connection.close()
    //psEzDBC.connection.close()

    // Other verifs....

    executionContext.closeAll()
  }

}
