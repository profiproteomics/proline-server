package fr.proline.core.service.msi

import java.io.File
import org.junit.{ After, AfterClass }
import org.junit.{ Before, BeforeClass, Test }
import org.junit.Assert.{ assertNotNull, assertTrue }
import com.typesafe.scalalogging.LazyLogging 
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.repository.DriverType
import fr.profi.util.StringUtils
import org.junit.Ignore
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.IResultSetProvider

object RFImporterSQLiteJPATest extends LazyLogging  {

  // TODO: retrieve this value from the properties
  /* On Linux servers, the lock file name contains ":uds_test?mode=memory&cache=shared" suffix */
  val UDS_SQLITE_MEMORY_LOCK_FILE = "uds_sqlite_shared_memory.lock"
  val UDS_SQLITE_LINUX_FILE = "uds_sqlite_shared_memory.lock:uds_test?mode=memory&cache=shared"

  val PDI_SQLITE_MEMORY_LOCK_FILE = "pdi_sqlite_shared_memory.lock"
  val PDI_SQLITE_LINUX_FILE = "pdi_sqlite_shared_memory.lock:pdi_test?mode=memory&cache=shared"

  val PS_SQLITE_MEMORY_LOCK_FILE = "ps_sqlite_shared_memory.lock"
  val PS_SQLITE_LINUX_FILE = "ps_sqlite_shared_memory.lock:ps_test?mode=memory&cache=shared"

  val MSI_SQLITE_MEMORY_LOCK_FILE = "msi_sqlite_shared_memory.lock"
  val MSI_SQLITE_LINUX_FILE = "msi_sqlite_shared_memory.lock:msi_test?mode=memory&cache=shared"

  def deleteSQLiteLockFiles() {
    logger.debug("Trying to delete SQLite lock files")

    _deleteSQLiteLockFile(UDS_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(UDS_SQLITE_LINUX_FILE)

    _deleteSQLiteLockFile(PDI_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(PDI_SQLITE_LINUX_FILE)

    _deleteSQLiteLockFile(PS_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(PS_SQLITE_LINUX_FILE)

    _deleteSQLiteLockFile(MSI_SQLITE_MEMORY_LOCK_FILE)
    _deleteSQLiteLockFile(MSI_SQLITE_LINUX_FILE)
  }

  private def _deleteSQLiteLockFile(path: String) {
    assert(!StringUtils.isEmpty(path), "_deleteSQLiteLockFile() invalid path")

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

@Test
class RFImporterSQLiteJPATest extends AbstractRFImporterTestCase {

  val driverType = DriverType.SQLITE

  protected override def beforeAllTests() {}
  protected override def afterAllTests() {}
  var executionContext : IExecutionContext = _
  var rsProvider : IResultSetProvider = _

  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    RFImporterSQLiteTest.deleteSQLiteLockFiles()

    super.setUp()

    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
 
    val (execContext, rsP) = buildJPAContext
    executionContext = execContext
    rsProvider = rsP
  }

  @After
  override def tearDown() {
     if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }


  //@Test
  @Ignore
  def runRFIwithJPA() = {
   
    assertNotNull(executionContext)

    try {
      logger.debug(" --- Get File " + _datFileName)
      var datFile: File = new File(this.getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "mascot.dat",
        instrumentConfigId = 1,
        peaklistSoftwareId = 1, // TODO : provide the right value
        importerProperties = Map.empty, // TODO Use propertiedBuilder here ?
        acDecoyRegex = None)

      logger.debug(" --- run service ")
      val result = importer.runService()
      val id = importer.getTargetResultSetId
      logger.debug(" --- done " + result + " save with resultID " + id)

      assertTrue(result)
      assertNotNull(id)
      assertTrue(id > 0)

      val rsBackOp = rsProvider.getResultSet(id)
      assertTrue(rsBackOp.isDefined)
      val rsBack: ResultSet = rsBackOp.get
      assertNotNull(rsBack)


      // Other verifs....

    } finally {
      executionContext.closeAll()
    }

  }

}
