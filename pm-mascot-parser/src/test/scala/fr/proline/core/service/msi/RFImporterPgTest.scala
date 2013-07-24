package fr.proline.core.service.msi

import java.io.File
import org.junit._
import org.junit.After
import org.junit.Assert.{ assertNotNull, assertTrue }
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.repository.DriverType
import javax.persistence.FlushModeType

@Test /* Manual PostgreSQL Test */
class RFImporterPgTest extends AbstractRFImporterTest_ {

  val driverType = DriverType.POSTGRESQL

  @Before
  @throws(classOf[Exception])
  override def setUp() = {

    super.setUp()

    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    updatePsPeptideSequence()
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  @Ignore
  def testRFIwithSQL() = {
    val (executionContext, rsProvider) = buildSQLContext

    assertNotNull(executionContext)

    try {
      logger.debug(" --- Get File " + _datFileName)
      var datFile: File = new File(getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "MascotMSParser",
        instrumentConfigId = 1,
        peaklistSoftwareId = 1, // TODO : provide the right value
        importerProperties = propertiedBuilder.result,
        acDecoyRegex = None)

      logger.debug(" --- run service ")
      val result = importer.runService()
      val id = importer.getTargetResultSetId
      logger.debug(" --- done " + result + " save with resultID " + id)

      assertTrue(result)

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

  @Ignore
  def runRFIwithJPA() = {
    val (executionContext, rsProvider) = buildJPAContext

    assertNotNull(executionContext)

    try {
      val msiEm = executionContext.getMSIDbConnectionContext.getEntityManager

      logger.info("EntityManager flush mode : " + msiEm.getFlushMode)

      msiEm.setFlushMode(FlushModeType.COMMIT)

      logger.debug(" --- Get File " + _datFileName)
      var datFile: File = new File(getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "MascotMSParser",
        instrumentConfigId = 1,
        peaklistSoftwareId = 1, // TODO : provide the right value
        importerProperties = propertiedBuilder.result,
        acDecoyRegex = None)

      logger.debug(" --- run service ")
      val result = importer.runService()
      val id = importer.getTargetResultSetId
      logger.debug(" --- done " + result + " save with resultID " + id)

      assertTrue(result)

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

  private def updatePsPeptideSequence() {
    val psDbConnector = psDBTestCase.getConnector

    val con = psDbConnector.getDataSource.getConnection

    try {
      val stm = con.createStatement()

      val rs = stm.executeQuery("SELECT setval(\'peptide_id_seq\', 1 + (SELECT max(id) FROM peptide) )")

      if (rs.next()) {
        logger.debug("Peptide Sequence new value : " + rs.getObject(1))
      }

      rs.close()

      stm.close()
    } catch {
      case ex: Exception => logger.error("Error error updating PS Peptide Sequence", ex)

    } finally {

      if (con != null) {
        try {
          con.close()
        } catch {
          case exClose: Exception => logger.error("Error closing PS Db SQL Connection", exClose)
        }
      }

    }

  }

}
