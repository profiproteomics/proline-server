package fr.proline.core.service.msi

import java.io.File

import org.junit._
import org.junit.After
import org.junit.Assert.{assertNotNull, assertTrue}
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.repository.DriverType
import javax.persistence.FlushModeType

import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.PeptideCacheExecutionContext

@Test /* Manual PostgreSQL Test */
class RFImporterPgTest extends AbstractRFImporterTestCase {

  val driverType = DriverType.POSTGRESQL
 var executionContext : IExecutionContext = _
  var rsProvider : IResultSetProvider = _
  
  @Before
  @throws(classOf[Exception])
  override def setUp(): Unit = {

    super.setUp()

    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    updateMsiPeptideSequence()
    val (execContext, rsP) = buildJPAContext()
    executionContext = execContext
    rsProvider = rsP
  }

  @After
  override def tearDown() {
    if (executionContext != null) {
      PeptideCacheExecutionContext(executionContext).getPeptideCache().clear()
      executionContext.closeAll()
    }
    super.tearDown()
  }

  @Ignore /* Manual PostgreSQL Test */
  def testRFIwithSQL(): Unit = {

    assertNotNull(executionContext)

    try {
      logger.debug(" --- Get File " + _datFileName)
      val datFile: File = new File(getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "mascot.dat",
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

  @Ignore  /* Manual PostgreSQL Test */
  def runRFIwithJPA(): Unit = {

    assertNotNull(executionContext)

    try {
      val msiEm = executionContext.getMSIDbConnectionContext.getEntityManager

      logger.info("EntityManager flush mode : " + msiEm.getFlushMode)

      msiEm.setFlushMode(FlushModeType.COMMIT)

      logger.debug(" --- Get File " + _datFileName)
      val datFile: File = new File(getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "mascot.dat",
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

  private def updateMsiPeptideSequence() {
    val msiDbConnector = msiDBTestCase.getConnector

    val con = msiDbConnector.getDataSource.getConnection

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
