package fr.proline.core.service.msi

import java.io.File
import java.sql.Connection

import org.junit.{ After, Assert }
import org.junit.{ Before, Test }
import org.junit.Assert._

import fr.proline.context.IExecutionContext
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.repository.DriverType
import fr.proline.repository.util.JDBCReturningWork

@Test
class RFImporterH2Test extends AbstractRFImporterTest_ {

  val driverType = DriverType.H2

  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    super.setUp()
    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
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
    var datFile: File = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)

    val importer: ResultFileImporterSQLStorer = new ResultFileImporterSQLStorer(
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

    // Other verifs....

    executionContext.closeAll()

  }

  @Test
  def runRFIwithJPA() = {
    val (executionContext, rsProvider) = buildJPAContext()

    Assert.assertNotNull(executionContext)

    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)

    val importer: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
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

    // Other verifs....

    executionContext.closeAll()
  }

  @Test
  def runRFIwithDoubleJPA() = {
    /* First import */
    val (executionContext, rsProvider) = buildJPAContext()

    assertNotNull(executionContext)

    val initialPeptideCount = countPsPeptide(executionContext)

    logger.debug(" --- Get first File " + _datFileName)
    var datFile: File = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)

    val importer: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = None)

    logger.debug(" --- run first service ")
    val result = importer.runService()
    val id = importer.getTargetResultSetId
    logger.debug(" --- done First import " + result + " save with resultID " + id)

    assertTrue(result)

    assertTrue(id > 0)

    val rsBackOp = rsProvider.getResultSet(id)
    assertTrue(rsBackOp.isDefined)
    val rsBack: ResultSet = rsBackOp.get
    assertNotNull(rsBack)

    val firstPeptideCount = countPsPeptide(executionContext)

    assertTrue("First PS Peptide creations", firstPeptideCount > initialPeptideCount)

    executionContext.closeAll()

    /* Second import with same dat file */
    val (executionContext2, rsProvider2) = buildJPAContext

    assertNotNull(executionContext2)

    logger.debug(" --- Get second File " + _datFileName)
    datFile = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

    val importer2: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
      executionContext2,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = None)

    logger.debug(" --- run second service ")
    val result2 = importer2.runService()
    val id2 = importer2.getTargetResultSetId
    logger.debug(" --- done Second import " + result2 + " save with resultID " + id)

    assertTrue(result2)

    assertTrue(id2 > 0)

    val secondPeptideCount = countPsPeptide(executionContext2)

    assertEquals("Second ResultSet import", firstPeptideCount, secondPeptideCount)

    executionContext2.closeAll()
  }

  private def countPsPeptide(executionContext: IExecutionContext): Long = {

    val sqlWork = new JDBCReturningWork[Long]() {

      override def execute(con: Connection): Long = {
        var peptideCount: Long = -1L

        val stat = con.prepareStatement("SELECT COUNT(*) from Peptide")

        val rs = stat.executeQuery()

        while ((peptideCount == -1L) && rs.next()) {
          val obj = rs.getObject(1) // 1st column

          if (obj.isInstanceOf[java.lang.Long]) {
            peptideCount = obj.asInstanceOf[java.lang.Long].longValue

            logger.debug("Peptides in PS : " + peptideCount)
          }

        }

        rs.close()

        stat.close()

        peptideCount
      }

    }

    logger.debug("Try to retrieve Peptides count from PS")

    val psDb = executionContext.getPSDbConnectionContext

    val transaction = psDb.getEntityManager.getTransaction
    transaction.begin()

    val result = executionContext.getPSDbConnectionContext.doReturningWork(sqlWork, false)

    transaction.commit()

    result
  }

}
