package fr.proline.core.service.msi

import java.io.File

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Ignore
import org.junit.Test

import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.repository.DriverType

@Test
class RFImporterH2JPATest extends AbstractRFImporterTestCase {

  val driverType = DriverType.H2
  var executionContext : IExecutionContext = _
  var rsProvider : IResultSetProvider = _

  @Before
  @throws(classOf[Exception])
  override def setUp() = {

    super.setUp()

    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
    msiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
    logger.info("MSI db succesfully initialized")
     val (execContext, rsPr) = buildJPAContext
    executionContext = execContext
    rsProvider = rsPr
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }


  @Test
  def testRFIwithJPA() = {
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
        importerProperties = Map.empty, // TODO use propertiedBuilder here ?
        acDecoyRegex = None)

      logger.debug(" --- run service ")
      val result = importer.runService()
      val id = importer.getTargetResultSetId
      logger.debug(" --- done " + result + " save with resultID " + id)

      assertTrue(result)

      assertTrue(id > 0)

      val rsBackOp = rsProvider.getResultSet(id)
      assertTrue(rsBackOp.isDefined)
      val rsBack = rsBackOp.get
      assertNotNull(rsBack)

      // Other verifs....

      val msiEM = executionContext.getMSIDbConnectionContext.getEntityManager

      val query = msiEM.createQuery("select count (*) from fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap")

      var count: Long = -1
      val countObj = query.getSingleResult()

      if (countObj.isInstanceOf[java.lang.Long]) {
        count = countObj.asInstanceOf[java.lang.Long].longValue
      }

      assertTrue("Number of ProteinMatchSeqDatabaseMap created", count > 0L)
      logger.debug("Number of ProteinMatchSeqDatabaseMap created: " + count)
    } finally {
      executionContext.closeAll()
    }

  }

  @Test
  def testRFIwithDoubleJPA() = {
    var datFile: File = new File(this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)


    assertNotNull(executionContext)

    var firstPeptideCount = -1L

      val initialPeptideCount = countPsPeptide(executionContext)

      logger.debug(" --- Get first File " + _datFileName)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "mascot.dat",
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

      firstPeptideCount = countPsPeptide(executionContext)

      assertTrue("First PS Peptide creations", firstPeptideCount > initialPeptideCount)
    
    /* Second import with same dat file */

      logger.debug(" --- Get second File " + _datFileName)
      datFile = new File(this.getClass.getResource(_datFileName).toURI)

      val importer2 = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = "mascot.dat",
        instrumentConfigId = 1,
        peaklistSoftwareId = 1, // TODO : provide the right value
        importerProperties = propertiedBuilder.result,
        acDecoyRegex = None)

      logger.debug(" --- run second service ")
      val result2 = importer2.runService()
      val id2 = importer2.getTargetResultSetId
      logger.debug(" --- done Second import " + result2 + " save with resultID " + id2)

      assertTrue(result2)

      assertTrue(id2 > 0)

      val secondPeptideCount = countPsPeptide(executionContext)

      assertEquals("Second ResultSet import", firstPeptideCount, secondPeptideCount)

  }

}
