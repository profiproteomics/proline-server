package fr.proline.core.service.msi

import java.io.File

import org.junit.{ Before, Ignore, Test }
import org.junit.After
import org.junit.Assert._

import fr.proline.repository.DriverType

@Test
class RFImporterH2SQLTest extends AbstractRFImporterTestCase {

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
    val (executionContext, rsProvider) = buildSQLContext

    val (jpaContext, ormProvider) = buildJPAContext

    assertNotNull(executionContext)

    try {
      logger.debug(" --- Get File " + _datFileName)
      var datFile: File = new File(RFImporterH2SQLTest.this.getClass.getResource(_datFileName).toURI)

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

      var nbrMascotProperties = 0
      rsBack.peptideMatches.foreach(psm => {
        assertTrue(psm.properties.isDefined)
        if (psm.properties.get.getMascotProperties.isDefined)
          nbrMascotProperties += 1
      })
      assertTrue(nbrMascotProperties > 0)

      // Other verifs....

      /* Reload with JPA */
      val ormLoadedRsOp = ormProvider.getResultSet(id)
      assertTrue(ormLoadedRsOp.isDefined)
      val ormLoadedRs = ormLoadedRsOp.get
      assertNotNull(ormLoadedRs)

      nbrMascotProperties = 0
      ormLoadedRs.peptideMatches.foreach(psm => {
        assertTrue(psm.properties.isDefined)
        if (psm.properties.get.getMascotProperties.isDefined)
          nbrMascotProperties += 1
      })
      assertTrue(nbrMascotProperties > 0)

    } finally {
      jpaContext.closeAll()

      executionContext.closeAll()
    }

  }

}
