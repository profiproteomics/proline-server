package fr.proline.core.service.msi

import java.io.File

import org.junit.{Before, Ignore, Test}
import org.junit.After
import org.junit.Assert._

import fr.proline.repository.DriverType

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
    val (executionContext, rsProvider) = buildSQLContext

    val (jpaContext, ormProvider) = buildJPAContext

    assertNotNull(executionContext)

    try {
      logger.debug(" --- Get File " + _datFileName)
      var datFile: File = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporterSQLStorer(
        executionContext,
        resultIdentFile = datFile,
        fileType = "MascotMSParser",
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
      rsBack.peptideMatches.foreach(psm =>{ 
        assertTrue(psm.properties.isDefined)
        if(psm.properties.get.getMascotProperties.isDefined)
            nbrMascotProperties +=1        
      })
      assertTrue(nbrMascotProperties>0)

      // Other verifs....

      /* Reload with JPA */
//      val ormLoadedRsOp = ormProvider.getResultSet(id)
//      assertTrue(ormLoadedRsOp.isDefined)
//      val ormLoadedRs = ormLoadedRsOp.get
//      assertNotNull(ormLoadedRs)

    } finally {
      jpaContext.closeAll()

      executionContext.closeAll()
    }

  }

  @Test
  def runRFIwithJPA() = {
    val (executionContext, rsProvider) = buildJPAContext

    assertNotNull(executionContext)

    try {
      logger.debug(" --- Get File " + _datFileName)
      var datFile: File = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

      val propertiedBuilder = Map.newBuilder[String, Any]
      propertiedBuilder += ("ion.score.cutoff" -> 0.5)
      propertiedBuilder += ("subset.threshold" -> 0.5)

      val importer = new ResultFileImporterJPAStorer(
        executionContext,
        resultIdentFile = datFile,
        fileType = "MascotMSParser",
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

    } finally {
      executionContext.closeAll()
    }

  }

  @Test
  def runRFIwithDoubleJPA() = {
    var datFile: File = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.5)
    propertiedBuilder += ("subset.threshold" -> 0.5)

    /* First import */
    val (executionContext, rsProvider) = buildJPAContext

    assertNotNull(executionContext)

    var firstPeptideCount = -1L

    try {
      val initialPeptideCount = countPsPeptide(executionContext)

      logger.debug(" --- Get first File " + _datFileName)

      val importer = new ResultFileImporterJPAStorer(
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

      firstPeptideCount = countPsPeptide(executionContext)

      assertTrue("First PS Peptide creations", firstPeptideCount > initialPeptideCount)
    } finally {
      executionContext.closeAll()
    }

    /* Second import with same dat file */
    val (executionContext2, rsProvider2) = buildJPAContext

    assertNotNull(executionContext2)

    try {
      logger.debug(" --- Get second File " + _datFileName)
      datFile = new File(RFImporterH2Test.this.getClass.getResource(_datFileName).toURI)

      val importer2 = new ResultFileImporterJPAStorer(
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
      logger.debug(" --- done Second import " + result2 + " save with resultID " + id2)

      assertTrue(result2)

      assertTrue(id2 > 0)

      val secondPeptideCount = countPsPeptide(executionContext2)

      assertEquals("Second ResultSet import", firstPeptideCount, secondPeptideCount)
    } finally {
      executionContext2.closeAll()
    }

  }

}
