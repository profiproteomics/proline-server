package fr.proline.core.service.msi

import java.io.File

import org.junit.{Before, Ignore, Test}
import org.junit.After
import org.junit.Assert._

import fr.proline.repository.DriverType
import fr.proline.module.parser.mascot.MascotResultFileProvider

@Test
class RFImporterNewPTMTest extends AbstractRFImporterTest_ {

  val driverType = DriverType.H2
  val fileType = "MascotMSParser"

  @Before
  @throws(classOf[Exception])
  override def setUp() = {

    super.setUp()

    _datFileName = "/dat_samples/STR_F122817_Hydroxylation.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  @Test
  def testImportRFWithNewPTM() = {
    val (executionContext, rsProvider) = buildJPAContext
    val psDbCtx = executionContext.getPSDbConnectionContext()
    val psEM = psDbCtx.getEntityManager()

    assertNotNull(executionContext)

    try {
      logger.debug("--- Opening file " + _datFileName)
      val datFile = new File(this.getClass.getResource(_datFileName).toURI)
      
      logger.debug("--- Checking Result File PTM definitions")
      val mascotRFProvider = new MascotResultFileProvider()
      val ptmDefs = mascotRFProvider.getPtmDefinitions(datFile, Map.empty)
      
      assertEquals( 18, ptmDefs.length )
      assertEquals( "H(3) C(2) N O", ptmDefs(0).ptmEvidences(0).composition)
      assertEquals( "Carbamidomethyl", ptmDefs(0).names.shortName)
      
      logger.debug("--- Certifying result file")
      
      // TODO: is there a better way to do that ?
      val ptmCountBeforeCertif = psEM.createQuery("FROM fr.proline.core.orm.ps.Ptm").getResultList().size
      logger.debug(ptmCountBeforeCertif+" PTMs found in PSdb before result file certification" )
      
      val certifier = new ResultFileCertifier(
        executionContext = executionContext,
        resultIdentFilesByFormat = Map( fileType -> Array(datFile) ),
        importProperties = Map.empty
      )
      certifier.run()
      
      // TODO: is there a better way to do that ?
      val ptmCountAfterCertif = psEM.createQuery("FROM fr.proline.core.orm.ps.Ptm").getResultList().size
      logger.debug(ptmCountAfterCertif+" PTMs found in PSdb after result file certification" )
      
      assertEquals( ptmCountBeforeCertif + 1, ptmCountAfterCertif )
      
      logger.debug("--- Importing result file")
      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = datFile,
        fileType = fileType,
        instrumentConfigId = 1,
        peaklistSoftwareId = 1, // TODO : provide the right value
        importerProperties = Map.empty,
        acDecoyRegex = None
      )
      importer.run()
      
      val id = importer.getTargetResultSetId
      logger.debug(" --- Result file saved with target result set ID=" + id)

      assertTrue(id > 0)
      
    } finally {
      executionContext.closeAll()
    }

  }

}
