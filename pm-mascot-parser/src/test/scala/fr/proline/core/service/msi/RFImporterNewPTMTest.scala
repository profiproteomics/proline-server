package fr.proline.core.service.msi

import java.io.File
import scala.collection.JavaConversions.collectionAsScalaIterable
import org.junit.{Before, Ignore, Test}
import org.junit.After
import org.junit.Assert._
import fr.proline.repository.DriverType
import fr.proline.module.parser.mascot.MascotResultFileProvider
import fr.proline.core.om.model.msi.IonTypes

@Test
class RFImporterNewPTMTest extends AbstractRFImporterTestCase {

  val driverType = DriverType.H2
  val fileType = "mascot.dat"

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
      
      // Check the number of parsed PTMs
      assertEquals( 18, ptmDefs.length )
      
      // Retrieve the Hydroxylation PTM definition specific to the Methionine residue
      val methPtmDef = ptmDefs.find( ptmDef => ptmDef.names.shortName == "Hydroxylation" && ptmDef.residue == 'M' ).get
      /*for( ev <- methPtmDef.ptmEvidences ) {
        println("type="+ev.ionType+" mono mass="+ev.monoMass + " comp="+ev.composition )
      }*/
      
      // Check the composition and the number of PTM evidences of the retrieved PTM definition
      assertEquals( "O", methPtmDef.precursorDelta.composition )
      assertEquals( 2, methPtmDef.ptmEvidences.filter( _.ionType != IonTypes.Precursor ).length )
      
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
      
      // Check we have now an additional PTM in the database
      assertEquals( ptmCountBeforeCertif + 1, ptmCountAfterCertif )
      
      // Retrieve Hydroxylation PTM
      val psPTM = psEM.createQuery(
        "FROM fr.proline.core.orm.ps.Ptm WHERE short_name='Hydroxylation'",
        classOf[fr.proline.core.orm.ps.Ptm]
      ).getSingleResult()
      
      // Check we have inserted the right number of PTM specificities
      val ptmSpecifs = psPTM.getSpecificities().toList
      assertEquals( 11, ptmSpecifs.size )
      
      // Check we have inserted the right number of PTM evidences for the Methionine specificity
      val methSpecif = ptmSpecifs.find( _.getResidue() == 'M' ).get
      /*for( ev <- methSpecif.getEvidences().toList ) {
        println("type="+ev.getType()+" mono mass="+ev.getMonoMass + " comp="+ev.getComposition )
      }*/
      assertEquals( 2, methSpecif.getEvidences().size )
      
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
