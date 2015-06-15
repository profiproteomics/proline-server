package fr.proline.core.service.msi

import java.io.File
import org.junit.After
import org.junit.Assert
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.orm.ps.repository.PsPtmRepository
import fr.proline.module.parser.mascot.MascotResultFileProvider
import fr.proline.repository.DriverType
import fr.proline.core.orm.ps.PtmEvidence
import scala.collection.JavaConversions._
import fr.proline.core.orm.ps.PtmSpecificity
import fr.proline.core.util.ResidueUtils._
import org.junit.Ignore

@Test
class RFCertifierH2CTDTest extends AbstractRFImporterTestCase {

  val driverType = DriverType.H2
  var executionContext : IExecutionContext = _
 
  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"

    super.initDBsDBManagement(driverType)
    logger.info("initDBsDBManagement DONE")
    logger.info("psDBTestCase.loadDataSet")
    psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
    logger.info("pdiDBTestCase.loadDataSet")
    pdiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Proteins_Dataset.xml")
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
    val (execContext, rsProvider) = buildJPAContext
    executionContext = execContext
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  @Test
  def testRFCertifier() = {
    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    var rfByFormat = Map("mascot.dat" -> Array(datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any])

    logger.debug(" --- run service ")
    val result = certifier.runService()

    Assert.assertTrue(result)
    executionContext.closeAll()
  }

  @Test
  def testRFCertifierWithMissingPtm() = {
    deletePtm(executionContext, "Carbamidomethyl")

    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    var rfByFormat = Map("mascot.dat" -> Array(datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any])

    logger.debug(" --- run service ")
    val result = certifier.runService()

    Assert.assertTrue(result)
    executionContext.closeAll()
  }

  @Test
  def testRFCertifierWithMissingSpecificity() = {
    val psEM = executionContext.getPSDbConnectionContext().getEntityManager()
    deletePtmSpecificity(executionContext, "Carbamidomethyl", 'E')
    var psPtm = PsPtmRepository.findPtmForShortName(psEM, "Carbamidomethyl")

    assertEquals(8, psPtm.getSpecificities().size())
    psEM.detach(psPtm)
    psEM.clear()

    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    var rfByFormat = Map("mascot.dat" -> Array(datFile))

    //psEM.getTransaction().begin()
    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any])

    logger.debug(" --- run service ")
    val result = certifier.runService()
    //psEM.getTransaction().commit()

    Assert.assertTrue(result)
    psPtm = PsPtmRepository.findPtmForShortName(psEM, "Carbamidomethyl")
    assertEquals(9, psPtm.getSpecificities().size())

    executionContext.closeAll()
  }

  @Test
  def testTwoRFCertifier() = {
    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    var datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    var rfByFormat = Map("mascot.dat" -> Array(datFile, datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any])

    logger.debug(" --- run service ")
    val result = certifier.runService()

    Assert.assertTrue(result)
    executionContext.closeAll()
  }

  def deletePtmSpecificity(execCtx: IExecutionContext, ptmShortName: String, residue: Char) {
    val psEM = execCtx.getPSDbConnectionContext().getEntityManager()
    var psPtm = PsPtmRepository.findPtmForShortName(psEM, ptmShortName)

    val psPtmSpecifs = psPtm.getSpecificities().toList

    val psMatchingPtmSpecifOpt = psPtmSpecifs.find { psPtmSpecif =>
      characterToScalaChar(psPtmSpecif.getResidue) == residue
    }

    val ptmSpecificity = psMatchingPtmSpecifOpt.getOrElse(psPtm.getSpecificities().iterator().next())
    
    if (ptmSpecificity.getResidue() != null)
      logger.info("Test will remove specificity " + ptmShortName + " (" + ptmSpecificity.getResidue() + ")")
    else
      logger.info("Test will remove specificity " + ptmShortName + " (" + ptmSpecificity.getLocation() + ")")

    psPtm.removeSpecificity(ptmSpecificity)
    psEM.getTransaction().begin()
    psEM.remove(ptmSpecificity)
    psEM.getTransaction().commit()
  }

  def deletePtm(execCtx: IExecutionContext, ptmShortName: String) = {
    val psEM = execCtx.getPSDbConnectionContext().getEntityManager()
    val psPtm = PsPtmRepository.findPtmForShortName(psEM, ptmShortName)
    psEM.getTransaction().begin()

    val composition = psPtm.getEvidences().iterator().next().getComposition()
    val query = psEM.createQuery("FROM fr.proline.core.orm.ps.PtmEvidence WHERE composition = :composition", classOf[PtmEvidence])
    val defs = query.setParameter("composition", composition).getResultList().toList
    psEM.remove(psPtm)
    for (e <- defs) {
      val ptm = e.getPtm()
      psEM.remove(ptm)
    }
    psEM.getTransaction().commit()
  }

  @Test
  def testRFCertifierWithUnknwonEnzyme() = {
    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    val unkownEnzyme_datFileName = "/dat_samples/F159835_unknown_enzyme.dat"
    logger.debug(" --- Get File " + unkownEnzyme_datFileName)
    var datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(unkownEnzyme_datFileName).toURI)
    var rfByFormat = Map("mascot.dat" -> Array(datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any])

    logger.debug(" --- run service ")
    val result = certifier.runService()

    Assert.assertTrue(result)
    executionContext.closeAll()
  }

}
