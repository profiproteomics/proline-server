package fr.proline.core.service.msi

import java.io.File

import fr.proline.context.IExecutionContext
import fr.proline.core.dbunit.JInit_Dataset
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.orm.msi.PtmEvidence
import fr.proline.core.orm.msi.repository.MsiPtmRepository
import fr.proline.core.util.ResidueUtils.characterToScalaChar
import fr.proline.module.parser.mascot.MascotResultFileProvider
import fr.proline.repository.DriverType
import org.junit.After
import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test

import scala.collection.JavaConverters._

@Test
class RFCertifierH2CTDTest extends AbstractRFImporterTestCase {

  val driverType = DriverType.H2
  var executionContext : IExecutionContext = _
 
  @Before
  @throws(classOf[Exception])
  override def setUp(): Unit = {
    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"

    super.initDBsDBManagement(driverType)
    logger.info("initDBsDBManagement DONE")
    logger.info("xxDBTestCase.loadDataSet")
    msiDBTestCase.loadDataSet(JInit_Dataset.msiDbDatasetPath)
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
    val (execContext, rsProvider) = buildJPAContext()
    executionContext = execContext
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()

    super.tearDown()
  }

  @Test
  def testRFCertifier(): Unit = {
    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    val datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    val rfByFormat = Map("mascot.dat" -> Array(datFile))

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
  def testRFCertifierWithMissingPtm(): Unit = {
    deletePtm(executionContext, "Carbamidomethyl")

    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    val datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    val rfByFormat = Map("mascot.dat" -> Array(datFile))

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
  def testRFCertifierWithMissingSpecificity(): Unit = {
    deletePtmSpecificity(executionContext, "Carbamidomethyl", 'E')
    val msiEM = executionContext.getMSIDbConnectionContext.getEntityManager
    var ptms = MsiPtmRepository.findPtmForShortName(msiEM, "Carbamidomethyl")

    assertEquals(8, ptms.getSpecificities.size())
    msiEM.detach(ptms)
    msiEM.clear()

    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    val datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    val rfByFormat = Map("mascot.dat" -> Array(datFile))

    //psEM.getTransaction().begin()
    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any])

    logger.debug(" --- run service ")
    val result = certifier.runService()
    //psEM.getTransaction().commit()

    Assert.assertTrue(result)
    ptms = MsiPtmRepository.findPtmForShortName(msiEM, "Carbamidomethyl")
    assertEquals(9, ptms.getSpecificities.size())

    executionContext.closeAll()
  }

  @Test
  def testTwoRFCertifier(): Unit = {
    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    logger.debug(" --- Get File " + _datFileName)
    val datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(_datFileName).toURI)
    val rfByFormat = Map("mascot.dat" -> Array(datFile, datFile))

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
    val msiEM = execCtx.getMSIDbConnectionContext.getEntityManager
    val msiPtm = MsiPtmRepository.findPtmForShortName(msiEM, ptmShortName)

    val psPtmSpecifs = msiPtm.getSpecificities.asScala.toList

    val psMatchingPtmSpecifOpt = psPtmSpecifs.find { psPtmSpecif =>
      characterToScalaChar(psPtmSpecif.getResidue) == residue
    }

    val ptmSpecificity = psMatchingPtmSpecifOpt.getOrElse(msiPtm.getSpecificities.iterator().next())
    
    if (ptmSpecificity.getResidue != null)
      logger.info("Test will remove specificity " + ptmShortName + " (" + ptmSpecificity.getResidue + ")")
    else
      logger.info("Test will remove specificity " + ptmShortName + " (" + ptmSpecificity.getLocation + ")")

    msiPtm.removeSpecificity(ptmSpecificity)
    msiEM.getTransaction.begin()
    msiEM.remove(ptmSpecificity)
    msiEM.getTransaction.commit()
  }

  def deletePtm(execCtx: IExecutionContext, ptmShortName: String): Unit = {
    val msiEM = execCtx.getMSIDbConnectionContext.getEntityManager
    val msiPtm = MsiPtmRepository.findPtmForShortName(msiEM, ptmShortName)
    msiEM.getTransaction.begin()

    val composition = msiPtm.getEvidences.iterator().next().getComposition
    val query = msiEM.createQuery("FROM fr.proline.core.orm.msi.PtmEvidence WHERE composition = :composition", classOf[PtmEvidence])
    val defs = query.setParameter("composition", composition).getResultList.asScala.toList
    msiEM.remove(msiPtm)
    for (e <- defs) {
      val ptm = e.getPtm
      msiEM.remove(ptm)
    }
    msiEM.getTransaction.commit()
  }

  @Test
  def testRFCertifierWithUnknwonEnzyme(): Unit = {
    ResultFileProviderRegistry.register(new MascotResultFileProvider())
    val unkownEnzyme_datFileName = "/dat_samples/F159835_unknown_enzyme.dat"
    logger.debug(" --- Get File " + unkownEnzyme_datFileName)
    val datFile: File = new File(RFCertifierH2CTDTest.this.getClass.getResource(unkownEnzyme_datFileName).toURI)
    val rfByFormat = Map("mascot.dat" -> Array(datFile))

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
