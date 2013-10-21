package fr.proline.core.service.msi

import java.io.File
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.SQLConnectionContext
import fr.proline.core.dal.SQLQueryHelper
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.orm.ps.repository.PsPtmRepository
import fr.proline.module.parser.mascot.MascotResultFileProvider
import fr.proline.repository.DriverType
import fr.proline.core.orm.ps.PtmEvidence
import scala.collection.JavaConversions._

@Test
class RFCertifierH2CTDTest extends AbstractRFImporterTest_ {
  
  val driverType = DriverType.H2
    
  @Before
  @throws( classOf[Exception] )
  override def setUp() = {
    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
      
    super.initDBsDBManagement(driverType)
    logger.info("initDBsDBManagement DONE")
    logger.info("psDBTestCase.loadDataSet")
    psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
    logger.info("pdiDBTestCase.loadDataSet")
    pdiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Proteins_Dataset.xml")
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )

  }
  
  @After
  override def tearDown() {
    super.tearDown()
  }
  
  @Test
  def testRFCertifier() = {
    val (executionContext, rsProvider) = buildJPAContext()
    ResultFileProviderRegistry.register(new MascotResultFileProvider() )
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFCertifierH2CTDTest.this.getClass.getResource( _datFileName ).toURI )
    var rfByFormat = Map("MascotMSParser" -> Array(datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any]
    )

    logger.debug( " --- run service " )
    val result = certifier.runService()

    Assert.assertTrue( result )
    executionContext.closeAll()
  }
  
    @Test
  def testRFCertifierWithMissingPtm() = {
    val (executionContext, rsProvider) = buildJPAContext()
    deletePtm(executionContext, "Carbamidomethyl")
    
    ResultFileProviderRegistry.register(new MascotResultFileProvider() )
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFCertifierH2CTDTest.this.getClass.getResource( _datFileName ).toURI )
    var rfByFormat = Map("MascotMSParser" -> Array(datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any]
    )

    logger.debug( " --- run service " )
    val result = certifier.runService()

    Assert.assertTrue( result )
    executionContext.closeAll()
  }
  
  @Test
  def testTwoRFCertifier() = {
    val (executionContext, rsProvider) = buildJPAContext()
    ResultFileProviderRegistry.register(new MascotResultFileProvider() )
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFCertifierH2CTDTest.this.getClass.getResource( _datFileName ).toURI )
    var rfByFormat = Map("MascotMSParser" -> Array(datFile, datFile))

    val certifier = new ResultFileCertifier(
      executionContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = Map.empty[String, Any]
    )

    logger.debug( " --- run service " )
    val result = certifier.runService()

    Assert.assertTrue( result )
    executionContext.closeAll()
  }
  
  def deletePtm(execCtx: IExecutionContext, ptmShortName:String) = {
	  val psEM = execCtx.getPSDbConnectionContext().getEntityManager()
	  val psPtm = PsPtmRepository.findPtmForShortName(psEM, ptmShortName)
	  psEM.getTransaction().begin()
	  val composition = psPtm.getEvidences().iterator().next().getComposition()
	  val query = psEM.createQuery("FROM fr.proline.core.orm.ps.PtmEvidence WHERE composition = :composition",classOf[PtmEvidence])
     val defs = query.setParameter("composition", composition).getResultList().toList
     psEM.remove(psPtm)
     for (e <- defs) {
       val ptm = e.getPtm()
       psEM.remove(ptm)
     }
	  psEM.getTransaction().commit()
	  
  }
  
}
