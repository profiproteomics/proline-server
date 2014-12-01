package fr.proline.module.fragment_match.service

import org.junit.After
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.repository.DriverType
import scala.collection.mutable.ArrayBuffer

class SpectrumMatchesGeneratorTest extends AbstractMultipleDBTestCase with Logging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "GRE_F068213_M2.4_TD_EColi"
  val targetRSId = 2
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = null
  var rsProvider: IResultSetProvider = null

  protected var generatorService : SpectrumMatchesGenerator = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    //Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")
    psDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/ps-db.xml")    
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs succesfully initialized !")

    val (execContext, rsProv) = buildSQLContext()
    executionContext = execContext
    generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, true)
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }

  def buildSQLContext() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false)
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false)
    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    val rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    (parserContext, rsProvider)
  }

  @Test
  def testGenerateSpectrumMatch() = {
    try {
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, false)
      val result = generatorService.runService
      assertTrue(result)

    } catch {

      case ex: Exception => {

        val msg = if (ex.getCause() != null) { "Error running Spectrum Matches Generator " + ex.getCause().getMessage() } else { "Error running Spectrum Matches Generator " + ex.getMessage() }
        fail(msg)

      }
    }
  }
  
    @Test
  def testGenerateExistingSpectrumMatch() = {
    try {
      
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += 348L
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, Some(pepMIds.toArray), false)
      val result = generatorService.runService
      assertTrue(result)
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, false)
      val result2 = generatorService.runService
      assertTrue(result2)

    } catch {

      case ex: Exception => {

        val msg = if (ex.getCause() != null) { "Error running Spectrum Matches Generator " + ex.getCause().getMessage() } else { "Error running Spectrum Matches Generator " + ex.getMessage() }
        fail(msg)

      }
    }
    }

    @Test
  def testForceGenerateSpectrumMatch() = {
    try {
      
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += 348L
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, Some(pepMIds.toArray), false)
      val result = generatorService.runService
      assertTrue(result)
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, true)
      val result2 = generatorService.runService
      assertTrue(result2)

    } catch {

      case ex: Exception => {

        val msg = if (ex.getCause() != null) { "Error running Spectrum Matches Generator " + ex.getCause().getMessage() } else { "Error running Spectrum Matches Generator " + ex.getMessage() }
        fail(msg)

      }
    }
  }

}