package fr.proline.core.service.msi

import java.io.File
import org.junit.{ After, AfterClass, Assert, Test, Before, BeforeClass }
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.dal._
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.impl.{ SQLPeptideProvider, SQLPTMProvider, SQLResultSetProvider, ORMSeqDatabaseProvider, ORMResultSetProvider, ORMProteinProvider, ORMPeptideProvider, ORMPTMProvider }
import fr.proline.core.om.utils.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType
import fr.proline.context.IExecutionContext

import fr.proline.context.BasicExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.IPTMProvider

// Note: the name of the trait ends with an underscore to indicate it must not be tested directly
trait AbstractRFImporterTest_ extends AbstractMultipleDBTestCase with Logging {

  protected val driverType: DriverType
  protected var _datFileName: String = "/F047876.dat"

  protected def beforeAllTests() {}
  protected def afterAllTests() {}

  this.beforeAllTests()

  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing Dbs")
    super.initDBsDBManagement(driverType)

    //Load Data
    psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
    pdiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Proteins_Dataset.xml")
    msiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Init_Dataset.xml")

    logger.info("PS, PDI and MSI dbs succesfully initialized")
  }

  override def tearDown() {
    super.tearDown()
  }

  //@Test
  def buildSQLContext() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false).asInstanceOf[SQLConnectionContext]
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false).asInstanceOf[SQLConnectionContext]
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false).asInstanceOf[SQLConnectionContext]

    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)

    val parserContext = new ProviderDecoratedExecutionContext(executionContext)

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    val rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    (parserContext, rsProvider)
  }

  //    @Test
  def buildJPAContext() = {
    val executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
    val rsProvider = new ORMResultSetProvider(executionContext.getMSIDbConnectionContext, executionContext.getPSDbConnectionContext, executionContext.getPDIDbConnectionContext)

    (executionContext, rsProvider)    
  }

}
