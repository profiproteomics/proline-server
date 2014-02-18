package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.util.AbstractMultipleDBTestCase
import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.model.msi.SpectrumTitleParsingRule
import fr.proline.core.om.provider.uds.impl.{ SQLPeaklistSoftwareProvider => UdsSQLPklSoftProvider }
import fr.proline.core.orm.uds.{ SpectrumTitleParsingRule => UdsSpecTitleParsingRule }
import fr.proline.core.om.util.AbstractMultipleDBTestCase
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.repository.DriverType
import org.junit.Assert
import javax.persistence.EntityManager

trait AbstractResultFileImporterTest extends AbstractMultipleDBTestCase with Logging {

  private val driverType = DriverType.H2
  private var parserContext: ProviderDecoratedExecutionContext = null
//  def getParserContext = parserContext
  private var executionContext: BasicExecutionContext = null
  def getExecutionContext = executionContext
  private var rsProvider: SQLResultSetProvider = null
  def getResultSet(rsId: Long): Option[ResultSet] = rsProvider.getResultSet(rsId)
//  private var parsingRules: SpectrumTitleParsingRule = null
//  def getParsingRules = Some(parsingRules)
  private var udsEM: EntityManager = null
  
  def init() {
    logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    logger.info("Initializing Dbs")
    psDBTestCase.loadDataSet("/datasets/Unimod_Dataset.xml")
    pdiDBTestCase.loadDataSet("/datasets/Proteins_Dataset.xml")
    msiDBTestCase.loadDataSet("/datasets/Init_Dataset.xml")

    logger.info("PS, PDI and MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/datasets/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true) // default: true
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, true) // default: false
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

    executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)

    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))
    
    rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    Assert.assertNotNull(parserContext)

    udsEM = udsDbCtx.getEntityManager()
  }
  
  def getParsingRules(parsingRuleId: Long): Option[SpectrumTitleParsingRule] = {
	val udsParsingRule = udsEM.find(classOf[UdsSpecTitleParsingRule], parsingRuleId)
    val parsingRules = new SpectrumTitleParsingRule(
      id = udsParsingRule.getId,
      rawFileNameRegex = Option(udsParsingRule.getRawFileName),
      firstCycleRegex = Option(udsParsingRule.getFirstCycle),
      lastCycleRegex = Option(udsParsingRule.getLastCycle),
      firstScanRegex = Option(udsParsingRule.getFirstScan),
      lastScanRegex = Option(udsParsingRule.getLastScan),
      firstTimeRegex = Option(udsParsingRule.getFirstTime),
      lastTimeRegex = Option(udsParsingRule.getLastTime)
    )
  	Some(parsingRules)
  }
  
  def closeResources() {
    logger.debug("Closing resources")
    executionContext.closeAll()
    super.tearDown
//    executionContext = null
//    rsProvider = null
  }
}
