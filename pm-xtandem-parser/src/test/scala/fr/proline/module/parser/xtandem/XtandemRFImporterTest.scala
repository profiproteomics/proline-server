package fr.proline.module.parser.xtandem

import org.junit.Test
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType
import org.junit.Before
import org.junit.Assert._
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.service.msi.ResultFileImporter
import java.io.File

@Test
class XtandemRFImporterTest extends AbstractMultipleDBTestCase {

  val driverType = DriverType.H2
  var _xtandemFileName = "--- "
  var parserContext: ProviderDecoratedExecutionContext = null
  _xtandemFileName = "/xtandemResultFile/output.2014_11_18_11_22_40.t.xml"

  @Before
  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing Dbs")
    super.initDBsDBManagement(driverType)

    SQLPeptideProvider.clear() // Clear peptide cache between tests

    //Load Data
    psDBTestCase.loadDataSet("/default_datasets/Unimod_Dataset.xml")
    pdiDBTestCase.loadDataSet("/default_datasets/Proteins_Dataset.xml")
    msiDBTestCase.loadDataSet("/default_datasets/Init_Dataset.xml")
    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    logger.info("Dbs succesfully initialized")

  }

  def buildJPAContext() = {
    val executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
    val rsProvider = new ORMResultSetProvider(executionContext.getMSIDbConnectionContext, executionContext.getPSDbConnectionContext, executionContext.getPDIDbConnectionContext)
    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(executionContext.getPSDbConnectionContext()))
    assertNotNull(parserContext)
    (executionContext, rsProvider)
  }

  override def tearDown() {

    try {
      SQLPeptideProvider.clear() // Clear peptide cache between tests
    } finally {
      super.tearDown()
    }

  }

  @Test
  def runRFImpoter() = {
    val (executionContext, rsProvider) = buildJPAContext

    assertNotNull(executionContext)

    try {

      logger.debug(" --- Get File " + _xtandemFileName)
      var identFile: File = new File(this.getClass.getResource(_xtandemFileName).toURI)

      val importer = new ResultFileImporter(
        executionContext,
        resultIdentFile = identFile,
        fileType = "xtandem.xml",
        instrumentConfigId = 1,
        peaklistSoftwareId = 1, // TODO : provide the right value
        importerProperties = Map.empty,
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

      val msiEM = executionContext.getMSIDbConnectionContext.getEntityManager

      val query = msiEM.createQuery("select count (*) from fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap")

      var count: Long = -1
      val countObj = query.getSingleResult()

      if (countObj.isInstanceOf[java.lang.Long]) {
        count = countObj.asInstanceOf[java.lang.Long].longValue
      }

      assertTrue("Number of ProteinMatchSeqDatabaseMap created", count > 0L)
      logger.debug("Number of ProteinMatchSeqDatabaseMap created: " + count)
    } finally {
      executionContext.closeAll()
    }

  }

}