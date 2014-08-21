package fr.proline.core.service.msi

import java.sql.{ Connection, SQLException }
import org.junit.Ignore
import org.junit.Assert.assertTrue
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.{ BasicExecutionContext, DatabaseConnectionContext, IExecutionContext }
import fr.proline.core.dal.ContextFactory
import fr.proline.core.dal.ProlineEzDBC
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.{ IPTMProvider, IPeptideProvider }
import fr.proline.core.om.provider.msi.impl.{ ORMResultSetProvider, SQLPTMProvider, SQLPeptideProvider, SQLResultSetProvider }
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType
import javax.persistence.EntityManager
import java.io.File

// Note: the name of the trait ends with an underscore to indicate it must not be tested directly
@Ignore
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
    logger.info("initDBsDBManagement DONE")

    SQLPeptideProvider.clear() // Clear peptide cache between tests

    //Load Data
    logger.info("psDBTestCase.loadDataSet")
    psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
    logger.info("pdiDBTestCase.loadDataSet")
    pdiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Proteins_Dataset.xml")
    logger.info("msiDBTestCase.loadDataSet")
    msiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Init_Dataset.xml")

    logger.info("PS, PDI and MSI dbs succesfully initialized")
  }

  override def tearDown() {

    try {
      SQLPeptideProvider.clear() // Clear peptide cache between tests
    } finally {
      super.tearDown()
    }

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

  def buildSQLContextForJPA() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false)
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(2), false)

    val executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    val rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    (parserContext, rsProvider)
  }

  def buildJPAContext() = {
    val executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
    val rsProvider = new ORMResultSetProvider(executionContext.getMSIDbConnectionContext, executionContext.getPSDbConnectionContext, executionContext.getPDIDbConnectionContext)

    (executionContext, rsProvider)
  }

  /* Protected methods */
  protected def importDatFile(localExecutionContext: IExecutionContext, datFileClassPath: String, decoyRegExp: String): Long = {
    logger.debug(" --- Load Mascot file [" + datFileClassPath + ']')

    val beforePeptideMatch = getPeptideMatchCount

    val datFile = new File(getClass.getResource(datFileClassPath).toURI)
    val datAbsolutePathname = datFile.getAbsolutePath

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.0)
    propertiedBuilder += ("subset.threshold" -> 1.0)

    val importer: ResultFileImporter = new ResultFileImporter(
      localExecutionContext,
      resultIdentFile = datFile,
      fileType = "mascot.dat",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = Some(decoyRegExp.r))

    val result = importer.runService()
    assertTrue("ResultFile [" + datAbsolutePathname + "] importer service result", result)

    val rsId = importer.getTargetResultSetId

    logger.debug("ResultFile [" + datAbsolutePathname + "] imported as TARGET ResultSet Id: " + rsId)

    val afterPeptideMatch = getPeptideMatchCount

    logger.info("TOTAL PeptideMatches created : " + (afterPeptideMatch - beforePeptideMatch))

    rsId
  }

  protected def getPeptideMatchCount(): Long = {
    var result: Long = 0

    val msiEM = dsConnectorFactoryForTest.getMsiDbConnector(1).getEntityManagerFactory.createEntityManager()

    try {
      val query = msiEM.createQuery("select count(distinct pm) from fr.proline.core.orm.msi.PeptideMatch pm")

      val obj = query.getSingleResult

      if (obj.isInstanceOf[java.lang.Long]) {
        result = obj.asInstanceOf[java.lang.Long].longValue
      }

    } finally {

      if (msiEM != null) {
        try {
          msiEM.close()
        } catch {
          case exClose: Exception => logger.error("Error closing MSI EntityManager", exClose)
        }
      }

    }

    result
  }
  def countPsPeptide(executionContext: IExecutionContext): Long = {
    var peptideCount: Long = -1L

    val psDb = executionContext.getPSDbConnectionContext

    val con = psDb.getConnection

    if (con != null) {
      peptideCount = countPsPeptideSQL(psDb)
    } else if (psDb.isJPA) {
      peptideCount = countPsPeptideJPA(psDb.getEntityManager)
    }

    logger.debug("Found PS Peptide count : " + peptideCount)

    peptideCount
  }
  
  private def countPsPeptideSQL(dbCtx: DatabaseConnectionContext): Long = {
    ProlineEzDBC( dbCtx ).selectLong("SELECT COUNT(*) from Peptide")
  }

  /*private def countPsPeptideSQL(con: Connection): Long = {
    var peptideCount: Long = -1L

    val stm = con.createStatement()

    try {
      val rs = stm.executeQuery("SELECT COUNT(*) from Peptide")

      while ((peptideCount == -1L) && rs.next()) {
        val obj = rs.getObject(1) // 1st column

        if (obj.isInstanceOf[java.lang.Long]) {
          peptideCount = obj.asInstanceOf[java.lang.Long].longValue
        }

      }

      rs.close()
    } finally {

      try {
        stm.close() // Also closes current ResultSet object
      } catch {
        case exClose: SQLException => logger.error("Error closing count statement", exClose)
      }

    }

    peptideCount
  }*/

  private def countPsPeptideJPA(em: EntityManager): Long = {
    var peptideCount: Long = -1L

    val query = em.createQuery("select count(*) from fr.proline.core.orm.ps.Peptide")

    val obj = query.getSingleResult

    if (obj.isInstanceOf[java.lang.Long]) {
      peptideCount = obj.asInstanceOf[java.lang.Long].longValue
    }

    peptideCount
  }

}
