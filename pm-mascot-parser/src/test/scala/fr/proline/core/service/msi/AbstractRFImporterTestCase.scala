package fr.proline.core.service.msi

import java.io.File
import javax.persistence.EntityManager

import fr.proline.context.BasicExecutionContext
import fr.proline.context.DatabaseConnectionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal._
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.repository.DriverType
import org.junit.Assert.assertTrue
import org.junit.Ignore


@Ignore
trait AbstractRFImporterTestCase extends AbstractMultipleDBTestCase   {

  protected val driverType: DriverType
  protected var _datFileName: String = "/F047876.dat"

  protected def beforeAllTests() {}
  protected def afterAllTests() {}

  this.beforeAllTests()

  @throws(classOf[Exception])
  def setUp(): Unit = {
    logger.info("Initializing Dbs")
    super.initDBsDBManagement(driverType)
    logger.info("initDBsDBManagement DONE")

    logger.info("msiDBTestCase.loadDataSet")
    msiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Init_Dataset.xml")

    logger.info("MSI db succesfully initialized")
  }


  def buildSQLContext(): (IExecutionContext, IResultSetProvider) = {
    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector(), useJPA = false)
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), useJPA = false)

    val executionContext = PeptideCacheExecutionContext(new BasicExecutionContext(1, udsDbCtx,  msiDbCtx, null))

    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(PeptideCacheExecutionContext(executionContext)))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(msiDbCtx))

    val rsProvider = new SQLResultSetProvider(PeptideCacheExecutionContext(executionContext))

    (parserContext, rsProvider)
  }

  def buildSQLContextForJPA():(IExecutionContext, IResultSetProvider) = {
    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector(), useJPA = false)
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(2), useJPA = false)

    val executionContext = BuildLazyExecutionContext(dsConnectorFactoryForTest, 1, useJPA = true) // Full JPA
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(PeptideCacheExecutionContext(parserContext)))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(msiDbCtx))

    val rsProvider = new SQLResultSetProvider(PeptideCacheExecutionContext(executionContext))

    (parserContext, rsProvider)
  }

  def buildJPAContext():(IExecutionContext, IResultSetProvider) = {
    val executionContext = BuildLazyExecutionContext(dsConnectorFactoryForTest, 1, useJPA = true) // Full JPA
    val rsProvider = new ORMResultSetProvider(executionContext.getMSIDbConnectionContext)

    (executionContext, rsProvider)
  }

  /* Protected methods */
  protected def importDatFile(localExecutionContext: IExecutionContext, datFileClassPath: String, decoyRegExp: String): Long = {
    logger.debug(" --- Load Mascot file [" + datFileClassPath + ']')

    val beforePeptideMatch = getPeptideMatchCount()

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

    val afterPeptideMatch = getPeptideMatchCount()

    logger.info("TOTAL PeptideMatches created : " + (afterPeptideMatch - beforePeptideMatch))

    rsId
  }

  protected def getPeptideMatchCount(): Long = {
    var result: Long = 0

    val msiEM = dsConnectorFactoryForTest.getMsiDbConnector(1).createEntityManager()

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

    val msiDb = executionContext.getMSIDbConnectionContext

    val con = msiDb.getConnection

    if (con != null) {
      peptideCount = countPsPeptideSQL(msiDb)
    } else if (msiDb.isJPA) {
      peptideCount = countPsPeptideJPA(msiDb.getEntityManager)
    }

    logger.debug("Found PS Peptide count : " + peptideCount)

    peptideCount
  }
  
  private def countPsPeptideSQL(dbCtx: DatabaseConnectionContext): Long = {
    ProlineEzDBC( dbCtx ).selectLong("SELECT COUNT(*) from Peptide")
  }

 
  private def countPsPeptideJPA(em: EntityManager): Long = {
    var peptideCount: Long = -1L

    val query = em.createQuery("select count(*) from fr.proline.core.orm.msi.Peptide")

    val obj = query.getSingleResult

    if (obj.isInstanceOf[java.lang.Long]) {
      peptideCount = obj.asInstanceOf[java.lang.Long].longValue
    }

    peptideCount
  }

}
