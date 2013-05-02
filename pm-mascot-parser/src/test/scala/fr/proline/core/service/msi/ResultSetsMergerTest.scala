package fr.proline.core.service.msi

import java.io.File

import scala.collection.mutable.ListBuffer

import org.junit.{ After, Assert }
import org.junit.{ Before, Ignore, Test }
import org.junit.Assert._

import com.weiglewilczek.slf4s.Logging

import fr.proline.context.IExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.repository.DriverType

class ResultSetsMergerTest extends AbstractRFImporterTest_ with Logging {

  val driverType = DriverType.H2
  // For manual postgres test !! If use, should comment all loadDataSet from setUp and AbstractRFImporterTest_.setUp
  //   val driverType = DriverType.POSTGRESQL 

  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    super.setUp()

    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  @Test
  def testMergeOneRS() {
    val (sqlExecutionContext, rzProvider) = buildSQLContext()

    var localJPAExecutionContext: IExecutionContext = null

    try {
      val rs1Id = importDatFile(sqlExecutionContext, "/dat_samples/STR_F136482_CTD.dat", """sp\|REV_\S+""")

      val rsIds = Seq(rs1Id, rs1Id)

      val rsMerger = new ResultSetMerger(sqlExecutionContext, loadResultSetsWithDecoy(rzProvider, rsIds))

      val result = rsMerger.runService
      Assert.assertTrue("ResultSet merger result", result)
      logger.info("End Run ResultSetMerger Service, merge same RS twice, in Test ")

      val tRSM = rsMerger.mergedResultSet
      assertNotNull(tRSM)

      val mergedDecoyRS = tRSM.decoyResultSet
      assertTrue("Merged DECOY ResultSet is present", (mergedDecoyRS != null) && mergedDecoyRS.isDefined)

      /* Try to reload merged ResultSet with JPA */
      val mergedRSId = tRSM.id

      localJPAExecutionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true)

      val rsProvider = new ORMResultSetProvider(localJPAExecutionContext.getMSIDbConnectionContext, localJPAExecutionContext.getPSDbConnectionContext, localJPAExecutionContext.getPDIDbConnectionContext)

      val optionalMergedRS = rsProvider.getResultSet(mergedRSId)
      assertTrue("Reloaded Merged ResultSet", (optionalMergedRS != null) && optionalMergedRS.isDefined)

      val optionalMergedDecoyRS = optionalMergedRS.get.decoyResultSet
      assertTrue("Reloaded Merged DECOY ResultSet", (optionalMergedDecoyRS != null) && optionalMergedDecoyRS.isDefined)
    } finally {

      if (localJPAExecutionContext != null) {
        try {
          localJPAExecutionContext.closeAll()
        } catch {
          case exClose: Exception => logger.error("Error closing local JPA ExecutionContext", exClose)
        }
      }

      if (sqlExecutionContext != null) {
        try {
          sqlExecutionContext.closeAll()
        } catch {
          case exClose: Exception => logger.error("Error closing JPA ExecutionContext", exClose)
        }
      }

    }

  }

  @Test
  def testMergeTwoRS() {
    val (sqlExecutionContext, rzProvider) = buildSQLContext()

    var localJPAExecutionContext: IExecutionContext = null

    try {
      val rs1Id = importDatFile(sqlExecutionContext, "/dat_samples/STR_F136482_CTD.dat", """sp\|REV_\S+""")
      val rs2Id = importDatFile(sqlExecutionContext, "/dat_samples/STR_F122817_Mascot_v2.3.dat", """sp\|REV_\S+""")

      val rsIds = Seq(rs1Id, rs2Id)

      val rsMerger = new ResultSetMerger(sqlExecutionContext, loadResultSetsWithDecoy(rzProvider, rsIds))

      val result = rsMerger.runService
      assertTrue("ResultSet merger result", result)
      logger.info("End Run ResultSetMerger Service, merge two different RS twice, in Test ")

      val tRSM = rsMerger.mergedResultSet
      assertNotNull(tRSM)

      val mergedDecoyRS = tRSM.decoyResultSet
      assertTrue("Merged DECOY ResultSet is present", (mergedDecoyRS != null) && mergedDecoyRS.isDefined)

      /* Try to reload merged ResultSet with JPA */
      val mergedRSId = tRSM.id

      localJPAExecutionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true)

      val rsProvider = new ORMResultSetProvider(localJPAExecutionContext.getMSIDbConnectionContext, localJPAExecutionContext.getPSDbConnectionContext, localJPAExecutionContext.getPDIDbConnectionContext)

      val optionalMergedRS = rsProvider.getResultSet(mergedRSId)
      assertTrue("Reloaded Merged ResultSet", (optionalMergedRS != null) && optionalMergedRS.isDefined)

      val optionalMergedDecoyRS = optionalMergedRS.get.decoyResultSet
      assertTrue("Reloaded Merged DECOY ResultSet", (optionalMergedDecoyRS != null) && optionalMergedDecoyRS.isDefined)
    } finally {

      if (localJPAExecutionContext != null) {
        try {
          localJPAExecutionContext.closeAll()
        } catch {
          case exClose: Exception => logger.error("Error closing local JPA ExecutionContext", exClose)
        }
      }

      if (sqlExecutionContext != null) {
        try {
          sqlExecutionContext.closeAll()
        } catch {
          case exClose: Exception => logger.error("Error closing SQL ExecutionContext", exClose)
        }
      }

    }

  }

  /* Private methods */
  private def importDatFile(localExecutionContext: IExecutionContext, datFileClassPath: String, decoyRegExp: String): Int = {
    logger.debug(" --- Load Mascot File " + datFileClassPath)

    val beforePeptideMatch = getPeptideMatchCount

    var datFile: File = new File(getClass.getResource(datFileClassPath).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.0)
    propertiedBuilder += ("subset.threshold" -> 1.0)

    val importer: ResultFileImporterSQLStorer = new ResultFileImporterSQLStorer(
      localExecutionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = Some(decoyRegExp.r))

    val result = importer.runService()
    val rsID = importer.getTargetResultSetId

    val localJPAContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true)

    val afterPeptideMatch = getPeptideMatchCount

    logger.info("TOTAL PeptideMatches created : " + (afterPeptideMatch - beforePeptideMatch))

    logger.debug("ResultFile result (" + result + ") from " + datFile.getAbsolutePath() + " with target resultID " + rsID)

    rsID
  }

  private def getPeptideMatchCount(): Long = {
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

  private def loadResultSetsWithDecoy(rsProvider: IResultSetProvider, rsIds: Seq[Int]): Seq[ResultSet] = {
    val loadedRS = ListBuffer.empty[ResultSet]

    for (rsId <- rsIds) {
      val optionalTargetRS = rsProvider.getResultSet(rsId)

      if ((optionalTargetRS == null) || optionalTargetRS.isEmpty) {
        logger.warn("TARGET ResultSet #" + rsId + " NOT found in MSI Db")
      } else {
        val targetRS = optionalTargetRS.get

        val optionalDecoyRS = targetRS.decoyResultSet
        if ((optionalDecoyRS == null) || optionalDecoyRS.isEmpty) {
          val decoyRSId = targetRS.getDecoyResultSetId

          if (decoyRSId != 0) {
            /* Force loading of DECOY ResultSet */
            val optionalLoadedDecoyRS = rsProvider.getResultSet(decoyRSId)

            if ((optionalLoadedDecoyRS == null) || optionalLoadedDecoyRS.isEmpty) {
              logger.warn("DECOY ResultSet #" + decoyRSId + " NOT found in MSI Db")
            } else {
              targetRS.decoyResultSet = Some(optionalLoadedDecoyRS.get) // Attach Decoy ResultSet entity
            }

          } else {
            logger.warn("ResultSet #" + rsId + "has no associated DECOY ResultSet")
          }

        }

        loadedRS += targetRS
      }

    }

    loadedRS.result
  }

}