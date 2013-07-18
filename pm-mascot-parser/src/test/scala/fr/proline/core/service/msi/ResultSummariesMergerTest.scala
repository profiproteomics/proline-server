package fr.proline.core.service.msi

import com.weiglewilczek.slf4s.Logging
import fr.proline.repository.DriverType
import org.junit.Before
import org.junit.After
import org.junit.Test
import fr.proline.context.IExecutionContext
import org.junit.Assert._
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.algo.msi.filtering.IPeptideMatchFilter
import fr.proline.core.algo.msi.filtering.pepmatch.RankPSMFilter
import fr.proline.core.algo.msi.validation.BasicTDAnalyzer
import fr.proline.core.algo.msi.validation.TargetDecoyModes
import org.msgpack.annotation.Ignore

class ResultSummariesMergerTest extends ResultSetsMergerTest with Logging {

  @Before
  override def setUp() = {
    super.setUp()
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  @Ignore
  override def testMergeOneRS() {
    /* Do not run inherited tests */
  }

  @Ignore
  override def testMergeTwoRS() {
    /* Do not run inherited tests */
  }

  @Test
  def testMergeTwoRSM() {
    val (sqlExecutionContext, rzProvider) = buildSQLContext()

    var localJPAExecutionContext: IExecutionContext = null

    try {
      logger.debug("Importing Result Files ...")
      val rs1Id = importDatFile(sqlExecutionContext, "/dat_samples/STR_F136482_CTD.dat", """sp\|REV_\S+""")
      val rs2Id = importDatFile(sqlExecutionContext, "/dat_samples/STR_F122817_Mascot_v2.3.dat", """sp\|REV_\S+""")

      logger.debug("Validating ResultSets ...")
      val rsm1Id = validate(sqlExecutionContext, rs1Id)
      val rsm2Id = validate(sqlExecutionContext, rs2Id)

      val rsmIds = Seq(rsm1Id, rsm2Id)

      logger.debug("Merging two ResultSummaries ...")

      val rsmMerger = new ResultSummaryMerger(sqlExecutionContext, Some(rsmIds), None)

      val result = rsmMerger.runService
      assertTrue("ResultSummary merger result", result)
      logger.info("End Run ResultSummaryMerger Service, merge two different RSMs")

      val tMergedRSM = rsmMerger.mergedResultSummary
      assertNotNull("Merged TARGET ResultSet", tMergedRSM)

      val mergedDecoyRSMId = tMergedRSM.getDecoyResultSummaryId
      assertTrue("Merged DECOY ResultSummary is present", mergedDecoyRSMId > 0)

      /* Try to reload merged TARGET ResultSet with JPA */
      val mergedRSId = tMergedRSM.getResultSetId

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

  private def validate(execContext: IExecutionContext, rsId: Long): Long = {
    /* PeptideMatch pre-filter on Rank */
    val seqBuilder = Seq.newBuilder[IPeptideMatchFilter]
    seqBuilder += new RankPSMFilter(2) // Only 1, 2 ranks

    val rsValidator = ResultSetValidator(execContext,
      rsId,
      Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      Option(seqBuilder.result),
      None,
      None,
      None)

    val result = rsValidator.runService

    assertTrue("Validation of RS #" + rsId, result)

    val validatedTargetRSM = rsValidator.validatedTargetRsm
    assertNotNull("Validated Target RSM", validatedTargetRSM)

    val decoyRSMId = validatedTargetRSM.getDecoyResultSummaryId
    assertTrue("Validated Decoy RSM", decoyRSMId > 0)

    validatedTargetRSM.id
  }

}