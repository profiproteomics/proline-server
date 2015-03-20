package fr.proline.module.exporter.pridexml

import java.io.File

import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.BeforeClass
import org.junit.Test

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msi.filtering.IPeptideMatchFilter
import fr.proline.core.algo.msi.filtering.pepmatch.RankPSMFilter
import fr.proline.core.algo.msi.validation.BasicTDAnalyzer
import fr.proline.core.algo.msi.validation.TargetDecoyModes
import fr.proline.core.dal.AbstractEmptyDatastoreTestCase
import fr.proline.core.dbunit.GRE_F068213_M2_4_TD_EColi
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.service.msi.ResultSetValidator
import fr.proline.module.fragment_match.service.SpectrumMatchesGenerator
import fr.proline.repository.DriverType

object PrideExporterTest extends AbstractEmptyDatastoreTestCase with Logging {

  val driverType = DriverType.H2
  val useJPA = false
  lazy val rs1 = {
    val provider = new SQLResultSetProvider(executionContext.getMSIDbConnectionContext(), executionContext.getPSDbConnectionContext(), executionContext.getUDSDbConnectionContext())
    val resultSet = provider.getResultSets(Array(2))(0)
    resultSet.decoyResultSet = Some(provider.getResultSets(Array(1))(0))
    resultSet
  }

  @BeforeClass
  override def setUp() {
    super.setUp()
    loadDbUnitResultFiles(GRE_F068213_M2_4_TD_EColi)
  }

}

class PrideExporterTest extends Logging {

  @Test
  def testExport() {
    val f = new File("pridexml_test.xml")
    try {
      var rsm = validate(PrideExporterTest.executionContext, PrideExporterTest.rs1)
      generateSpectrumMatches(PrideExporterTest.executionContext, rsm.id, PrideExporterTest.rs1.id)

      val extraParams = Map.newBuilder[String, Object]
      extraParams += ("protocol_description" -> "<Protocol> \n  <ProtocolName>PRTNAME</ProtocolName> \n    <ProtocolSteps> \n <StepDescription> \n <cvParam cvLabel=\"PRIDE\" accession=\"PRIDE:0000024\" name=\"Enzyme digestion\" value=\"0\" /> \n </StepDescription> \n    </ProtocolSteps>\n </Protocol>\n")
      extraParams += ("sample_name" -> "My SAMPLE")
      extraParams += ("sample_desc" -> "the sample comment")
      val exporter = new PrideExporterService(PrideExporterTest.executionContext, rsm.id, f.getAbsolutePath(), extraParams.result)
      exporter.runService()
      assertTrue(f.exists())
      f.delete()
    } catch {
      case e: Exception => logger.error("error", e)
    }
  }

  private def generateSpectrumMatches(execContext: IExecutionContext, rsmId: Long, rsId: Long) {
    val service = new SpectrumMatchesGenerator(execContext, rsId, Some(rsmId), None, false)
    service.run
  }

  private def validate(execContext: IExecutionContext, rs: ResultSet): ResultSummary = {
    /* PeptideMatch pre-filter on Rank */
    val seqBuilder = Seq.newBuilder[IPeptideMatchFilter]
    seqBuilder += new RankPSMFilter(2) // Only 1, 2 ranks

    val rsValidator = new ResultSetValidator(
      execContext = execContext,
      targetRs = rs,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Option(seqBuilder.result),
      protSetFilters = None,
      protSetValidator = None,
      storeResultSummary = true)

    val result = rsValidator.runService

    assertTrue("Validation of RS #" + rs.id, result)

    val validatedTargetRSM = rsValidator.validatedTargetRsm
    assertNotNull("Validated Target RSM", validatedTargetRSM)

    val decoyRSMId = validatedTargetRSM.getDecoyResultSummaryId
    assertTrue("Validated Decoy RSM", decoyRSMId > 0)

    validatedTargetRSM
  }

}