package fr.proline.module.exporter.pridexml

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msi.InferenceMethod
import fr.proline.core.algo.msi.filtering.IPeptideMatchFilter
import fr.proline.core.algo.msi.filtering.pepmatch.PrettyRankPSMFilter
import fr.proline.core.algo.msi.validation.{TDAnalyzerBuilder, TargetDecoyAnalyzers}
import fr.proline.core.dal.AbstractDatastoreTestCase
import fr.proline.core.dbunit.GRE_F068213_M2_4_TD_EColi
import fr.proline.core.om.model.msi.{ResultSet, ResultSummary}
import fr.proline.core.service.msi.{ResultSetValidator, ValidationConfig}
import fr.proline.module.fragmentmatch.service.SpectrumMatchesGenerator
import fr.proline.repository.DriverType
import org.junit.Assert.{assertNotNull, assertTrue}
import org.junit.Test

object PrideExporterTest extends AbstractDatastoreTestCase {

  override val driverType = DriverType.H2
  override val dbUnitResultFile = GRE_F068213_M2_4_TD_EColi
  override val useJPA: Boolean = false

  val targetRSId: Long = 2L
  val decoyRSId: Option[Long] = Some(1L)

}

class PrideExporterTest extends LazyLogging {

  @Test
  def testExport() {
    val f = new File("pridexml_test.xml")

    val rsm = validate(PrideExporterTest.executionContext, PrideExporterTest.getRS(PrideExporterTest.targetRSId, PrideExporterTest.decoyRSId))
    generateSpectrumMatches(PrideExporterTest.executionContext, rsm.id, PrideExporterTest.targetRSId)

    val extraParams = Map.newBuilder[String, Object]
    extraParams += ("protocol_description" -> "<Protocol> \n  <ProtocolName>PRTNAME</ProtocolName> \n    <ProtocolSteps> \n <StepDescription> \n <cvParam cvLabel=\"PRIDE\" accession=\"PRIDE:0000024\" name=\"Enzyme digestion\" value=\"0\" /> \n </StepDescription> \n    </ProtocolSteps>\n </Protocol>\n")
    extraParams += ("sample_name" -> "My SAMPLE")
    extraParams += ("sample_desc" -> "the sample comment")
    extraParams += ("contact_name" -> "John Doo")
    extraParams += ("contact_institution" -> "Proline")
    val exporter = new PrideExporterService(PrideExporterTest.executionContext, rsm.id, f.getAbsolutePath, extraParams.result)
    exporter.runService()
    assertTrue(f.exists())
    f.delete()
    
  }

  private def generateSpectrumMatches(execContext: IExecutionContext, rsmId: Long, rsId: Long) {
    val service = new SpectrumMatchesGenerator(execContext, rsId, Some(rsmId), None,None, false)
    service.run
  }

  private def validate(execContext: IExecutionContext, rs: ResultSet): ResultSummary = {
    /* PeptideMatch pre-filter on Rank */
    val seqBuilder = Seq.newBuilder[IPeptideMatchFilter]
    seqBuilder += new PrettyRankPSMFilter(2) // Only 1, 2 ranks
    val tdAnalyzerBuilder = new TDAnalyzerBuilder(TargetDecoyAnalyzers.BASIC)

    val rsValidator =  ResultSetValidator(
      execContext = execContext,
      targetRs = rs,
      validationConfig = ValidationConfig(tdAnalyzerBuilder = Some(tdAnalyzerBuilder), pepMatchPreFilters = Option(seqBuilder.result)),
      inferenceMethod = Some(InferenceMethod.PARSIMONIOUS),
      storeResultSummary = true,
      propagatePepMatchValidation = false,
      propagateProtSetValidation = false)

    val result = rsValidator.runService()

    assertTrue("Validation of RS #" + rs.id, result)

    val validatedTargetRSM = rsValidator.validatedTargetRsm
    assertNotNull("Validated Target RSM", validatedTargetRSM)

    val decoyRSMId = validatedTargetRSM.getDecoyResultSummaryId
    assertTrue("Validated Decoy RSM", decoyRSMId > 0)

    validatedTargetRSM
  }

}