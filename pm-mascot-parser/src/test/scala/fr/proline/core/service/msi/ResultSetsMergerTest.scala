package fr.proline.core.service.msi

import java.io.File

import scala.collection.mutable.{ HashMap, ArrayBuffer }

import org.junit.{ After, Assert, Test, Before }

import com.weiglewilczek.slf4s.Logging

import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msi.filtering.pepmatch.{ ScorePSMFilter, RankPSMFilter, _ }
import fr.proline.core.algo.msi.filtering.proteinset.{ ScoreProtSetFilter, SpecificPeptidesPSFilter }
import fr.proline.core.algo.msi.filtering.{ IPeptideMatchFilter, FilterPropertyKeys, _ }
import fr.proline.core.algo.msi.validation.pepmatch.TDPepMatchValidatorWithFDROptimization
import fr.proline.core.algo.msi.validation.proteinset.ProtSetRulesValidatorWithFDROptimization
import fr.proline.core.algo.msi.validation.{ BasicTDAnalyzer, _ }
import fr.proline.core.algo.msi.InferenceMethods
import fr.proline.core.algo.msi.scoring.ProtSetScoring
import fr.proline.core.dal.{ SQLQueryHelper, SQLConnectionContext }
import fr.proline.core.om.model.msi.{ ResultSet, PeptideMatch, FilterDescriptor }
import fr.proline.core.om.provider.msi.impl.{ SQLResultSetProvider, ORMResultSetProvider }
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.repository.DriverType

class ResultSetsMergerTest extends AbstractRFImporterTest_ with Logging {

  val driverType = DriverType.H2
  // For manual postgres test !! If use, should comment all loadDataSet from setUp and AbstractRFImporterTest_.setUp
  //   val driverType = DriverType.POSTGRESQL 

  var executionContext: IExecutionContext = null
  var rs1IDWork: Int = 0
  var rs2IDWork: Int = 0
  var rsProvider: IResultSetProvider = null

  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    super.setUp()

    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    val (execContext, rsProv) = buildSQLContext() //JPAContext() //SQLContext()    
    executionContext = execContext
    rsProvider = rsProv

  }

  private def importDatFile(datFileClassPath: String, decoyRegExp: String): Int = {
    logger.debug(" --- Load Mascot File " + datFileClassPath)

    var datFile: File = new File(getClass.getResource(datFileClassPath).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.0)
    propertiedBuilder += ("subset.threshold" -> 1.0)

    val importer: ResultFileImporterSQLStorer = new ResultFileImporterSQLStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = Some(decoyRegExp.r))

    val result = importer.runService()
    val rsID = importer.getTargetResultSetId

    logger.debug(" ResultFile  loaded (" + result + ") from " + datFile.getAbsolutePath() + " with target resultID " + rsID)
    rsID
  }

  @After
  override def tearDown() {
    if (executionContext != null)
      executionContext.closeAll()
    super.tearDown()
  }

  @Test
  def testMergeOneRS() = {

    rs1IDWork = importDatFile("/dat_samples/STR_F136482_CTD.dat", """sp\|REV_\S+""")

    val rsIDs = Seq(rs1IDWork, rs1IDWork)

    val rsMerger = ResultSetMerger(
      execContext = executionContext,
      resultSetIds = rsIDs)

    val result = rsMerger.runService
    Assert.assertTrue("ResultSet merger result", result)
    logger.info(" End Run ResultSetMerger Service, merge same RS twice, in Test ")

    val tRSM = rsMerger.mergedResultSet

    Assert.assertNotNull(tRSM)

  }

  @Test
  def testMergeTwoRS() = {

    rs1IDWork = importDatFile("/dat_samples/STR_F136482_CTD.dat", """sp\|REV_\S+""")
    rs2IDWork = importDatFile("/dat_samples/STR_F122817_Mascot_v2.3.dat", """sp\|REV_\S+""")
    val rsIDs = Seq(rs1IDWork, rs2IDWork)

    val rsMerger = ResultSetMerger(
      execContext = executionContext,
      resultSetIds = rsIDs)

    val result = rsMerger.runService
    Assert.assertTrue("ResultSet merger result", result)
    logger.info(" End Run ResultSetMerger Service, merge two different RS twice, in Test ")

    val tRSM = rsMerger.mergedResultSet

    Assert.assertNotNull(tRSM)

  }

}