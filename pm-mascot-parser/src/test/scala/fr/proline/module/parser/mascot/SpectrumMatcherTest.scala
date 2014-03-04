package fr.proline.module.parser.mascot

import java.io.File

import org.junit.After
import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.Test

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.util.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType

@Test
class SpectrumMatcherTest extends AbstractMultipleDBTestCase with Logging {

  val driverType = DriverType.H2

  val importProperties = Map(
    "mascot.version" -> "2.3.0.1",
    "mascot.server.url" -> "http://www.matrixscience.com/cgi/" //http://tol-brandir/mascot/cgi
  )

  def setUp() = {
    logger.info("Initializing Dbs")
    super.initDBsDBManagement(driverType)

    //Load Data
    psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
    pdiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Proteins_Dataset.xml")
    msiDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Init_Dataset.xml")

    logger.info("PS, PDI and MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")
  }

  @After
  override def tearDown() {
    super.tearDown()
  }

  def buildParserContext() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false)
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false)

    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)

    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    parserContext
  }

  @Test
  def testNoModFile(): Unit = {
    setUp()

    val parserContext = buildParserContext()
    Assert.assertNotNull(parserContext)

    val datFileName: String = "/dat_samples/TLS_F027737_MTD_no_varmod.dat"

    logger.info(" --- Get File " + datFileName)

    var datFile = new File(getClass.getResource(datFileName).toURI)

    logger.info(" --- SpectrumMatcher  " + datFile.exists)

    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("mascot.dat")
    if (rfProvider == None) {
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")
    }

    //import com.codahale.jerkson.Json.generate
    import fr.proline.core.utils.serialization.ProlineJson

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)

    var spectrumMatchesCount: Int = 0
    var fragMatchesCount: Int = 0

    val rs = resultFile.eachSpectrumMatch(true, { sMatch =>
      {
        spectrumMatchesCount += 1
        fragMatchesCount += sMatch.fragMatches.length
      }

    }
    )

    // Free memory
    resultFile.close()

    assertEquals("SpectrumMatches Count", 2647, spectrumMatchesCount)
    assertEquals("Calculated FragMatches Count", 24569, fragMatchesCount)
  }

  @Test
  def testModFileGRE(): Unit = {
    setUp()

    val parserContext = buildParserContext()
    Assert.assertNotNull(parserContext)

    val datFileName: String = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"

    logger.info(" --- Get File " + datFileName)

    var datFile = new File(getClass.getResource(datFileName).toURI)

    logger.info(" --- SpectrumMatcher  " + datFile.exists)

    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("mascot.dat")
    if (rfProvider == None) {
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")
    }

    //import com.codahale.jerkson.Json.generate
    import fr.proline.core.utils.serialization.ProlineJson

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)

    var spectrumMatchesCount: Int = 0
    var fragMatchesCount: Int = 0

    val rs = resultFile.eachSpectrumMatch(false, { sMatch =>
      {
        spectrumMatchesCount += 1
        fragMatchesCount += sMatch.fragMatches.length
      }

    }
    )

    // Free memory
    resultFile.close()

    //    assertEquals("SpectrumMatches Count", EXPECTED_SPECTRUM_MATCHES_COUNT, spectrumMatchesCount)
    //    assertEquals("Calculated FragMatches Count", EXPECTED_FRAG_MATCHES_COUNT, fragMatchesCount)
  }

  @Test
  def testModFileSTR(): Unit = {
    setUp()

    val parserContext = buildParserContext()
    Assert.assertNotNull(parserContext)

    val datFileName: String = "/dat_samples/STR_F136482_CTD.dat"

    logger.info(" --- Get File " + datFileName)

    var datFile = new File(getClass.getResource(datFileName).toURI)

    logger.info(" --- SpectrumMatcher  " + datFile.exists)

    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("mascot.dat")
    if (rfProvider == None) {
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")
    }

    //import com.codahale.jerkson.Json.generate
    import fr.proline.core.utils.serialization.ProlineJson

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)

    var spectrumMatchesCount: Int = 0
    var fragMatchesCount: Int = 0

    val rs = resultFile.eachSpectrumMatch(false, { sMatch =>
      {
        spectrumMatchesCount += 1
        fragMatchesCount += sMatch.fragMatches.length
      }

    }
    )

    // Free memory
    resultFile.close()

    //    assertEquals("SpectrumMatches Count", EXPECTED_SPECTRUM_MATCHES_COUNT, spectrumMatchesCount)
    //    assertEquals("Calculated FragMatches Count", EXPECTED_FRAG_MATCHES_COUNT, fragMatchesCount)
  }

}