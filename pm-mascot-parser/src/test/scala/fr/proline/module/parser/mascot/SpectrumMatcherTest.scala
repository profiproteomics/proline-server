package fr.proline.module.parser.mascot

import java.io.File
import java.util.Locale

import org.junit.{ Assert, Test }

import com.weiglewilczek.slf4s.Logging

import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.{ ContextFactory, SQLConnectionContext }
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.{ IPTMProvider, IPeptideProvider, ResultFileProviderRegistry }
import fr.proline.core.om.provider.msi.impl.{ SQLPTMProvider, SQLPeptideProvider }
import fr.proline.core.om.utils.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType

@Test
class SpectrumMatcherTest extends AbstractMultipleDBTestCase with Logging {

  val driverType = DriverType.H2
  var datFileName: String = "/dat_samples/TLS_F027737_MTD_no_varmod.dat"
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

  def buildParserContext() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false).asInstanceOf[SQLConnectionContext]
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false).asInstanceOf[SQLConnectionContext]
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false).asInstanceOf[SQLConnectionContext]

    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)

    val parserContext = new ProviderDecoratedExecutionContext(executionContext)

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    parserContext
  }

  @Test
  def runService(): Unit = {
    setUp()

    val parserContext = buildParserContext()
    Assert.assertNotNull(parserContext)

    logger.info(" --- Get File " + datFileName)

    var datFile = new File(getClass.getResource(datFileName).toURI)

    logger.info(" --- SpectrumMatcher  " + datFile.exists)

    // Get Right ResultFile provider
    val oldLocale = Locale.getDefault

    try {
      logger.info("Setting ENGLISH Locale")
      Locale.setDefault(Locale.ENGLISH)

      val rfProvider = ResultFileProviderRegistry.get("MascotMSParser")
      if (rfProvider == None)
        throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")

      //import com.codahale.jerkson.Json.generate
      import fr.proline.core.utils.serialization.ProlineJson

      // Open the result file
      val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)

      var fragMatchesCount = 0
      val rs = resultFile.eachSpectrumMatch(true, { sMatch =>
        fragMatchesCount += sMatch.fragmentMatches.length
        //println(ProlineJson.generate(sMatch).length)
      })

      Assert.assertEquals(24569, fragMatchesCount)

    } finally {
      logger.info("Resetting locale to " + oldLocale)
      Locale.setDefault(oldLocale)
    }

  }

}