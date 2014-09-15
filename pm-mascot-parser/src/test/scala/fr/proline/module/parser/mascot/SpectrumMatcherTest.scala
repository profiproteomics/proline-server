package fr.proline.module.parser.mascot

import java.io.File

import scala.collection.immutable.HashMap

import org.junit.After
import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.module.fragment_match.PeptideSpectrumMatcherMascot
import fr.proline.repository.DriverType
import matrix_science.msparser.ms_fragmentationrules

@Test
class SpectrumMatcherTest extends AbstractMultipleDBTestCase with Logging {

  val driverType = DriverType.H2

  val importProperties = Map(
    "mascot.version" -> "2.3.0.1",
    "mascot.server.url" -> "http://www.matrixscience.com/cgi/" //http://tol-brandir/mascot/cgi
    )

  @Before
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

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)

    var spectrumMatchesCount: Int = 0
    var fragMatchesCount: Int = 0
    
    resultFile.eachSpectrumMatch(true, { sMatch =>
      {
        spectrumMatchesCount += 1
        fragMatchesCount += sMatch.fragMatches.length
      }

    })

    // Free memory
    resultFile.close()
    
    parserContext.closeAll()

    assertEquals("SpectrumMatches Count", 2647, spectrumMatchesCount)

  }

  @Test
  def testModFileGRE(): Unit = {
    
    val parserContext = buildParserContext()
    Assert.assertNotNull(parserContext)

    val datFileName: String = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"

    logger.info(" --- Get File " + datFileName)

    var datFile = new File(getClass.getResource(datFileName).toURI)
    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("mascot.dat")
    if (rfProvider == None) {
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")
    }

    //import com.codahale.jerkson.Json.generate
    import fr.proline.core.util.serialization.ProlineJson

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)
    var spectraById = new HashMap[Long, Spectrum]();
    var spectraIdByTitle = new HashMap[String, Long]();
    
    var fragMatchesCount: Int = 0

    var start = System.currentTimeMillis
    resultFile.eachSpectrum( spectrum => {
        spectraById += (spectrum.id -> spectrum)
        spectraIdByTitle += (spectrum.title -> spectrum.id)
      })
      
    for (msQuery <- resultFile.msQueryByInitialId.values ) {
    	msQuery.asInstanceOf[Ms2Query].spectrumId = spectraIdByTitle(msQuery.asInstanceOf[Ms2Query].spectrumTitle)
    }
    
    logger.debug("elapsed time = "+(System.currentTimeMillis - start) + "ms for collecting "+spectraById.size+" spectra")
    start = System.currentTimeMillis()
    val msmsSearchSettings = resultFile.msiSearch.searchSettings.msmsSearchSettings.get
    val sMatcher = new PeptideSpectrumMatcherMascot(spectraById, msmsSearchSettings.ms2ErrorTol, msmsSearchSettings.ms2ErrorTolUnit)
    var spectrumMatchesByQueryId = new HashMap[Pair[Int,Int], SpectrumMatch]();

    for (peptideMatch <- resultFile.getResultSet(false).peptideMatches) {
    	spectrumMatchesByQueryId += (Pair(peptideMatch.getMs2Query.initialId, peptideMatch.rank) -> sMatcher.getSpectrumMatch(peptideMatch))
    }
    logger.debug("elapsed time = "+(System.currentTimeMillis - start) + "ms for "+spectrumMatchesByQueryId.size)
    start = System.currentTimeMillis
    var spectrumMatchesByQueryId2 = new HashMap[Pair[Int,Int], SpectrumMatch]();
   
    resultFile.eachSpectrumMatch(false, { sMatch =>
      {
        spectrumMatchesByQueryId2 += (Pair(sMatch.msQueryInitialId, sMatch.peptideMatchRank) -> sMatch)
        fragMatchesCount += sMatch.fragMatches.length
      }

    })
    
    
    logger.debug("elapsed time = "+(System.currentTimeMillis - start) + "ms for "+spectrumMatchesByQueryId2.size)
    
    // Free memory
    resultFile.close()
    
    // Compare fragmentMatches
    spectrumMatchesByQueryId2.foreach( kv => {
      compareFragments(kv._2, spectrumMatchesByQueryId(kv._1))
    })
    
    parserContext.closeAll()
  }
  
  def compareFragments(expected: SpectrumMatch, actual: SpectrumMatch) = {
    assertEquals(expected.fragMatches.length, actual.fragMatches.length)
    for (expectedFragment <- expected.fragMatches) {
      val actualFragment = actual.fragMatches.find( f => (f.label.equals(expectedFragment.label)) && (f.aaPosition == expectedFragment.aaPosition))
      if (!actualFragment.isDefined) fail("Expected fragment "+expectedFragment.label+" not found")
      assertEquals(expectedFragment.moz, actualFragment.get.moz, 0.001)
    }
  }
    
    
  @Test
  def testModFileSTR(): Unit = {
   
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
    import fr.proline.core.util.serialization.ProlineJson

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)

    var spectrumMatchesCount: Int = 0
    var fragMatchesCount: Int = 0

    val rs = resultFile.eachSpectrumMatch(false, { sMatch =>
      {
        spectrumMatchesCount += 1
        fragMatchesCount += sMatch.fragMatches.length
      }

    })

    // Free memory
    resultFile.close()

    //    assertEquals("SpectrumMatches Count", EXPECTED_SPECTRUM_MATCHES_COUNT, spectrumMatchesCount)
    //    assertEquals("Calculated FragMatches Count", EXPECTED_FRAG_MATCHES_COUNT, fragMatchesCount)
    
    parserContext.closeAll()
  }

  @Test
  def testFragmentationRules(): Unit = {

    val parserContext = buildParserContext()
    val datFileName: String = "/dat_samples/STR_F136482_CTD.dat"
    var datFile = new File(getClass.getResource(datFileName).toURI)
    logger.info(" --- SpectrumMatcher  " + datFile.exists)
    new MascotResultFile(datFile, importProperties, parserContext)

    val mascotServerCGIURLAsStr = importProperties("mascot.server.url")

    this.logger.debug("Iterating over spectrum matches of result file '%s' (mascot version=%s ; server URL =%s)".format(
      datFile.getName, importProperties("mascot.version"), mascotServerCGIURLAsStr))

    val mascotConfig = new MascotRemoteConfig(importProperties("mascot.version"), mascotServerCGIURLAsStr.asInstanceOf[String])
    val mascotFragRules = mascotConfig.fragmentationRulesFile.getInstrumentByName("ESI-QUAD")
    logger.info(mascotFragRules.getTitle())
    for (series <- ms_fragmentationrules.getFirstSeries to ms_fragmentationrules.getLastSeries) {

      if (mascotFragRules.isSeriesUsed(series)) {
    	  logger.info("series = "+ms_fragmentationrules.getSeriesName(series)+ ", "+series)
      }
    }
    
    parserContext.closeAll()
  }
  
  @Test
  def testPklInputFileSTR(): Unit = {
    
    val parserContext = buildParserContext()
    Assert.assertNotNull(parserContext)

    val datFileName: String = "/dat_samples/STR_F159394_pkl_input.dat"

    logger.info(" --- Get File " + datFileName)

    var datFile = new File(getClass.getResource(datFileName).toURI)
    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("mascot.dat")
    if (rfProvider == None) {
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")
    }

    //import com.codahale.jerkson.Json.generate
    import fr.proline.core.util.serialization.ProlineJson

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, importProperties, parserContext)
    
    for (peptideMatch <- resultFile.getResultSet(false).peptideMatches) {
      val query = peptideMatch.getMs2Query
//      logger.debug("ABU "+query.spectrumTitle+" == Cmpd "+query.initialId+", +MSn("+query.moz+"), ? min ???")
      assertEquals(query.spectrumTitle, "Cmpd "+query.initialId+", +MSn("+query.moz+"), ? min")
    }

    // Free memory
    resultFile.close()
    
    parserContext.closeAll()
  }
}