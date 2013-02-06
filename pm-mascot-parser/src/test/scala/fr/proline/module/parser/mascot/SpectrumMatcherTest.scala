package fr.proline.module.parser.mascot

import java.io.File

import org.junit._

import com.weiglewilczek.slf4s.Logging

import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.dal.SQLConnectionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.utils.AbstractMultipleDBTestCase
import fr.proline.repository.DriverType

@Test
class SpectrumMatcherTest extends AbstractMultipleDBTestCase with Logging  {
  
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
    
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )
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
  def runService() : Unit = {
    
    this.setUp()
    
    val parserContext = this.buildParserContext()
    Assert.assertNotNull( parserContext )
    
    logger.info(" --- Get File "+datFileName)
    
    var datFile = new File(this.getClass.getResource(datFileName).toURI)
    
    logger.info(" --- SpectrumMatcher  "+ datFile.exists)
    
    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get( "MascotMSParser" )
    if (rfProvider == None)
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")
    
    //import com.codahale.jerkson.Json.generate
    import fr.proline.core.utils.serialization.ProlineJson
    
    // Open the result file
    val resultFile = rfProvider.get.getResultFile( datFile, importProperties, parserContext )
    
    var fragMatchesCount = 0
    val rs = resultFile.eachSpectrumMatch(true, { sMatch => 
      fragMatchesCount += sMatch.fragmentMatches.length
      //println(ProlineJson.generate(sMatch).length)
    })
    
    Assert.assertEquals(24569,fragMatchesCount)

  }

}