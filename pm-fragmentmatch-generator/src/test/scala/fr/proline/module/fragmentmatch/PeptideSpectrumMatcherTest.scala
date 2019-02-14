package fr.proline.module.fragmentmatch

import com.typesafe.scalalogging.StrictLogging
import fr.profi.util.serialization.CustomDoubleJacksonSerializer
import fr.profi.util.serialization.ProfiJSMSerialization
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.BuildMsiDbConnectionContext
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.om.model.msi.FragmentationRuleSet
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideMatchProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import fr.proline.repository.DriverType
import org.junit.After
import org.junit.Assert
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test

import scala.collection.mutable.ArrayBuffer

class PeptideSpectrumMatcherTest extends AbstractMultipleDBTestCase with StrictLogging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "GRE_F068213_M2.4_TD_EColi"
  val targetRSId = 2L
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = _
  var rsProvider: IResultSetProvider = _

  protected var matcherService : PeptideSpectrumMatcherMascot = _

  @Before
  @throws(classOf[Exception])
  def setUp(): Unit = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    //Load Data
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs succesfully initialized !")
    executionContext = buildSQLContext()

  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }

  def buildSQLContext(): IExecutionContext = {
    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector(), useJPA = false)
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), useJPA = false)
    val executionContext = PeptideCacheExecutionContext(new BasicExecutionContext(1, udsDbCtx, msiDbCtx, null))
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(PeptideCacheExecutionContext(parserContext)))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(msiDbCtx))

    parserContext
  }

  private def getAndTestMSISearch(): MSISearch = {
    val msiDbHelper = new MsiDbHelper(executionContext.getMSIDbConnectionContext)
    val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext,
      executionContext.getMSIDbConnectionContext)

    //TODO Get ResultSetId for case only RSM specified !
    val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(targetRSId))(0))

    if (msiSearch.isEmpty || msiSearch.get.searchSettings.msmsSearchSettings.isEmpty) {
      logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
      throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
    }
    msiSearch.get
  }
   
    @Test
  def testGenerateMascotSpectrumMatchUsingRules(): Unit = {
	  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer
         
    try {
      
      val pepMId = 348L
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += pepMId
      
      val peptideMatchProvider = new SQLPeptideMatchProvider(PeptideCacheExecutionContext(executionContext))
      val spectrumProvider = new SQLSpectrumProvider(executionContext.getMSIDbConnectionContext)
      
      val peptideMatches = peptideMatchProvider.getPeptideMatches(pepMIds.toArray)
      val spectrumIds = peptideMatches.map(_.getMs2Query().spectrumId)
      val spectra = spectrumProvider.getSpectra(spectrumIds)
      val spectraById = Map() ++ spectra.map { sp => sp.id -> sp }
            
      val msiSearch = getAndTestMSISearch()      
      matcherService = new PeptideSpectrumMatcherMascot(spectraById, msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTol,
                                  msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit, msiSearch.searchSettings.fragmentationRuleSet,FragmentationRuleSetSource.SEARCH_SETTINGS)
      
      val spectrumMatch = matcherService.getSpectrumMatch(peptideMatches(0))
      Assert.assertNotNull(spectrumMatch.fragMatches)
      assertTrue(!spectrumMatch.fragMatches.isEmpty)
      
      Assert.assertNotNull(spectrumMatch.fragTable)
      assertTrue(!spectrumMatch.fragTable.isEmpty)     
      
    } catch {

      case ex: Exception =>
        val msg = if (ex.getCause != null) { "Error running  PeptideSpectrumMatcherMascot " + ex.getCause.getMessage } else { "Error running PeptideSpectrumMatcherMascot " + ex.getMessage }
        fail(msg)

    }    
  }
    
    
       @Test
  def testGenerateMascotSpectrumMatchWithoutsRules(): Unit = {
	  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer
         
    try {
      
      val pepMId = 348L
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += pepMId
      
      val peptideMatchProvider = new SQLPeptideMatchProvider(PeptideCacheExecutionContext(executionContext))
      val spectrumProvider = new SQLSpectrumProvider(executionContext.getMSIDbConnectionContext)
      
      val peptideMatches = peptideMatchProvider.getPeptideMatches(pepMIds.toArray)
      val spectrumIds = peptideMatches.map(_.getMs2Query().spectrumId)
      val spectra = spectrumProvider.getSpectra(spectrumIds)
      val spectraById = Map() ++ spectra.map { sp => sp.id -> sp }
                  
      val fragRuleSet = new FragmentationRuleSet(id = 1l, name="ESI-QUAD-TOF" )

      matcherService = new PeptideSpectrumMatcherMascot(spectraById, 0.8, "Da", Some(fragRuleSet),FragmentationRuleSetSource.SEARCH_SETTINGS)
      
      val spectrumMatch = matcherService.getSpectrumMatch(peptideMatches(0))    
      Assert.assertNotNull(spectrumMatch.fragMatches)
      assertTrue(!spectrumMatch.fragMatches.isEmpty)
      
      Assert.assertNotNull(spectrumMatch.fragTable)
      assertTrue(!spectrumMatch.fragTable.isEmpty)     
      
    } catch {

      case ex: Exception =>
        val msg = if (ex.getCause != null) { "Error running  PeptideSpectrumMatcherMascot " + ex.getCause.getMessage } else { "Error running PeptideSpectrumMatcherMascot " + ex.getMessage }
        fail(msg)

    }    
  }
}