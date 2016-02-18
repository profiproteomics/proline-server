package fr.proline.module.fragment_match

import org.junit.After
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test
import com.typesafe.scalalogging.StrictLogging
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.repository.DriverType
import scala.collection.mutable.ArrayBuffer
import fr.proline.repository.util.JDBCWork
import java.sql.Connection
import fr.profi.util.serialization.ProfiJSMSerialization
import fr.profi.util.serialization.CustomDoubleJacksonSerializer
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.msi.impl.SQLPeptideMatchProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.proline.core.om.model.msi.Instrument
import fr.proline.core.om.model.msi.FragmentationRule
import org.junit.Assert
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider

class PeptideSpectrumMatcherTest extends AbstractMultipleDBTestCase with StrictLogging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "GRE_F068213_M2.4_TD_EColi"
  val targetRSId = 2
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = null
  var rsProvider: IResultSetProvider = null

  protected var matcherService : PeptideSpectrumMatcherMascot = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    //Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")
    psDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/ps-db.xml")    
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs succesfully initialized !")
     val (execContext, rsProv) = buildSQLContext()
    executionContext = execContext

  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }

  def buildSQLContext() = {
    val udsDbCtx = ContextFactory.buildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false)
    val msiDbCtx = ContextFactory.buildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false)
    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    val rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    (parserContext, rsProvider)
  }

  private def getAndTestMSISearch(): MSISearch = {
    val msiDbHelper = new MsiDbHelper(executionContext.getMSIDbConnectionContext())
    val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext(),
      executionContext.getMSIDbConnectionContext(),
      executionContext.getPSDbConnectionContext())

    //TODO Get ResultSetId for case only RSM specified !
    val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(targetRSId))(0))
    if (!msiSearch.isDefined || !msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
      logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
      throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
    }
    msiSearch.get
  }
   
    @Test
  def testGenerateMascotSpectrumMatchUsingRules() = {
	  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer
         
    try {
      
      val pepMId = 348L
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += pepMId
      
      val peptideMatchProvider = new SQLPeptideMatchProvider(executionContext.getMSIDbConnectionContext(), executionContext.getPSDbConnectionContext)
      val spectrumProvider = new SQLSpectrumProvider(executionContext.getMSIDbConnectionContext())
      
      val peptideMatches = peptideMatchProvider.getPeptideMatches(pepMIds.toArray)
      val spectrumIds = peptideMatches.map(_.getMs2Query.spectrumId)
      val spectra = spectrumProvider.getSpectra(spectrumIds)
      val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
            
      val msiSearch = getAndTestMSISearch()      
      matcherService = new PeptideSpectrumMatcherMascot(spectraById, msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTol, msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit, msiSearch.searchSettings.instrumentConfig)
      
      val spectrumMatch = matcherService.getSpectrumMatch(peptideMatches(0))
      Assert.assertNotNull(spectrumMatch.fragMatches)
      assertTrue(!spectrumMatch.fragMatches.isEmpty)
      
      Assert.assertNotNull(spectrumMatch.fragTable)
      assertTrue(!spectrumMatch.fragTable.isEmpty)     
      
    } catch {

      case ex: Exception => {

        val msg = if (ex.getCause() != null) { "Error running  PeptideSpectrumMatcherMascot " + ex.getCause().getMessage() } else { "Error running PeptideSpectrumMatcherMascot " + ex.getMessage() }
        fail(msg)

      }
    }    
  }
    
    
       @Test
  def testGenerateMascotSpectrumMatchWithoutsRules() = {
	  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer
         
    try {
      
      val pepMId = 348L
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += pepMId
      
      val peptideMatchProvider = new SQLPeptideMatchProvider(executionContext.getMSIDbConnectionContext(), executionContext.getPSDbConnectionContext)
      val spectrumProvider = new SQLSpectrumProvider(executionContext.getMSIDbConnectionContext())
      
      val peptideMatches = peptideMatchProvider.getPeptideMatches(pepMIds.toArray)
      val spectrumIds = peptideMatches.map(_.getMs2Query.spectrumId)
      val spectra = spectrumProvider.getSpectra(spectrumIds)
      val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
                  
      val instr = new Instrument(id = 1l, name="ESI-QUAD-TOF" )
      val instrConfig = new InstrumentConfig(id=1l, name="ESI-QUAD-TOF (A1=QUAD F=CID A2=TOF)", instrument= instr, ms1Analyzer = "QUAD", msnAnalyzer ="TOF",activationType="CID")
      
      matcherService = new PeptideSpectrumMatcherMascot(spectraById, 0.8, "Da", instrConfig)
      
      val spectrumMatch = matcherService.getSpectrumMatch(peptideMatches(0))    
      Assert.assertNotNull(spectrumMatch.fragMatches)
      assertTrue(!spectrumMatch.fragMatches.isEmpty)
      
      Assert.assertNotNull(spectrumMatch.fragTable)
      assertTrue(!spectrumMatch.fragTable.isEmpty)     
      
    } catch {

      case ex: Exception => {

        val msg = if (ex.getCause() != null) { "Error running  PeptideSpectrumMatcherMascot " + ex.getCause().getMessage() } else { "Error running PeptideSpectrumMatcherMascot " + ex.getMessage() }
        fail(msg)

      }
    }    
  }
}