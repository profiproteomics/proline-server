package fr.proline.module.fragmentmatch.service

import java.sql.Connection

import com.typesafe.scalalogging.StrictLogging
import fr.profi.util.serialization.CustomDoubleJacksonSerializer
import fr.profi.util.serialization.ProfiJSMSerialization
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.BuildMsiDbConnectionContext
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.repository.DriverType
import fr.proline.repository.util.JDBCWork
import org.junit.After
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test

import scala.collection.mutable.ArrayBuffer

class SpectrumMatchesGeneratorTest extends AbstractMultipleDBTestCase with StrictLogging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "GRE_F068213_M2.4_TD_EColi"
  val targetRSId = 2
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = _
  var rsProvider: IResultSetProvider = _

  
  protected var generatorService : SpectrumMatchesGenerator = _

  @Before
  @throws(classOf[Exception])
  def setUp(): Unit = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)
    
    //Load Data
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info(" MSI and UDS dbs succesfully initialized !")

    executionContext = buildSQLContext()
    generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, None, true)
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }

  def buildSQLContext(): IExecutionContext = {
    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector(), useJPA = false)
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), useJPA = false)
    val executionContext = PeptideCacheExecutionContext(new BasicExecutionContext(1, udsDbCtx,  msiDbCtx, null))
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(PeptideCacheExecutionContext(executionContext)))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(msiDbCtx))

   parserContext
  }

  @Test
  def testGenerateSpectrumMatch(): Unit = {
    try {
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, None, false)
      val result = generatorService.runService()
      assertTrue(result)

      //Read Back Spectrum info
      val pepMReadBackId = 348L
      
      var spectrumAsString  =""
        
      object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer
      
      val getExistingMatchesWork = new JDBCWork() {
	      override def execute(con: Connection) {
	    	var associatedObjectTreeId : Long = 0L
	    	
	        val query = "SELECT  peptide_match_object_tree_map.object_tree_id FROM peptide_match_object_tree_map " +
	          " WHERE peptide_match_object_tree_map.peptide_match_id =  " + pepMReadBackId+ " AND peptide_match_object_tree_map.schema_name = 'peptide_match.spectrum_match'; "
	        val stmt = con.createStatement()
	        val sqlResultSet = stmt.executeQuery(query)
	        if (sqlResultSet.next) {
	          associatedObjectTreeId = sqlResultSet.getLong(1)
	        } else {
	          fail ("No Spectrum matches saved for PepMatch "+pepMReadBackId)
	        }
	        
	        
	      	val query2 = "SELECT clob_data FROM object_tree WHERE id = " + associatedObjectTreeId + " ; "
			val stmt2 = con.createStatement()
			val sqlResultSet2 = stmt2.executeQuery(query2)
			if (sqlResultSet2.next) {
	          spectrumAsString =  sqlResultSet2.getString(1)	         
	        } else {
	          fail ("No Spectrum matches found for PepMatch "+pepMReadBackId)
	        }
	        stmt.close()
	      }
      } // End of jdbcWork anonymous inner class
      
      executionContext.getMSIDbConnectionContext.doWork(getExistingMatchesWork, false)
      this.logger.debug(" ------ RESULt BEFORE Serialisation  => "+spectrumAsString)
      val resultSpMa =CustomSerializer.deserialize[SpectrumMatch](spectrumAsString)    		  
      this.logger.info(" ---------  RESULt AFTER Serialisation  => "+resultSpMa.fragMatches)
   
    } catch {

      case ex: Exception => {

        val msg = if (ex.getCause != null) { "Error running Spectrum Matches Generator " + ex.getCause.getMessage } else { "Error running Spectrum Matches Generator " + ex.getMessage }
        fail(msg)

      }
    }
  }
  
    @Test
  def testGenerateExistingSpectrumMatch(): Unit = {
    try {
      
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += 348L
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, Some(pepMIds.toArray), None, false)
      val result = generatorService.runService()
      assertTrue(result)
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, None, false)
      val result2 = generatorService.runService()
      assertTrue(result2)

    } catch {

      case ex: Exception =>
        val msg = if (ex.getCause != null) { "Error running Spectrum Matches Generator " + ex.getCause.getMessage } else { "Error running Spectrum Matches Generator " + ex.getMessage }
        fail(msg)

    }
  }

    @Test
  def testForceGenerateSpectrumMatch(): Unit = {
    try {
      
      val pepMIds = new ArrayBuffer[Long]()
      pepMIds += 348L
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, Some(pepMIds.toArray), None, false)
      val result = generatorService.runService()
      assertTrue(result)
      
      generatorService = new SpectrumMatchesGenerator(executionContext, targetRSId, None, None, None, true)
      val result2 = generatorService.runService()
      assertTrue(result2)

    } catch {

      case ex: Exception =>
        val msg = if (ex.getCause != null) { "Error running Spectrum Matches Generator " + ex.getCause.getMessage } else { "Error running Spectrum Matches Generator " + ex.getMessage }
        fail(msg)

    }
  }

}