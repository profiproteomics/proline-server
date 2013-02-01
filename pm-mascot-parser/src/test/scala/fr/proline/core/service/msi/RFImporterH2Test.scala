package fr.proline.core.service.msi

import java.io.File

import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.Assert

import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.service.msi.AbstractRFImporterTest_
import fr.proline.core.service.msi.ResultFileImporterJPAStorer
import fr.proline.core.service.msi.ResultFileImporterSQLStorer
import fr.proline.repository.DriverType

@Test
class RFImporterH2Test extends AbstractRFImporterTest_ {
  
  val driverType = DriverType.H2
  
  @Before
  @throws( classOf[Exception] )
  override def setUp() = {
    super.setUp()
    _datFileName = "/dat_samples/STR_F122817_Mascot_v2.3.dat"
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )
  }
  
  @After
  @throws( classOf[Exception] )
  override def tearDown() {
    super.tearDown()
  }
  
  @Test
  def testRFIwithSQL() = {
    val (executionContext, rsProvider) = buildSQLContext()
        Assert.assertNotNull( executionContext )
    
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFImporterH2Test.this.getClass.getResource( _datFileName ).toURI )
    
    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.5 )
    propertiedBuilder += ( "subset.threshold" -> 0.5 )

    val importer: ResultFileImporterSQLStorer = new ResultFileImporterSQLStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",     
      instrumentConfigId = 1,
      peaklistSoftwareId = 1,// TODO : provide the right value
      importerProperties = Map.empty,
      acDecoyRegex = None
    )

    logger.debug( " --- run service " )
    val result = importer.runService()
    val id = importer.getTargetResultSetId
    logger.debug( " --- done " + result + " save with resultID " + id )

    Assert.assertTrue( result )
    Assert.assertNotNull( id )
    Assert.assertTrue( id > 0 )

    val rsBackOp = rsProvider.getResultSet( id )
    Assert.assertTrue( rsBackOp.isDefined )
    val rsBack: ResultSet = rsBackOp.get
    Assert.assertNotNull( rsBack )

    // Other verifs....
    
    executionContext.closeAll()
    
  }
  
  @Test
  def runRFIwithJPA() = {
    val (executionContext, rsProvider) = buildJPAContext()
    
    Assert.assertNotNull( executionContext )
    
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFImporterH2Test.this.getClass.getResource( _datFileName ).toURI )
    
    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.5 )
    propertiedBuilder += ( "subset.threshold" -> 0.5 )

    val importer: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",     
      instrumentConfigId = 1,
      peaklistSoftwareId = 1,// TODO : provide the right value
      importerProperties = Map.empty,
      acDecoyRegex = None
    )

    logger.debug( " --- run service " )
    val result = importer.runService()
    val id = importer.getTargetResultSetId
    logger.debug( " --- done " + result + " save with resultID " + id )

    Assert.assertTrue( result )
    Assert.assertNotNull( id )
    Assert.assertTrue( id > 0 )
    
    val rsBackOp = rsProvider.getResultSet( id )
    Assert.assertTrue( rsBackOp.isDefined )
    val rsBack: ResultSet = rsBackOp.get
    Assert.assertNotNull( rsBack )

    // Other verifs....
    
    executionContext.closeAll()
  }

}
