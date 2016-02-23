package fr.proline.core.service.msi

import java.io.File

import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Ignore
import org.junit.Test

import fr.proline.context.IExecutionContext
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.repository.DriverType

@Test
class RFImporterH2CTDSQLTest extends AbstractRFImporterTestCase {
  
  val driverType = DriverType.H2    
  var executionContext : IExecutionContext = _
  var rsProvider : IResultSetProvider = _
    
  @Before
  @throws( classOf[Exception] )
  override def setUp() = {
    super.setUp()
    _datFileName = "/dat_samples/STR_F136482_CTD.dat"
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )
    val (execContext, rsP) = buildJPAContext
    executionContext = execContext
    rsProvider = rsP
  }
  
  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }
  
  @Test
  def testRFIwithSQL() = {
  
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( this.getClass.getResource( _datFileName ).toURI )
    
    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.5 )
    propertiedBuilder += ( "subset.threshold" -> 0.5 )
    
    val importer = new ResultFileImporter(
      executionContext,
      resultIdentFile = datFile,
      fileType = "mascot.dat",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1,// TODO : provide the right value
      importerProperties = Map.empty,      
      acDecoyRegex = Some("""sp\|REV_\S+""".r)
    )


    logger.debug( " --- run service " )
    val result = importer.runService()
    val id = importer.getTargetResultSetId
    logger.debug( " --- done " + result + " save with target resultID " + id )

    Assert.assertTrue( result )
    Assert.assertNotNull( id )
    Assert.assertTrue( id > 0 )

    val rsBackOp = rsProvider.getResultSet( id )
    Assert.assertTrue( rsBackOp.isDefined )
    val rsBack: ResultSet = rsBackOp.get
    Assert.assertNotNull( rsBack )

    executionContext.closeAll()
    // Other verifs....
  }
  


}
