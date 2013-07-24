package fr.proline.core.service.msi

import java.io.File
import org.junit.{After,AfterClass, Assert, Test, Before,BeforeClass}
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.repository.DriverType
import fr.proline.core.dal.SQLQueryHelper
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.SQLConnectionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.dal.ContextFactory

@Test
class RFImporterH2CTDTest extends AbstractRFImporterTest_ {
  
  val driverType = DriverType.H2
    
  @Before
  @throws( classOf[Exception] )
  override def setUp() = {
    super.setUp()
    _datFileName = "/dat_samples/STR_F136482_CTD.dat"
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )
  }
  
  @After
  override def tearDown() {
    super.tearDown()
  }
  
  @Test
  def testRFIwithSQL() = {
    val (executionContext, rsProvider) = buildSQLContext()
  
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFImporterH2CTDTest.this.getClass.getResource( _datFileName ).toURI )
    
    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.5 )
    propertiedBuilder += ( "subset.threshold" -> 0.5 )
    
    val importer = new ResultFileImporter(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
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
  
  @Test
  def runRFIwithJPA() = {
    val (executionContext, rsProvider) = buildJPAContext()
 
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFImporterH2CTDTest.this.getClass.getResource( _datFileName ).toURI )
    
    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.5 )
    propertiedBuilder += ( "subset.threshold" -> 0.5 )
    
    val importer = new ResultFileImporter(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
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
