package fr.proline.core.service.msi

import java.io.File
import org.junit.{After,AfterClass, Assert, Test, Before,BeforeClass}
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.orm.util.DatabaseManager
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.repository.DriverType
import fr.proline.core.dal.SQLQueryHelper

@Test
class RFImporterH2Test extends AbstractRFImporterTest_ {
  
  val driverType = DriverType.H2
  
  @Before
  @throws( classOf[Exception] )
  override def setUp() = {
    super.setUp()
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )
  }
  
  @After
  @throws( classOf[Exception] )
  override def tearDown() {
    super.tearDown()
  }
  
  @Test
  override def runRFIwithSQLPepProviders() = {
    //super.runRFIwithSQLPepProviders()
  }
  
  @Test
  override def runRFIwithJPA() = {
    super.runRFIwithJPA()
  }

  protected def runServiceForTest( stContext: StorerContext, rsProvider: IResultSetProvider  ) = {
    
    Assert.assertNotNull( stContext )
    
    logger.debug( " --- Get File " + _datFileName )
    var datFile: File = new File( RFImporterH2Test.this.getClass.getResource( _datFileName ).toURI )
    
    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.5 )
    propertiedBuilder += ( "subset.threshold" -> 0.5 )

    val importer: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
      stContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      providerKey = thisProviderKey,
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

    //val rsProvider: IResultSetProvider = new ORMResultSetProvider(stContext.msiDbContext,stContext.psDbContext,stContext.pdiDbContext)

    val rsBackOp = rsProvider.getResultSet( id )
    Assert.assertTrue( rsBackOp.isDefined )
    val rsBack: ResultSet = rsBackOp.get
    Assert.assertNotNull( rsBack )

    // Other verifs....
  }

}
