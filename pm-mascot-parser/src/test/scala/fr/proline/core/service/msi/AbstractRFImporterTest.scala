package fr.proline.core.service.msi

import java.io.File
import org.junit.{After,AfterClass, Assert, Test, Before,BeforeClass}
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.dal._
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.impl.{SQLPeptideProvider, SQLPTMProvider, SQLResultSetProvider, ORMSeqDatabaseProvider, ORMResultSetProvider, ORMProteinProvider, ORMPeptideProvider, ORMPTMProvider}
import fr.proline.core.om.provider.msi.{ProvidersFactory, IResultSetProvider}
import fr.proline.core.om.utils.AbstractMultipleDBTestCase
import fr.proline.core.om.storer.msi.impl.{StorerContext,StorerContextBuilder}
import fr.proline.repository.DriverType
import fr.proline.repository.DatabaseContext

// Note: the name of the trait ends with an underscore to indicate it must not be tested directly
trait AbstractRFImporterTest_ extends AbstractMultipleDBTestCase with Logging {

  protected val driverType: DriverType
  protected var _datFileName: String = "/F047876.dat"
  protected val thisProviderKey = "MSParser"
  
  protected def beforeAllTests() {}
  protected def afterAllTests() {}
  
  this.beforeAllTests()

  @throws( classOf[Exception] )
  def setUp() = {
    
    logger.info( "Initializing Dbs" )
    super.initDBsDBManagement( driverType )

    //Load Data
    psDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/Unimod_Dataset.xml" )  
    pdiDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/Proteins_Dataset.xml" )
    msiDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/Init_Dataset.xml" )
    
    logger.info( "PS, PDI and MSI dbs succesfully initialized" )
  }
  
  @throws( classOf[Exception] )
  def tearDown() {
    super.closeDbs
  }
  
  protected def runServiceForTest( stContext: StorerContext, rsProvider: IResultSetProvider ): Unit

  //@Test
  def runRFIwithSQLPepProviders() = {
    
    val msiDbConnector = msiDBTestCase.getConnector
    
    val udsDbCtx = StorerContextBuilder.buildDbContext(dbManagerForTest.getUdsDbConnector,useJpa = false)
    val pdiDbCtx = StorerContextBuilder.buildDbContext(dbManagerForTest.getPdiDbConnector,useJpa = true)
    val psDbCtx = StorerContextBuilder.buildDbContext(dbManagerForTest.getPsDbConnector,useJpa = false)
    val msiDbCtx = StorerContextBuilder.buildDbContext(msiDbConnector,useJpa = false)
    
    val stContext = new StorerContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx)

    //val pdiEM = stContext.pdiEm
    val psEzDBC = ProlineEzDBC( psDbCtx )
    
    ProvidersFactory.registerPeptideProvider( thisProviderKey, new SQLPeptideProvider( psDbCtx, psEzDBC ) )
    ProvidersFactory.registerPTMProvider( thisProviderKey, new SQLPTMProvider( psDbCtx, psEzDBC ) )
    ProvidersFactory.registerProteinProvider( thisProviderKey, new ORMProteinProvider(pdiDbCtx) )
    ProvidersFactory.registerSeqDatabaseProvider( thisProviderKey, new ORMSeqDatabaseProvider(pdiDbCtx) )

    val msiEzDBC = ProlineEzDBC( msiDbCtx ) // new SQLQueryHelper(stContext.msiConnector).ezDBC
    val udsSqlCtx = new SQLContext( udsDbCtx, ProlineEzDBC( udsDbCtx ) )
    val rsProvider = new SQLResultSetProvider( msiDbCtx, msiEzDBC, psDbCtx, psEzDBC, udsSqlCtx )
    
    this.runServiceForTest( stContext, rsProvider )
    
    stContext.closeAll()
  }
  
//    @Test
  def runRFIwithJPA() = {
    
    val msiDbConnector = msiDBTestCase.getConnector
    val stContext = StorerContextBuilder(dbManagerForTest, msiDbConnector,useJpa = true)
    
    /*val udsDbCtx = StorerContextBuilder.buildDbContext(dbManagerForTest.getUdsDbConnector,useJpa = false)
    val pdiDbCtx = StorerContextBuilder.buildDbContext(dbManagerForTest.getPdiDbConnector,useJpa = true)
    val psDbCtx = StorerContextBuilder.buildDbContext(dbManagerForTest.getPsDbConnector,useJpa = true)
    val msiDbCtx = StorerContextBuilder.buildDbContext(msiDbConnector,useJpa = true)    
    val stContext = new StorerContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx)*/

    //val psEM = stContext.psDbContext.getEntityManager()
    //val pdiEM = stContext.pdiDbContext.getEntityManager()
    ProvidersFactory.registerPeptideProvider( thisProviderKey, new ORMPeptideProvider(stContext.psDbContext) )
    ProvidersFactory.registerPTMProvider( thisProviderKey, new ORMPTMProvider(stContext.psDbContext) )
    ProvidersFactory.registerProteinProvider( thisProviderKey, new ORMProteinProvider(stContext.pdiDbContext) )
    ProvidersFactory.registerSeqDatabaseProvider( thisProviderKey, new ORMSeqDatabaseProvider(stContext.pdiDbContext) )

    val rsProvider = new ORMResultSetProvider( stContext.msiDbContext, stContext.psDbContext, stContext.pdiDbContext )
    
    this.runServiceForTest( stContext, rsProvider )
    
    stContext.closeAll()
    
    this.afterAllTests()
  }

}
