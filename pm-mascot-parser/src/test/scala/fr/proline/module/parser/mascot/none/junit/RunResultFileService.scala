package fr.proline.module.parser.mascot.none.junit

import java.io.File
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.om.provider.msi.impl.ORMPTMProvider
import fr.proline.core.om.provider.msi.impl.ORMPeptideProvider
import fr.proline.core.om.provider.msi.impl.ORMProteinProvider
import fr.proline.core.om.provider.msi.impl.ORMSeqDatabaseProvider
import fr.proline.core.om.provider.msi.ProvidersFactory
import fr.proline.core.om.storer.msi.impl.StorerContextBuilder
import fr.proline.core.orm.util.DatabaseManager
import fr.proline.core.service.msi.ResultFileImporterJPAStorer
import fr.proline.module.parser.mascot.MascotParseParams
import fr.proline.repository.DatabaseContext

object RunResultFileService extends Logging  {

    private var _datFileName: String = "/F047876.dat"
    private var absoluteDatFileNameSet = false
    def datFileName_= (value:String):Unit = {
      _datFileName = value
      absoluteDatFileNameSet= true
    }
    
    def datFileName = _datFileName
    
    val thisProviderKey = "MSParser"
    
    //new DatabaseManagerForTest(udsDb = udsConnector,initialize = true)
    val dbManager = DatabaseManager.getInstance()
    dbManager.initialize("db_uds_test.properties")
    
    lazy val udsConnector = {
      //val conn = DatabaseConnectorFactory.createDatabaseConnectorInstance(Database.UDS,"db_uds_test.properties")
	  val conn = dbManager.getUdsDbConnector()
	  
	  try {
		  // This is necessary since in-memory databases are closed when the last connection is closed. This
		  // method call creates a first connection that will be closed by closeAll() method.
		  conn.getDataSource().getConnection()
	  } catch {
	    case e: Exception =>  e.printStackTrace();
	  }
	  conn
    }
    
	def initParse() {
		logger.info(" --- Init DatabaseManagment ")

		//val psEMF = Persistence.createEntityManagerFactory(JPAUtil.PersistenceUnitNames.PS_Key.getPersistenceUnitName, dbManager.psDBConnector.getEntityManagerSettings)
		val psDbConnector = dbManager.getPsDbConnector
		val psDbCtx = new DatabaseContext( psDbConnector )
		//val psEM = psDbCtx.getEntityManager

		//val pdiEMF = Persistence.createEntityManagerFactory(JPAUtil.PersistenceUnitNames.PDI_Key.getPersistenceUnitName, dbManager.pdiDBConnector.getEntityManagerSettings)
		val pdiDbConnector = dbManager.getPdiDbConnector
		val pdiDbCtx = new DatabaseContext( pdiDbConnector )
		//val pdiEM = pdiDbCtx.getEntityManager

		ProvidersFactory.registerPeptideProvider(thisProviderKey, new ORMPeptideProvider(psDbCtx) )
//		ProvidersFactory.registerPeptideProvider(thisProviderKey, new SQLPeptideProvider(PsDb.apply(dbManager)))
		
		ProvidersFactory.registerPTMProvider(thisProviderKey, new ORMPTMProvider(psDbCtx))
		ProvidersFactory.registerProteinProvider(thisProviderKey, new ORMProteinProvider(pdiDbCtx))
		ProvidersFactory.registerSeqDatabaseProvider(thisProviderKey, new ORMSeqDatabaseProvider(pdiDbCtx))  			    
	}
    
    def runService() : Unit = {
        logger.info(" --- Get File "+datFileName)
        var datFile : File  = null
        if(absoluteDatFileNameSet)
        	datFile = new File(datFileName)
        else
          datFile = new File(RunResultFileService.getClass.getResource(datFileName).toURI)
        
        logger.info(" --- ResultFileImporter  "+datFile.exists)
        val msiCo = dbManager.getMsiDbConnector(2)
        logger.info(" --- msiCo  "+msiCo)
        //logger.info(" --- msiCo getEntityManagerSettings.isEmpty  "+msiCo.getEntityManagerSettings.isEmpty)
        
        val propertiedBuilder = Map.newBuilder[String, Any]
    	propertiedBuilder += (MascotParseParams.ION_SCORE_CUTOFF.toString ->0.5)
    	propertiedBuilder += (MascotParseParams.SUBSET_THRESHOLD.toString ->0.5)
    	
    	val projectId = 2
    	val storerContext = StorerContextBuilder(dbManager,projectId,useJpa = true)
        
		val importer = new ResultFileImporterJPAStorer(
		                     storerContext = storerContext,
		    				 resultIdentFile= datFile,
		    				 fileType ="MascotMSParser",
		    				 providerKey=thisProviderKey,
		    				 instrumentConfigId=1,
		    				 peaklistSoftwareId=1,
		    				 importerProperties = Map.empty)
        logger.info(" --- run service ")
        val result  = importer.runService()
        
        logger.info(" --- done "+result+ " save with resultID "+importer.getTargetResultSetId)
    }
    
    
  	def main(args: Array[String]): Unit = {
        System.out.println(" --- You should have correctly modified the UDS db configuration in /db_uds_test.properties !! --- ")
        System.out.println(" Press any key to continue ...")
        System.in.read()
		logger.debug("Start Logging Debug...RunResultFileService")
		val resultFileService = RunResultFileService
		if(args.length>0)
		  resultFileService.datFileName  = args.apply(0)
    	   
		resultFileService.initParse
		resultFileService.runService
 }
  	
 
}