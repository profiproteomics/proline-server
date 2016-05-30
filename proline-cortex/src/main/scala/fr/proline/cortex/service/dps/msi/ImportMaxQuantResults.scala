package fr.proline.cortex.service.dps.msi

import java.util.ArrayList
import scala.collection.JavaConversions.mapAsScalaMap
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.provider.msi.SeqDbFakeProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.jms.service.api.AbstractRemoteProcessService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.module.parser.maxquant.MaxQuantResultParser
import scala.collection.mutable.Map
import java.io.File
import org.apache.commons.io.FilenameUtils
import java.io.IOException
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.io.FileInputStream


/**
 * Import MaxQuant result file in the MSIdb corresponding to the provided project id
 * 
 * Input params
 *   project_id : The id of the project used for data importation.
 *   result_files_dir : The path to folder or Zip file containing Result files to be imported. 
 *   instrument_config_id : id in datastore of the instrument config used for result file acquisition
 *   peaklist_software_id : id in datastore of the software use to generate peaklist
 * 
 *    
 *  Output params
 *    Following map corresponding to import result 
 *    - result_set_Ids : List of all created result set Ids 
 *    - warning_msg : information message that could be of interest for to caller (even up to GUI)... 
 */
class ImportMaxQuantResults extends AbstractRemoteProcessService with LazyLogging with ISingleThreadedService {
  
  /* JMS Service identification */
  val serviceName = "proline/dps/msi/ImportMaxQuantResults"
  val serviceVersion = "1.0"
  override val defaultVersion = true
  val singleThreadIdent= "ImportThread"
  
  private def buildParserContext(executionContext: IExecutionContext): ProviderDecoratedExecutionContext = {

    // Register some providers
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    // TODO: use real protein and seqDb providers
    parserContext.putProvider(classOf[IProteinProvider], ProteinFakeProvider)
    parserContext.putProvider(classOf[ISeqDatabaseProvider], SeqDbFakeProvider)

    val psSQLCtx = executionContext.getPSDbConnectionContext
    val sqlPTMProvider = new SQLPTMProvider(psSQLCtx)
    parserContext.putProvider(classOf[IPTMProvider], sqlPTMProvider)

    val sqlPepProvider = new SQLPeptideProvider(psSQLCtx)
    parserContext.putProvider(classOf[IPeptideProvider], sqlPepProvider)

    parserContext
  }

  /* Define the concrete doProcess method */
  override def doProcess(params: NamedParamsRetriever): Object = {
    require((params != null), "no parameter specified")
    
    val projectId = params.getLong("project_id")
    val resultFileFolders = params.getString("result_files_dir")
    val instrumentConfigId = params.getLong("instrument_config_id")
    val peaklistSoftwareId = params.getLong("peaklist_software_id")
//    val importerProperties = if (params.hasParam("importer_properties") == false) Map.empty[String, Any]
//    else params.getMap("importer_properties").map {
//      case (a, b) => {
//        if (a.endsWith(".file")) {
//          a -> MountPointRegistry.replacePossibleLabel(b.toString(), Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
//        } else a -> b.asInstanceOf[Any]
//      }
//    } toMap
//
//    val saveSpectrumMatches = if (params.hasParam("save_spectrum_matches") == false) false
//    else params.getBoolean("save_spectrum_matches")

    logger.info("Params : " + serialize(params))

    var result: Map[String, Object] = Map.empty 
    
    // Initialize the providers    
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      val parserCtx = this.buildParserContext(execCtx)
      val udsDbCtx = execCtx.getUDSDbConnectionContext()

      val udsDbHelper = new UdsDbHelper(udsDbCtx)

      var localPathname = MountPointRegistry.replacePossibleLabel(resultFileFolders, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
      var mqFolderName = localPathname
      val localFile =  new File(localPathname)
      if(!localFile.exists())
        throw new IllegalArgumentException("Specified Path not found on server side : "+resultFileFolders)
      
      if(!localFile.isDirectory()) { //Should be a zip file !
         mqFolderName =  unZipIt(localFile) 
      }
       logger.debug("TRY to Import MaxQuant result on: " + mqFolderName)
      
        // Instantiate the ResultFileImporter service
        val rsMQImporter = new MaxQuantResultParser(parserCtx,
            instrumentConfigId,
            peaklistSoftwareId,
            mqFolderName
        )
        rsMQImporter.run()
        val rsIds = rsMQImporter.getCreatedResultSetIds
        result.put("result_set_Ids", rsIds);
        result.put("warning_msg", rsMQImporter.getWarningMessage);
        logger.debug("Import MaxQuant result done : " + result)


    } finally {
      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    System.gc()

    result
  }

  
    /**
     * Unzip maxQuant result file 
     * @param zipFile input zip file
     * @param output output folder
     * @return the root folder name
     */
    private def unZipIt( zipFile: File) : String = {

      val  buffer : Array[Byte] = new Array[Byte](4096)
      var zis :ZipInputStream  = null
      try{
    		
      	//create output directory 
      	val parentFolder = zipFile.getParentFile
      	val folderName = FilenameUtils.getBaseName(zipFile.getAbsolutePath)
      	val folder = new File(parentFolder, folderName+"_unzip")
    	  if(!folder.exists()){
    		  folder.mkdir();
    	  }
    		
      	//get the zip file content
      	zis = new ZipInputStream(new FileInputStream(zipFile));
      	//get the zipped file list entry
      	var ze : ZipEntry = zis.getNextEntry();
    		
      	while(ze!=null){
    			
    	     val nextFileName = ze.getName();
           val newFile = new File(folder, nextFileName);                
           logger.debug("file unzip : "+ newFile.getAbsoluteFile());
                
            //create all non exists folders
            //else you will hit FileNotFoundException for compressed folder
            new File(newFile.getParent()).mkdirs();
              
            val fos :FileOutputStream = new FileOutputStream(newFile);             
            var len = zis.read(buffer);
            while ( len > 0) {
       		      fos.write(buffer, 0, len);
       		      len = zis.read(buffer)
            }        		
            fos.close();   
            ze = zis.getNextEntry();
    	  }
    	
        zis.closeEntry();
    	  zis.close();
    		
    	logger.debug("All zip entry unzip : "+ zipFile.getName());
    	
    	if(folder.list().length == 1) //root folder in ZIP
    	  return folder.listFiles()(0).getAbsolutePath
    	else
    	  return folder.getAbsolutePath
    	  
    }catch {
      case e: IOException => { 
        logger.debug("exception caught: " + e.getMessage)
        if(zis != null){
          try {
            zis.closeEntry();    	      
            zis.close();
          } catch {
            case e: IOException =>  logger.error("Error closing ZIP : " + zipFile.getName)
          }
        }
      }
      return null;
    }
 
   } 
  
}

