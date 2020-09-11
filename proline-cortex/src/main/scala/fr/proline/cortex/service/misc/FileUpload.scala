package fr.proline.cortex.service.misc

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream


import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.ProfiJSONRPC2Response
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.cortex.util.fs.WorkDirectoryFactory
import fr.proline.jms.ServiceRunner
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteBytesMsgService
import fr.proline.jms.util.JMSConstants
import javax.jms.BytesMessage

/**
 * 
 * Define JMS Service to get Upload a file on Proline Server FileSystem.
 * 
 * This is not a JSON RPC Service. The unique parameter is specified in the message header.
 * -  dest_file_name : name of the file which will be uploaded on server FileSystem in 
 * working directory.
 * 
 * 
 */
class FileUpload extends IRemoteBytesMsgService with IDefaultServiceVersion with LazyLogging {

  /* JMS Service identification */
  val serviceNamespace = "proline/misc"
  val serviceLabel = "FileUpload"

  val DEST_FOLDER_PATH_PARAM_KEY = "dest_folder_path"
  val DEST_FILE_NAME_PARAM_KEY = "dest_file_name"
//  val OVERWRITE_PARAM_KEY = "overwrite_file"
  
  // TODO: define me in Proline-Cortex-API
  def methodDefinitions: Seq[IJSONRPC2Method] = Seq()

  override def runService(message: BytesMessage, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require(message != null, "message is null")

    val jsonRequestId = message.getJMSMessageID
    var jsonResponse: JSONRPC2Response = null
    
    var delFileOnexit = false
    
    //Get Parameters 
    val overwriteFile = false
    val readFileName =   message.getStringProperty(DEST_FILE_NAME_PARAM_KEY)
    val readFilePath =   message.getStringProperty(DEST_FOLDER_PATH_PARAM_KEY)
    logger.debug("Will try to upload "+readFileName+" in "+readFilePath+ " (or temp). Allow overwrite ? "+overwriteFile )
    var localPath = ""
    
    if(readFilePath != null) {
      localPath = MountPointRegistry.replacePossibleLabel(readFilePath).localPathname
    } else {
      localPath =  WorkDirectoryFactory.prolineWorkDirectory.getAbsolutePath
      delFileOnexit = true
    }
    
            
    val dirPath = new File(localPath)
    if(!dirPath.exists()){
      try {
        dirPath.mkdirs()
      }catch {
        case se: SecurityException => {
          logger.debug("Unable to create destination folder "+localPath)
          val ex = new Exception("Unable to create destination folder "+readFilePath)
          jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, ex), jsonRequestId)
        }
      }
    }
      
    if(jsonResponse == null) { //No Previous Error    
      if (!dirPath.isDirectory) {
        logger.debug("Invalid destination folder "+localPath)
        val ex = new Exception("Invalid destination folder "+readFilePath)
        jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, ex), jsonRequestId)
      } else {
  
        //Read File name
        logger.debug("Write uploaded File to "+readFileName+" in "+dirPath.getAbsoluteFile)
        val destFile = new File(dirPath, readFileName)
        if(destFile.exists() && !overwriteFile){
          logger.debug("Destination file already exist in "+localPath+" : " +destFile.getAbsoluteFile)
          val ex = new Exception(s"Destination file already exist ${destFile.getName} in $readFilePath")
          jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, ex), jsonRequestId)
        } else {
          //    if(delFileOnexit)
          //    destFile.deleteOnExit()
        
        
          val fos = new FileOutputStream(destFile)
          val outBuf = new BufferedOutputStream(fos)
          message.setObjectProperty("JMS_HQ_SaveStream", outBuf)  //  JMS_HORNETQ_SAVE_STREAM  // JPM.JMS
  
          outBuf.close()
          fos.close()
  
          jsonResponse = new ProfiJSONRPC2Response(destFile.getAbsolutePath, jsonRequestId)
        }
      }
    } // End No Previous error

    jsonResponse
  }
}