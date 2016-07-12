package fr.proline.cortex.service.misc

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import scala.util.control.Breaks._
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.jms.service.api.IRemoteBytesMsgService
import fr.proline.jms.util.jsonrpc.ProfiJSONRPC2Response
import javax.jms.BytesMessage
import fr.proline.jms.util.JMSConstants
import fr.proline.cortex.util.WorkDirectoryFactory
import fr.proline.jms.ServiceRunner

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
class FileUpload extends IRemoteBytesMsgService with LazyLogging {

  /* JMS Service identification */
  val serviceName = "proline/misc/FileUpload"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  val DEST_FOLDER_PATH_PARAM_KEY = "dest_folder_path"
  val DEST_FILE_NAME_PARAM_KEY = "dest_file_name"

  
  override def service(jmsMessageContext: Map[String, Any], message: BytesMessage): JSONRPC2Response = {
    require((message != null), "message is null")

    val jsonRequestId = message.getJMSMessageID
    var jsonResponse: JSONRPC2Response = null
    
    var delFileOnexit = false;
    
    //Get Parameters 
    val readFileName =   message.getStringProperty(DEST_FILE_NAME_PARAM_KEY)
    val readFilePath =   message.getStringProperty(DEST_FOLDER_PATH_PARAM_KEY)
    var localPath = ""
    
    if(readFilePath != null) {
      localPath = MountPointRegistry.replacePossibleLabel(readFilePath).localPathname
    } else {
      localPath =  WorkDirectoryFactory.prolineWorkDirectory.getAbsolutePath
      delFileOnexit = true
    }
    
            
    val dirPath = new File(localPath)
        
    if (!dirPath.isDirectory()) {
      jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, "Invalid destination folder "+localPath), jsonRequestId)
    } else {

      //Read File name
      logger.debug("Write upladed File to "+readFileName+" in "+dirPath.getAbsoluteFile)
      val destFile = new File(dirPath, readFileName)
    //    if(delFileOnexit)
    //    destFile.deleteOnExit()
      
      
      val fos: FileOutputStream = new FileOutputStream(destFile);
      val outBuf: BufferedOutputStream = new BufferedOutputStream(fos);
      message.setObjectProperty("JMS_HQ_SaveStream", outBuf)


      outBuf.close()
      fos.close();

      jsonResponse = new ProfiJSONRPC2Response(destFile.getAbsolutePath, jsonRequestId)
    }

    jsonResponse
  }
}