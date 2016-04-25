package fr.proline.cortex.service.misc

import fr.proline.jms.service.api.IRemoteBytesMessageService
import com.typesafe.scalalogging.LazyLogging
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import fr.proline.jms.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.jms.util.jsonrpc.ProfiJSONRPC2Response
import javax.jms.BytesMessage
import fr.proline.jms.ServiceRunner
import java.io.FileOutputStream
import fr.proline.jms.util.JMSConstants
import java.io.BufferedOutputStream
import java.io.File
import scala.util.control.Breaks._

class FileUpload extends IRemoteBytesMessageService with LazyLogging {

  /* JMS Service identification */
  val serviceName = "proline/misc/FileUpload"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  val DEST_FOLDER_PATH_PARAM_KEY = "dest_folder_path"
  val DEST_FILE_NAME_PARAM_KEY = "dest_file_name"

  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {

    return new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, req.getID)
  }

  override def service(jmsMessageContext: Map[String, Any], message: BytesMessage, req: JSONRPC2Request): JSONRPC2Response = {
    require((req != null), "Req is null")
    require((message != null), "message is null")

    val requestId = req.getID
    val methodName = req.getMethod

    /* Method dispatch */
    methodName match {
       case "process" => { upload(jmsMessageContext,message,  req) }

      // Method name not supported
      case _ => return new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
    }
  }
  
  private def upload(jmsMessageContext: Map[String, Any],message: BytesMessage, req: JSONRPC2Request): JSONRPC2Response = {
    assert((req != null), "upload() req is null")

    val jsonRequestId = req.getID
    var jsonResponse: JSONRPC2Response = null
    val mountsPoints = MountPointRegistry.retrieveMountPointsByType(MountPointRegistry.RESULT_FILES_DIRECTORY, true)
    var destFileName = ""
    breakable { 
      for(mp <- mountsPoints){
        if(mp.label.endsWith("VDTEST")){
          destFileName = mp.path
          break
        }
      }
    }
    

    val localPathname = destFileName+"/vdtest.txt"
    val destFile = new File(localPathname);
//    if (!destFile.exists())
//      destFile.mkdirs()

//    if (!destFolder.isDirectory()) {
//      jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, "Invalid \"" + DEST_FOLDER_PATH_PARAM_KEY + "\" JSON-RPC named param"), jsonRequestId)
//
//    } else {

    //ReadFile name
     val fName = message.readUTF()
     logger.info("READ "+fName)
//      val file = new File(destFolder, fileName);
      val fos: FileOutputStream = new FileOutputStream(destFile);
      val outBuf: BufferedOutputStream = new BufferedOutputStream(fos);
      var nbrByte: Integer = 0;
      var i: Int = message.readInt()
      while (!i.equals(-1)) {
        outBuf.write(i);
        nbrByte = nbrByte + 4 //read more 4byte
        i = message.readInt()
      }
      outBuf.close()
      fos.close();

      jsonResponse = new ProfiJSONRPC2Response(nbrByte, jsonRequestId)
    

    jsonResponse
  }
}