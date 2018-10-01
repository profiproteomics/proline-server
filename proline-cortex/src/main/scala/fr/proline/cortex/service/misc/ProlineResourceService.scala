package fr.proline.cortex.service.misc

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import javax.jms.Message
import javax.jms.Session

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.jsonrpc.BuildJSONRPC2Response
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.ServiceRunner
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteCompleteJsonRPC2Service
import fr.proline.jms.service.api.IRemoteServiceIdentity
import fr.proline.jms.util.JMSConstants
import org.hornetq.api.jms.HornetQJMSConstants

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf



trait IProlineResourceService extends IRemoteServiceIdentity with IDefaultServiceVersion with LazyLogging  {
 
    /* JMS Service identification */
  val serviceNamespace = "proline/misc"

  // TODO: rename to MergeDataSetsService
  val serviceLabel = "ProlineResourceService"

   
  this.serviceDescription = Some(
    "Return the specified file from Proline File System either by using the absolute path or by trying to use Proline Mount Point. If using absolute path a unique Noide should be running or absolute path should be the same on all nodes. Use can use ResourceService to specify a node."
  )
  
  val methodDefinitions: Seq[IJSONRPC2Method] = List(GET_RESOURCE_METHOD)
  
    // Description of the merge_result_sets service method
  object GET_RESOURCE_METHOD extends JSONRPC2DefaultMethod { // Emulate an enumeration of objects
    
    
    // Method description
    val name = "get_resource_as_stream"
    val description =  "Return the specified file from Proiline File System."
    val returns = JSONRPC2MethodResult(
      typeOf[String],
      "HornetQJMSConstants.JMS_HORNETQ_INPUT_STREAM Stream of requested resource."
    )
    
    val parameters = List(FILE_PATH_PARAM)
    
    object FILE_PATH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "file_path"
      val description = "The path, absolute or usinf mount point, of the resource to get."
      val scalaType: universe.Type = typeOf[String]
    }
  }
}


class ProlineResourceService extends IProlineResourceService with IRemoteCompleteJsonRPC2Service with LazyLogging  {

  
  override def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any],session: Session): Message= {  
    require(jsonRequest != null, "Req is null")
    require(session != null, "No session specified to create return message")
    
    val requestId = jsonRequest.getID
    val methodName = jsonRequest.getMethod
    var responseJMSMessage : javax.jms.Message = null
    
    /* Method dispatch */
    methodName match {
      
      case GET_RESOURCE_METHOD.name => {
          val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
          responseJMSMessage = getResourceAsStream(session, jmsMessageContext, paramsRetriever, jsonRequest) // Call service
          return responseJMSMessage
      }

      // Method name not supported
      case _ => {        
        return ServiceRunner.buildJMSMsgFromJSONRPC2Response(session, BuildJSONRPC2Response.forMethodNotFound(requestId),jmsMessageContext) 
      }
    }

    ServiceRunner.buildJMSMsgFromJSONRPC2Response(session, BuildJSONRPC2Response.forMethodNotFound(requestId),jmsMessageContext)     
  }
  
  
  private def getResourceAsStream(session: Session, jmsMessageContext: Map[String, Any], paramsRetriever: NamedParamsRetriever, req: JSONRPC2Request): Message = {
    require(session != null, "getResourceAsStream() session is null")
    require(req != null, "getResourceAsStream() req is null")

    val jsonRequestId = req.getID
    var responseJMSMessage: Message = null
    var jsonErrResponse: JSONRPC2Response = null
        

    /* Extract and check JSON params */
    val filePath = paramsRetriever.getString(GET_RESOURCE_METHOD.FILE_PATH_PARAM)    

    if (StringUtils.isEmpty(filePath)) {
      val errorMsg = s"Invalid '$GET_RESOURCE_METHOD.FILE_PATH_PARAM' JSON-RPC named param"
      logger.error(errorMsg)
      
      val ex = new Exception(errorMsg)
      jsonErrResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, ex), jsonRequestId)
    } else {
      
      val localFilePath =  MountPointRegistry.replacePossibleLabel(filePath, None).localPathname
      
      
      val file = new File(localFilePath)

      if (file.isFile) { // TODO Check file access with WorkDirectoryRegistry.isManagedFile()
        
        val absolutePathname = file.getAbsolutePath

        var br: BufferedInputStream = null

        try {
          br = new BufferedInputStream(new FileInputStream(file))

          responseJMSMessage = session.createBytesMessage()

          logger.debug(s"Sending InputStream from File [$absolutePathname] to JMS BytesMessage")

          responseJMSMessage.setObjectProperty(HornetQJMSConstants.JMS_HORNETQ_INPUT_STREAM, br)
        } catch {

          case ex: Exception => {
            val errorMessage = s"Error reading [$absolutePathname] InputStream"
            logger.error(errorMessage, ex)

            if (br != null) {
              try {
                br.close()
              } catch {
                case exClose: IOException => logger.error(s"Error closing [$absolutePathname] InputStream", exClose)
              }
            }

            jsonErrResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, ex), jsonRequestId)
          }

        }

        // TODO: BufferedInputStream is closed by HornetQ JMS implementation ?
      } else {
        val errorMsg = s"Unknown '$filePath' file pathname"
        logger.error(errorMsg)
        
        val ex = new Exception(errorMsg)
        jsonErrResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, ex), jsonRequestId)
      }

    } //End else file not empty
    
    if(responseJMSMessage == null) { //Error during process
      if(jsonErrResponse == null){//Should never be the case !
        jsonErrResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, new  RuntimeException("Unknow Error in ProlineResourceService")), jsonRequestId)
      }
      
      responseJMSMessage =  ServiceRunner.buildJMSMsgFromJSONRPC2Response(session, jsonErrResponse,jmsMessageContext)
    }

     responseJMSMessage
  }

} 

