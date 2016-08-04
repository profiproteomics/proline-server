package fr.proline.jms.service.api

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever

import fr.profi.util.jsonrpc.BuildJSONRPC2Response
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.profi.util.jsonrpc.ProfiJSONRPC2Response

abstract class AbstractRemoteProcessingService extends IRemoteJsonRPC2Service {
  
  @throws(classOf[Exception])
  protected def doProcess(paramsRetriever: NamedParamsRetriever): Any
  
  protected var _requestId: Object = null
  def getRequestId(): Object = _requestId
  
  // Exceptions are caught by ServiceRunner to return Error 
  override def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    
    require(jsonRequest != null, "jsonRequest is null")

    _requestId = jsonRequest.getID
    
    /* Method dispatch */
    jsonRequest.getMethod match {

      case RemoteServiceIdentity.PROCESS_METHOD_NAME => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)

        val result = doProcess(paramsRetriever).asInstanceOf[Object] // Call service

        return ProfiJSONRPC2Response(result, _requestId)
      }

      // Method name not supported
      case _ => return BuildJSONRPC2Response.forMethodNotFound(_requestId)
    }
 
    BuildJSONRPC2Response.forMethodNotFound(_requestId)
  }

  
}