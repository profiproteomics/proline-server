package fr.proline.cortex.service

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever

import fr.proline.cortex.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.util.jsonrpc.ProfiJSONRPC2Response

abstract class AbstractRemoteProcessService extends IRemoteService {
  
  protected def doProcess(paramsRetriever: NamedParamsRetriever) : Object
  protected var _requestId : Object = null
  def getRequestId = { _requestId}
    
  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {
    
    require((req != null), "Req is null")

    _requestId = req.getID
    val methodName = req.getMethod
    
    /* Method dispatch */
    methodName match {

      case "process" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)

        val result = doProcess(paramsRetriever) // Call service

        new ProfiJSONRPC2Response(result, _requestId)
      }

      // Method name not supported
      case _ => new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, _requestId)
    }
 
    new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, _requestId)
  }

  
}