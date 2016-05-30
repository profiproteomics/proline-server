package fr.proline.jms.service.api

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever

import fr.proline.jms.util.jsonrpc.JSONRPC2Utils
import fr.proline.jms.util.jsonrpc.ProfiJSONRPC2Response

abstract class AbstractRemoteProcessService extends IRemoteJsonRPCService {
  
  @throws(classOf[Exception])
  protected def doProcess(paramsRetriever: NamedParamsRetriever) : Object
  protected var _requestId : Object = null
  def getRequestId = { _requestId}
    
  //Exception are catched by ServicRunner to return Error 
  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {
    
    require((req != null), "Req is null")

    _requestId = req.getID
    val methodName = req.getMethod
    
    /* Method dispatch */
    methodName match {

      case "process" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)

        val result = doProcess(paramsRetriever) // Call service

        return new ProfiJSONRPC2Response(result, _requestId)
      }

      // Method name not supported
      case _ => return new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, _requestId)
    }
 
    new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, _requestId)
  }

  
}