package fr.proline.cortex.service.misc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.api.service.misc.IMiscService
import fr.proline.jms.ServiceManager
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.util.JMSConstants
import fr.proline.jms.service.api.IRemoteJsonRPC2Service

class CancelService extends IMiscService with IDefaultServiceVersion with LazyLogging with IRemoteJsonRPC2Service   { 
  
   /* JMS Service identification */
  
  val serviceLabel = "CancelService"
  
  // TODO: define me in Proline-Cortex-API
  def methodDefinitions: Seq[IJSONRPC2Method] = Seq()

  var requestID : Object = ""
  
  def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require(jsonRequest != null, "jsonRequest is null")

    requestID = jsonRequest.getID
    val methodName = jsonRequest.getMethod

    /* Method dispatch */
    methodName match {
      case "cancel" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        return cancelProcess(jmsMessageContext, paramsRetriever)        
      }
      // Method name not supported
      case _ => return new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestID)
      
    } 
  }
  
  def cancelProcess(jmsMessageContext: Map[String, Any], paramsRetriever: NamedParamsRetriever) : JSONRPC2Response = {
    val cancelledMsgId = paramsRetriever.getString("message_id")
    logger.info("Will Cancel message : "+cancelledMsgId)
    val runningFuture = ServiceManager.getFutureForMessage(cancelledMsgId)
    if(runningFuture != null ){
      val ret = runningFuture.cancel(true)
      return new JSONRPC2Response(ret, requestID)
    } else {
        val errorMessage = s"Invalid Message ID ($cancelledMsgId)specified for 'Cancel' (##Message##_"+jmsMessageContext.get(JMSConstants.JMSMessageID)+")"
        logger.warn(errorMessage)
        new JSONRPC2Response(JSONRPC2Error.INVALID_PARAMS, errorMessage)
    }
  }

}
