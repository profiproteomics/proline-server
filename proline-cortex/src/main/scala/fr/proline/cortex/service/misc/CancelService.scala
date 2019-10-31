package fr.proline.cortex.service.misc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.jsonrpc.BuildJSONRPC2Response
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.api.service.misc._
import fr.proline.jms.ServiceManager
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteJsonRPC2Service
import fr.proline.jms.util.JMSConstants

// TODO: rename me ServiceManager and implement multiple methods (cancel_service, get_service_status...)
class CancelService extends ICancelService with IDefaultServiceVersion with LazyLogging with IRemoteJsonRPC2Service   { 
  
   /* JMS Service identification */
  
  def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require(jsonRequest != null, "jsonRequest is null")

    val requestID = jsonRequest.getID
    val methodName = jsonRequest.getMethod

    /* Method dispatch */
    methodName match {
      case CANCEL_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        this._cancelProcess(paramsRetriever, requestID, jmsMessageContext)        
      }
      // Method name not supported
      case _ => BuildJSONRPC2Response.forMethodNotFound(requestID)
    }
  }

  private def _cancelProcess(
    paramsRetriever: NamedParamsRetriever,
    requestID: Object,
    jmsMessageContext: Map[String, Any]
  ): JSONRPC2Response = {

    val cancelledMsgId = paramsRetriever.getString("message_id")
    logger.info("Will Cancel message : " + cancelledMsgId)

    val runningFuture = ServiceManager.getFutureForMessage(cancelledMsgId)
    if (runningFuture != null) {
      val ret = runningFuture.cancel(true)
      new JSONRPC2Response(ret, requestID)
    } else {
      val errorMessage = s"Invalid Message ID ($cancelledMsgId)specified for 'Cancel' (##Message##_" + jmsMessageContext.get(JMSConstants.JMSMessageID) + ")"
      logger.warn(errorMessage)
      new JSONRPC2Response(JSONRPC2Error.INVALID_PARAMS, errorMessage)
    }
  }

}
