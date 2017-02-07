package fr.proline.jms.service.misc

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.IOException

import javax.jms.Message
import javax.jms.MessageProducer
import javax.jms.Session
import javax.jms.TextMessage

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging

import org.hornetq.api.jms.HornetQJMSConstants

import fr.profi.util.StringUtils
import fr.proline.jms.ServiceEvent
import fr.proline.jms.ServiceRunner
import fr.proline.jms.util.IServiceMonitoringNotifier
import fr.proline.jms.util.JMSConstants._


/**
 * JMS Service to get a resource on Proline Server or at least Node side as a Stream. Value is returned in BytesMessage
 *
 * Input param :  file_path : specify the path to the resource to get.
 *
 */
class ResourceService extends LazyLogging {

  /* Service Constants */
  val GET_RESOURCE_AS_STREAM_METHOD = "get_resource_as_stream"
  val FILE_PATH_PARAM_KEY = "file_path"

  val serviceName = "proline/misc/ResourceService"

  /* About the same code as ServiceRunner.handleMessage() but can return either a JMS BytesMessage with JMS_HQ_InputStream property 
     either a JMS TextMessage containing a standard JSON-RPC Response string (in case of service or file error) */
  def handleMessage(
    session: Session,
    message: Message,
    replyProducer: MessageProducer,
    serviceMonitoringNotifier: IServiceMonitoringNotifier
  ): Unit = {
    require((session != null), "Session is null")
    require((message != null), "Message is null")
    require((replyProducer != null), "ReplyProducer is null")

    val jmsMessageId = message.getJMSMessageID

    logger.debug(s"Handling ResourceService JMS Message [$jmsMessageId]")

    var responseJMSMessage: Message = null

    var jsonRequest: JSONRPC2Request = null
    var jsonRequestId: java.lang.Object = null
    var jsonResponse: JSONRPC2Response = new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, jsonRequestId)
    var serviceVersionOpt = Option.empty[String]
    var serviceSourceOpt = Option.empty[String]
    var serviceDescrOpt = Option.empty[String]
  
    try {

      if (message.isInstanceOf[TextMessage]) {
        val requestString = message.asInstanceOf[TextMessage].getText

        /* Parse JSON Request */
        jsonRequest = JSONRPC2Request.parse(requestString)
        jsonRequestId = jsonRequest.getID
        jsonResponse.setID(jsonRequestId)

        if (Thread.interrupted()) {
          val errorMessage = s"Thread interrupted before calling Service [$serviceName]"
          logger.error(errorMessage)

          jsonResponse.setError(ServiceRunner.buildJSONRPC2Error(SERVICE_ERROR_CODE, new Exception(errorMessage)))
        } else {
          val jmsMessageContext = ServiceRunner.buildJMSMessageContext(message)
          
          val serviceVersion = message.getStringProperty(PROLINE_SERVICE_VERSION_KEY)
          if (StringUtils.isNotEmpty(serviceVersion))
            serviceVersionOpt = Some(serviceVersion) 
          
          val serviceSource = message.getStringProperty(PROLINE_SERVICE_SOURCE_KEY)          
          if (StringUtils.isNotEmpty(serviceSource)) 
            serviceSourceOpt = Some(serviceSource) 
          
          val serviceDescr = message.getStringProperty(PROLINE_SERVICE_DESCR_KEY)          
          if (StringUtils.isNotEmpty(serviceDescr)) 
            serviceDescrOpt = Some(serviceDescr) 
          
          val serviceEvent = new ServiceEvent(
            jmsMessageId,
            jsonRequestId,
            serviceName,
            ServiceEvent.EVENT_START, 
            serviceVersionOpt,
            serviceSourceOpt,
            serviceDescrOpt
          )
          serviceEvent.setComplementaryInfo(requestString)
          serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

          val resourceResult = service(session, jmsMessageContext, jsonRequest)

          responseJMSMessage = resourceResult.responseJMSMessage
          jsonResponse = resourceResult.jsonResponse
        }

      } else {
        val errorMessage = "Invalid ResourceService JMS Message type"
        logger.error(errorMessage)

        jsonResponse.setError(ServiceRunner.buildJSONRPC2Error(MESSAGE_ERROR_CODE, new Exception(errorMessage) ))
      } // End if (JMS Message is a TextMessage)

    } catch {

      /* Catch all Throwables */
      case t: Throwable => {
        val errorMessage = s"Error handling ResourceService JMS Message [$jmsMessageId]"
        logger.error(errorMessage, t)

        jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(MESSAGE_ERROR_CODE, t), jsonRequestId)
      }

    } finally {

      /* In all cases, try to send a JSON Response to the JMS source Client */
      val replyDestination = message.getJMSReplyTo

      if (replyDestination == null) {
        logger.warn("ResourceService JMS Message has no 'JMSReplyTo' destination: cannot send JMS response to the client")
      } else {
        var serviceEvent: ServiceEvent = null

        if (responseJMSMessage == null) {
          serviceEvent = new ServiceEvent(
            jmsMessageId,
            jsonRequestId,
            serviceName,
            ServiceEvent.EVENT_FAIL,
            serviceVersionOpt,
            serviceSourceOpt,
            serviceDescrOpt
          )

          responseJMSMessage = session.createTextMessage()
          responseJMSMessage.setJMSCorrelationID(jmsMessageId)

          if (jsonResponse == null) {
            jsonResponse = new JSONRPC2Response(JSONRPC2Error.INTERNAL_ERROR, jsonRequestId)
          }

          responseJMSMessage.asInstanceOf[TextMessage].setText(jsonResponse.toJSONString())
        } else {
          serviceEvent = new ServiceEvent(
            jmsMessageId,
            jsonRequestId,
            serviceName,
            ServiceEvent.EVENT_SUCCESS,
            serviceVersionOpt,
            serviceSourceOpt,
            serviceDescrOpt
          )
        }

        /* Notify */
        serviceEvent.setComplementaryInfo(jsonRequest.toJSONString())
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        logger.debug(s"Sending JMS Response to ResourceService JMS Message [$jmsMessageId] on Destination [$replyDestination]")

        replyProducer.send(replyDestination, responseJMSMessage)
        logger.info(s"JMS Response to ResourceService JMS Message [$jmsMessageId] sent")
      }

    }

  }

  private def service(session: Session, jmsMessageContext: Map[String, Any], req: JSONRPC2Request): ResourceResult = {
    require(req != null, "service() req is null")

    val jsonRequestId = req.getID
    val method = req.getMethod

    /* Method dispatcher */
    method match {
      case GET_RESOURCE_AS_STREAM_METHOD => getResourceAsStream(session, jmsMessageContext, req)
      case _                             => new ResourceResult(null, new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, jsonRequestId))
    }

  }

  private def getResourceAsStream(session: Session, jmsMessageContext: Map[String, Any], req: JSONRPC2Request): ResourceResult = {
    require(session != null, "getResourceAsStream() session is null")
    require(req != null, "getResourceAsStream() req is null")

    val jsonRequestId = req.getID
    var responseJMSMessage: Message = null
    var jsonResponse: JSONRPC2Response = null

    /* Extract and check JSON params */
    val namedParams = req.getNamedParams

    val value = namedParams.get(FILE_PATH_PARAM_KEY)
    val filePath = value match {
      case str: String => str
      case _ => null
    }

    if (StringUtils.isEmpty(filePath)) {
      val errorMsg = s"Invalid '$FILE_PATH_PARAM_KEY' JSON-RPC named param"
      logger.error(errorMsg)
      
      val ex = new Exception(errorMsg)
      jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(SERVICE_ERROR_CODE, ex), jsonRequestId)
    } else {
      val file = new File(filePath)

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

            jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(SERVICE_ERROR_CODE, ex), jsonRequestId)
          }

        }

        // TODO: BufferedInputStream is closed by HornetQ JMS implementation ?
      } else {
        val errorMsg = s"Unknown '$filePath' file pathname"
        logger.error(errorMsg)
        
        val ex = new Exception(errorMsg)
        jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(SERVICE_ERROR_CODE, ex), jsonRequestId)
      }

    }

    new ResourceResult(responseJMSMessage, jsonResponse)
  }

}

class ResourceResult(val responseJMSMessage: Message, val jsonResponse: JSONRPC2Response) {
  require(responseJMSMessage != null || jsonResponse != null, "ResponseJMSMessage and jsonResponse cannot be both null")

}
