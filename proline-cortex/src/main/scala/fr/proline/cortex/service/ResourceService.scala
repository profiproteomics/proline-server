package fr.proline.cortex.service

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.util.StringUtils
import fr.proline.cortex.ServiceRunner
import javax.jms.Message
import javax.jms.MessageProducer
import javax.jms.Session
import javax.jms.TextMessage
import java.io.IOException

class ResourceService extends Logging {

  /* Service Constants */
  val GET_RESOURCE_AS_STREAM_METHOD = "get_resource_as_stream"
  val FILE_PATH_PARAM_KEY = "file_path"

  val JMS_HORNET_Q_INPUT_STREAM_KEY = "JMS_HQ_InputStream"

  val serviceName = "proline/admin/ResourceService"

  /* About the same code as ServiceRunner.handleMessage() but can return either a JMS BytesMessage with JMS_HQ_InputStream property 
     either a JMS TextMessage containing a standard JSON-RPC Response string (in case of service or file error) */
  def handleMessage(session: Session, message: Message, replyProducer: MessageProducer) {
    require(session != null, "Session is null")
    require(message != null, "Message is null")
    require(replyProducer != null, "ReplyProducer is null")

    val jmsMessageId = message.getJMSMessageID

    logger.debug("Handling ResourceService JMS Message [" + jmsMessageId + ']')

    var jmsResponseMessage: Message = null

    var jsonRequestId: java.lang.Object = null
    var jsonResponse: JSONRPC2Response = new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, jsonRequestId)

    try {

      if (message.isInstanceOf[TextMessage]) {
        val requestString = message.asInstanceOf[TextMessage].getText

        /* Parse JSON Request */
        val jsonRequest = JSONRPC2Request.parse(requestString)

        jsonRequestId = jsonRequest.getID

        jsonResponse.setID(jsonRequestId)

        if (Thread.interrupted()) {
          val errorMessage = "Thread interrupted before calling Service [" + serviceName + ']'
          logger.warn(errorMessage)

          jsonResponse.setError(ServiceRunner.buildJSONRPC2Error(ServiceRunner.SERVICE_ERROR_CODE, errorMessage))
        } else {
          val resourceResult = process(session, jsonRequest)

          jmsResponseMessage = resourceResult.jmsResponseMessage
          jsonResponse = resourceResult.jsonResponse
        }

      } else {
        val errorMessage = "Invalid JMS Message type"
        logger.warn(errorMessage)

        jsonResponse.setError(ServiceRunner.buildJSONRPC2Error(ServiceRunner.MESSAGE_ERROR, errorMessage))
      } // End if (JMS Message is a TextMessage)

    } catch {

      /* Catch all Throwables */
      case t: Throwable => {
        val errorMessage = "Error handling ResourceService JMS Message [" + jmsMessageId + ']'
        logger.error(errorMessage, t)

        jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(ServiceRunner.MESSAGE_ERROR, errorMessage, t), jsonRequestId)
      }

    } finally {

      /* In all cases, try to send a JSON Response to the JMS source Client */
      val replyDestination = message.getJMSReplyTo

      if (replyDestination == null) {
        logger.warn("Message has no JMSReplyTo Destination : Cannot send JSON Response to Client")
      } else {

        if (jmsResponseMessage == null) {
          jmsResponseMessage = session.createTextMessage()
          jmsResponseMessage.setJMSCorrelationID(jmsMessageId)

          if (jsonResponse == null) {
            jsonResponse = new JSONRPC2Response(JSONRPC2Error.INTERNAL_ERROR, jsonRequestId)
          }

          jmsResponseMessage.asInstanceOf[TextMessage].setText(jsonResponse.toJSONString)
        }

        logger.debug("Sending JMS Response to Message [" + jmsMessageId + "] on Destination [" + replyDestination + ']')

        replyProducer.send(replyDestination, jmsResponseMessage)

        logger.info("ResourceService JMS Response to Message [" + jmsMessageId + "] sent")
      }

    }

  }

  private def process(session: Session, req: JSONRPC2Request): ResourceResult = {
    val jsonRequestId = req.getID
    val method = req.getMethod

    /* Method dispatcher */
    method match {
      case GET_RESOURCE_AS_STREAM_METHOD => getResourceAsStream(session, req)

      case _                             => new ResourceResult(null, new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, jsonRequestId))
    }

  }

  private def getResourceAsStream(session: Session, req: JSONRPC2Request): ResourceResult = {
    val jsonRequestId = req.getID

    var jmsResponseMessage: Message = null

    var jsonResponse: JSONRPC2Response = null

    /* Extract and check JSON params */
    val namedParams = req.getNamedParams

    var filePath: String = null

    val value = namedParams.get(FILE_PATH_PARAM_KEY)

    if (value.isInstanceOf[String]) {
      filePath = value.asInstanceOf[String]
    }

    if (StringUtils.isEmpty(filePath)) {
      jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(ServiceRunner.SERVICE_ERROR_CODE, "Invalid \"" + FILE_PATH_PARAM_KEY + "\" JSON-RPC named param"), jsonRequestId)
    } else {
      val file = new File(filePath)

      if (file.isFile) { // TODO Check file access with WorkDirectoryRegistry.isManagedFile()
        val absolutePathname = file.getAbsolutePath

        var br: BufferedInputStream = null

        try {
          br = new BufferedInputStream(new FileInputStream(file))

          jmsResponseMessage = session.createBytesMessage()

          logger.debug("Sending InputStream from File [" + absolutePathname + "] to JMS BytesMessage")

          jmsResponseMessage.setObjectProperty(JMS_HORNET_Q_INPUT_STREAM_KEY, br)
        } catch {

          case ex: Exception => {
            val errorMessage = "Error reading [" + absolutePathname + "] InputStream"
            logger.error(errorMessage, ex)

            if (br != null) {
              try {
                br.close()
              } catch {
                case ioEx: IOException => logger.error("Error closing [" + absolutePathname + "] InputStream", ioEx)
              }
            }

            jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(ServiceRunner.SERVICE_ERROR_CODE, errorMessage, ex), jsonRequestId)
          }

        }

        // br is closed by HornetQ JMS implementation ?        
      } else {
        jsonResponse = new JSONRPC2Response(ServiceRunner.buildJSONRPC2Error(ServiceRunner.SERVICE_ERROR_CODE, "Unknown [" + filePath + "] file pathname"), jsonRequestId)
      }

    }

    new ResourceResult(jmsResponseMessage, jsonResponse)
  }

}

class ResourceResult(val jmsResponseMessage: Message, val jsonResponse: JSONRPC2Response) {
  require((jmsResponseMessage != null) || (jsonResponse != null), "jmsResponseMessage and jsonResponse cannot be both null")

}