package fr.proline.cortex

import java.lang.Thread

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.slf4j.Logging

import Constants.PROLINE_NODE_ID_KEY
import Constants.PROLINE_SERVICE_NAME_KEY
import Constants.PROLINE_SERVICE_VERSION_KEY
import NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME
import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.cortex.service.IRemoteService
import fr.proline.cortex.service.ISingleThreadedService
import javax.jms.Connection
import javax.jms.Message
import javax.jms.MessageProducer
import javax.jms.Queue
import javax.jms.Session
import javax.jms.TextMessage

object ServiceRunner {

  /* Constants */

  /* JSON RPC Error codes */
  val MESSAGE_ERROR = -32001
  val SERVICE_ERROR_CODE = -32002

  /* Static methods */
  def buildJSONRPC2Error(code: Int, baseErrorMessage: String, t: Throwable = null): JSONRPC2Error = {
    require(!StringUtils.isEmpty(baseErrorMessage), "Invalid baseErrorMessage")

    if (t == null) {
      new JSONRPC2Error(code, baseErrorMessage)
    } else {
      /* Put full stack trace string in {{{JSONRPC2Error}}} data field */
      new JSONRPC2Error(code, baseErrorMessage + " : " + t, t.getStackTraceString)
    }

  }

  def buildSelectorString2(handledServices: List[IRemoteService], parallelizableRunner: Boolean): String = {
    val buff = new StringBuilder()

    var first: Boolean = true

    if (parallelizableRunner) {
      /* Add ResourceService handling for this Node */
      buff.append("((").append(PROLINE_SERVICE_NAME_KEY)
      buff.append(" = \'").append(ServiceRegistry.resourceService.serviceName).append("\') AND (")
      buff.append(PROLINE_NODE_ID_KEY)
      buff.append(" = \'").append(NodeConfig.NODE_ID).append("\'))")

      first = false
    }

    for (service <- handledServices) {

      if (parallelizableRunner) {

        /* Do not allow SingleThreadedService in parallelizableRunner */
        if (service.isInstanceOf[ISingleThreadedService]) {
          throw new RuntimeException("Parallelizable ServiceRunner does not accept ISingleThreadedService")
        }

      }

      if (first) {
        first = false
      } else {
        buff.append(" OR ")
      }

      buff.append("((").append(PROLINE_SERVICE_NAME_KEY)
      buff.append(" = \'").append(service.serviceName).append("\') AND (")
      buff.append(PROLINE_SERVICE_VERSION_KEY)
      buff.append(" = \'").append(service.serviceVersion).append("\'))")

      if (service.defaultVersion) {
        buff.append(" OR ")
        buff.append("((").append(PROLINE_SERVICE_NAME_KEY)
        buff.append(" = \'").append(service.serviceName).append("\') AND (")
        buff.append(PROLINE_SERVICE_VERSION_KEY)
        buff.append(" IS NULL))")
      }

    } // End loop for each service

    buff.toString
  }

}

/**
 * Builds JMS Consumer to run {{{IRemoteService}}} on given JMS {{{Queue}}}.
 */
class ServiceRunner(queue: Queue, connection: Connection) extends Runnable with Logging {

  import ServiceRunner._

  /* Constructor checks */
  require(queue != null, "Queue is null")
  require(connection != null, "Connection is null")

  /* Concrete Runnable.run() method */
  def run() {
    val currentThread = Thread.currentThread

    if (!currentThread.getUncaughtExceptionHandler.isInstanceOf[ThreadLogger]) {
      currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))
    }

    // Step 5. Create a JMS Session (Session MUST be confined in current Thread)
    // Not transacted, AUTO_ACKNOWLEDGE
    val session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE)

    try {
      // Step 6. Create a JMS Message Consumer (Consumer MUST be confined in current Thread)
      val selectorString = buildSelectorString()

      if (StringUtils.isEmpty(selectorString)) {
        throw new RuntimeException("No valid selector on [" + PROLINE_SERVICE_REQUEST_QUEUE_NAME + ']')
      }

      logger.debug("Consumer selector string \"" + selectorString + '\"')

      val consumer = session.createConsumer(queue, selectorString)

      /* ReplyProducer to send JMS Response Message to Client (Producer MUST be confined in current Thread) */
      val replyProducer = session.createProducer(null)

      logger.debug("Entering Consumer receive loop [" + currentThread.getName + ']')

      val resourceService = ServiceRegistry.resourceService
      val nodeId = NodeConfig.NODE_ID

      /* Infinite loop consuming JMS messages */
      var goOn: Boolean = true // Optimistic initialization

      while (goOn) {

        try {
          val message = consumer.receive() // blocks indefinitely

          if (message == null) {
            goOn = false
            logger.info("Consumer Connection is closed : exiting receive loop")
          } else if (resourceService.serviceName.equals(message.getStringProperty(PROLINE_SERVICE_NAME_KEY)) &&
            nodeId.equals(message.getStringProperty(PROLINE_NODE_ID_KEY))) {
            resourceService.handleMessage(session, message, replyProducer)
          } else {
            handleMessage(session, message, replyProducer)
          }

        } catch {
          /* Catch all Throwables to run INFINITE loop */
          case t: Throwable => logger.error("Error running Consumer reception loop", t)
        }

      }

    } finally {

      if (session != null) {
        try {
          session.close()
          logger.info("JMS Session closed")
        } catch {
          case exClose: Exception => logger.error("Error closing JMS Session", exClose)
        }
      }

    }

  }

  protected def buildSelectorString(): String = {
    val handledServices = ServiceRegistry.getParallelizableServices

    /* Parallelizable ServiceRunner */
    buildSelectorString2(handledServices, true)
  }

  protected def getServiceInstance(serviceName: String, serviceVersion: String): Option[IRemoteService] = {
    ServiceRegistry.getService(serviceName, serviceVersion)
  }

  /* Private methods */
  private def handleMessage(session: Session, message: Message, replyProducer: MessageProducer) {
    assert(session != null, "handleMessage() session is null")
    assert(message != null, "handleMessage() message is null")
    assert(replyProducer != null, "handleMessage() replyProducer is null")

    val jmsMessageId = message.getJMSMessageID

    logger.debug("Handling JMS Message [" + jmsMessageId + ']')

    var jsonRequestId: java.lang.Object = null
    var jsonResponse: JSONRPC2Response = new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, jsonRequestId)

    try {

      if (message.isInstanceOf[TextMessage]) {
        val requestString = message.asInstanceOf[TextMessage].getText

        /* Parse JSON Request */
        val jsonRequest = JSONRPC2Request.parse(requestString)

        jsonRequestId = jsonRequest.getID

        jsonResponse.setID(jsonRequestId)

        val serviceName = message.getStringProperty(PROLINE_SERVICE_NAME_KEY)
        val serviceVersion = message.getStringProperty(PROLINE_SERVICE_VERSION_KEY)

        if (StringUtils.isEmpty(serviceName)) {
          /* Cannot occur if 'selectorString' is a valid filter for JMS Messages */
          val errorMessage = "Invalid \"" + PROLINE_SERVICE_NAME_KEY + "\" property"
          logger.warn(errorMessage)

          jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR, errorMessage))
        } else {
          val optionalServiceInstance = getServiceInstance(serviceName, serviceVersion)

          if (optionalServiceInstance.isDefined) {
            jsonResponse = callService(optionalServiceInstance.get, jsonRequest)
          } else {
            /* Cannot occur if 'selectorString' is a valid filter for JMS Messages */
            val errorMessageBuilder = new StringBuilder()
            errorMessageBuilder.append("Unknown \"").append(PROLINE_SERVICE_NAME_KEY).append("\" [")
            errorMessageBuilder.append(serviceName).append("]  for ")

            if (StringUtils.isEmpty(serviceVersion)) {
              errorMessageBuilder.append("default version")
            } else {
              errorMessageBuilder.append("version [").append(serviceVersion).append(']')
            }

            val errorMessage = errorMessageBuilder.toString
            logger.warn(errorMessage)

            jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR, errorMessage))
          } // End if (serviceName map a valid ServiceInstance)

        } // End if (serviceName property is valid)

      } else {
        val errorMessage = "Invalid JMS Message type"
        logger.warn(errorMessage)

        jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR, errorMessage))
      } // End if (JMS Message is a TextMessage)

    } catch {

      /* Catch all Throwables */
      case t: Throwable => {
        val errorMessage = "Error handling JMS Message [" + jmsMessageId + ']'
        logger.error(errorMessage, t)

        jsonResponse = new JSONRPC2Response(buildJSONRPC2Error(MESSAGE_ERROR, errorMessage, t), jsonRequestId)
      }

    } finally {

      /* In all cases, try to send a JSON Response to the JMS source Client */
      val replyDestination = message.getJMSReplyTo

      if (replyDestination == null) {
        logger.warn("Message has no JMSReplyTo Destination : Cannot send JSON Response to Client")
      } else {

        if (jsonResponse == null) {
          jsonResponse = new JSONRPC2Response(JSONRPC2Error.INTERNAL_ERROR, jsonRequestId)
        }

        val jmsResponseMessage = session.createTextMessage()
        jmsResponseMessage.setJMSCorrelationID(jmsMessageId)
        jmsResponseMessage.setText(jsonResponse.toJSONString)

        logger.debug("Sending JMS Response to Message [" + jmsMessageId + "] on Destination [" + replyDestination + ']')

        replyProducer.send(replyDestination, jmsResponseMessage)
      }

    }

  }

  private def callService(serviceInstance: IRemoteService, jsonRequest: JSONRPC2Request): JSONRPC2Response = {
    assert(serviceInstance != null, "callService() serviceInstance is null")
    assert(jsonRequest != null, "callService() jsonRequest is null")

    val serviceName = serviceInstance.getClass.getName

    val jsonRequestId = jsonRequest.getID

    if (Thread.interrupted()) {
      val errorMessage = "Thread interrupted before calling Service [" + serviceName + ']'
      logger.warn(errorMessage)

      new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, errorMessage), jsonRequestId)
    } else {

      try {
        logger.debug("Calling Service [" + serviceName + "] with JSON Request [" + jsonRequest + ']')

        serviceInstance.process(jsonRequest)
      } catch {

        case intEx: InterruptedException => {
          val errorMessage = "Thread interrupted running Service [" + serviceName + ']'
          logger.warn(errorMessage, intEx)

          new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, errorMessage, intEx), jsonRequestId)
        }

        /* Catch all Throwables */
        case t: Throwable => {
          val errorMessage = "Error calling Service [" + serviceName + ']'
          logger.error(errorMessage, t)

          new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, errorMessage, t), jsonRequestId)
        }

      }

    } // End if (Thread is not interrupted)

  }

}

class SingleThreadedServiceRunner(queue: Queue, connection: Connection, serviceName: String)
  extends ServiceRunner(queue, connection) {

  import ServiceRunner._

  /* Constructor checks */
  require(!StringUtils.isEmpty(serviceName), "Invalid serviceName")

  val handledServices = retrieveHandledServices()

  require((handledServices != null) && !handledServices.isEmpty, "No SingleThreadedServices for name [" + serviceName + ']')

  protected override def buildSelectorString(): String = {
    /* NON-Parallelizable ServiceRunner */
    buildSelectorString2(handledServices, false)
  }

  /* Private methods */
  private def retrieveHandledServices(): List[IRemoteService] = {
    val singleThreadedServicesPerName = ServiceRegistry.getSingleThreadedServices
    val optionalList = singleThreadedServicesPerName.get(serviceName)
    optionalList.getOrElse(null)
  }

}
