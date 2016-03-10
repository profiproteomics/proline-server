package fr.proline.jms

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.mutable

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response

import com.typesafe.scalalogging.LazyLogging

import fr.proline.jms.util.IServiceMonitoringNotifier
import fr.proline.jms.util.MonitoringTopicPublisherRunner
import fr.proline.jms.util.JMSConstants
import fr.proline.jms.util.NodeConfig
import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.jms.service.api.IRemoteService
import fr.proline.jms.service.api.ISingleThreadedService

import javax.jms.Connection
import javax.jms.Message
import javax.jms.MessageProducer
import javax.jms.Queue
import javax.jms.Session
import javax.jms.TextMessage
import javax.jms.JMSException

object ServiceRunner extends LazyLogging {

  /* Constants */



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

  def buildConcreteSelectorString(handledServices: List[IRemoteService], parallelizableRunner: Boolean): String = {
    require((handledServices != null), "HandledServices List is null")

    val buff = new StringBuilder()

    var first: Boolean = true

    
    if (parallelizableRunner) {
      /* Add ResourceService handling for this Node */
      buff.append("((").append(JMSConstants.PROLINE_SERVICE_NAME_KEY)
      buff.append(" = \'").append(ServiceRegistry.resourceService.serviceName).append("\') AND (")
      buff.append(JMSConstants.PROLINE_NODE_ID_KEY)
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

      buff.append("((").append(JMSConstants.PROLINE_SERVICE_NAME_KEY)
      buff.append(" = \'").append(service.serviceName).append("\') AND (")
      buff.append(JMSConstants.PROLINE_SERVICE_VERSION_KEY)
      buff.append(" = \'").append(service.serviceVersion).append("\'))")

      if (service.defaultVersion) {
        buff.append(" OR ")
        buff.append("((").append(JMSConstants.PROLINE_SERVICE_NAME_KEY)
        buff.append(" = \'").append(service.serviceName).append("\') AND (")
        buff.append(JMSConstants.PROLINE_SERVICE_VERSION_KEY)
        buff.append(" IS NULL))")
      }

    } // End loop for each service

    buff.toString
  }

  def buildJMSMessageContext(message: Message): Map[String, Any] = {
    require((message != null), "Message is null")

    val mutableMap = mutable.Map.empty[String, Any]

    /* Add some Standard JMS header proprties */
    val jmsMessageId = message.getJMSMessageID
    mutableMap.put(JMSConstants.JMS_MESSAGE_ID_KEY, jmsMessageId) // Should not be null on message Reception

    val jmsCorrelationId = message.getJMSCorrelationID
    if (jmsCorrelationId != null) {
      mutableMap.put(JMSConstants.JMS_CORRELATION_ID_KEY, jmsCorrelationId)
    }

    val jmsTimestamp = message.getJMSTimestamp
    mutableMap.put(JMSConstants.JMS_TIMESTAMP_KEY, jmsTimestamp) // Long primitive

    val jmsDestination = message.getJMSDestination
    mutableMap.put(JMSConstants.JMS_DESTINATION_KEY, jmsDestination) // Should not be null on message Reception

    val jmsReplyTo = message.getJMSReplyTo
    if (jmsReplyTo != null) {
      mutableMap.put(JMSConstants.JMS_REPLY_TO_KEY, jmsReplyTo)
    }

    /* Add all other JMS properties (Provider and Proline specific) */
    val propertyNames = message.getPropertyNames

    while (propertyNames.hasMoreElements) {
      val elem = propertyNames.nextElement

      if (elem.isInstanceOf[String]) {
        val propertyName = elem.asInstanceOf[String]

        val value = message.getObjectProperty(propertyName)

        val oldValue = mutableMap.put(propertyName, value)
        if (oldValue.isDefined) {
          logger.warn("JMS Message Property [" + propertyName + "] overridden")
        }

      } else {
        logger.warn("Invalid JMS Message Property name")
      }

    }

    mutableMap.toMap
  }

}

/**
 * Builds JMS Consumer to run {{{IRemoteService}}} on given JMS {{{Queue}}}.
 */
class ServiceRunner(queue: Queue, connection: Connection, serviceMonitoringNotifier: IServiceMonitoringNotifier) extends Runnable with LazyLogging {

  import ServiceRunner._

  /* Constructor checks */
  require((queue != null), "Queue is null")

  require((connection != null), "Connection is null")

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
        throw new RuntimeException("No valid selector on [" + NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME + ']')
      }

      logger.debug("Consumer selector string \"" + selectorString + '\"')

      val consumer = session.createConsumer(queue, selectorString)

      /* ReplyProducer to send Response JMS Message to Client (Producer MUST be confined in current Thread) */
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
          } else if (resourceService.serviceName.equals(message.getStringProperty(JMSConstants.PROLINE_SERVICE_NAME_KEY)) &&
            nodeId.equals(message.getStringProperty(JMSConstants.PROLINE_NODE_ID_KEY))) {
            /* Special ResourceService handling */
            resourceService.handleMessage(session, message, replyProducer, serviceMonitoringNotifier)
          } else {
            /* Normal Service Request handling */
            handleMessage(session, message, replyProducer)
          }

        } catch {
          /* Catch all Throwables to run INFINITE loop */
          case t: Throwable => logger.error("Error running Consumer reception loop", t)
        }

      } // End infinite loop on message receive

      logger.info("Exiting ServiceRunner loop")
    } finally {

      if (session != null) {
        try {
          session.close()
          logger.info("JMS Session closed")
        } catch {
          case exClose: JMSException => logger.error("Error closing JMS Session", exClose)
        }
      }

    }

  }

  protected def buildSelectorString(): String = {
    val handledServices = ServiceRegistry.getParallelizableServices

    /* Parallelizable ServiceRunner */
    buildConcreteSelectorString(handledServices, true)
  }

  protected def getServiceInstance(serviceName: String, serviceVersion: String): Option[IRemoteService] = {
    ServiceRegistry.getService(serviceName, serviceVersion)
  }

  /* Private methods */
  private def handleMessage(session: Session, message: Message, replyProducer: MessageProducer) {
    assert((session != null), "handleMessage() session is null")
    assert((message != null), "handleMessage() message is null")
    assert((replyProducer != null), "handleMessage() replyProducer is null")

    val jmsMessageId = message.getJMSMessageID

    logger.debug("Handling Request JMS Message [" + jmsMessageId + ']')

    var jsonRequestId: java.lang.Object = null
    var jsonResponse: JSONRPC2Response = new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, jsonRequestId)
    var serviceName: String = null

    try {

      if (message.isInstanceOf[TextMessage]) {
        val requestString = message.asInstanceOf[TextMessage].getText

        /* Parse JSON Request */
        val jsonRequest = JSONRPC2Request.parse(requestString)

        jsonRequestId = jsonRequest.getID

        jsonResponse.setID(jsonRequestId)

        serviceName = message.getStringProperty(JMSConstants.PROLINE_SERVICE_NAME_KEY)
        val serviceVersion = message.getStringProperty(JMSConstants.PROLINE_SERVICE_VERSION_KEY)

        if (StringUtils.isEmpty(serviceName)) {
          /* Cannot occur if 'selectorString' is a valid filter for JMS Messages */
          val errorMessage = "Invalid \"" + JMSConstants.PROLINE_SERVICE_NAME_KEY + "\" property"
          logger.warn(errorMessage)

          jsonResponse.setError(buildJSONRPC2Error(JMSConstants.MESSAGE_ERROR_CODE, errorMessage))
        } else {
          val optionalServiceInstance = getServiceInstance(serviceName, serviceVersion)

          if (optionalServiceInstance.isDefined) {
            val jmsMessageContext = buildJMSMessageContext(message)

            jsonResponse = callService(jmsMessageContext, optionalServiceInstance.get, jsonRequest)
          } else {
            /* Cannot occur if 'selectorString' is a valid filter for JMS Messages */
            val errorMessageBuilder = new StringBuilder()
            errorMessageBuilder.append("Unknown \"").append(JMSConstants.PROLINE_SERVICE_NAME_KEY).append("\" [")
            errorMessageBuilder.append(serviceName).append("]  for ")

            if (StringUtils.isEmpty(serviceVersion)) {
              errorMessageBuilder.append("default version")
            } else {
              errorMessageBuilder.append("version [").append(serviceVersion).append(']')
            }

            val errorMessage = errorMessageBuilder.toString
            logger.warn(errorMessage)

            jsonResponse.setError(buildJSONRPC2Error(JMSConstants.MESSAGE_ERROR_CODE, errorMessage))
          } // End if (serviceName map a valid ServiceInstance)

        } // End if (serviceName property is valid)

      } else {
        val errorMessage = "Invalid Request JMS Message type"
        logger.warn(errorMessage)

        jsonResponse.setError(buildJSONRPC2Error(JMSConstants.MESSAGE_ERROR_CODE, errorMessage))
      } // End if (JMS Message is a TextMessage)

    } catch {

      /* Catch all Throwables */
      case t: Throwable => {
        val errorMessage = "Error handling Request JMS Message [" + jmsMessageId + ']'
        logger.error(errorMessage, t)

        jsonResponse = new JSONRPC2Response(buildJSONRPC2Error(JMSConstants.MESSAGE_ERROR_CODE, errorMessage, t), jsonRequestId)
      }

    } finally {

      /* In all cases, try to send a JSON Response to the JMS source Client */
      val replyDestination = message.getJMSReplyTo

      if (replyDestination == null) {
        logger.warn("Request JMS Message has no JMSReplyTo Destination : Cannot send JSON Response to Client")
      } else {

        if (jsonResponse == null) {
          jsonResponse = new JSONRPC2Response(JSONRPC2Error.INTERNAL_ERROR, jsonRequestId)
        }

        /* Notify */
        val serviceEvent = if (jsonResponse.getError == null) {

          if (jsonResponse.getResult == null) {
            new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_FAIL)
          } else {
            new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_SUCCESS)
          }

        } else {
          new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_FAIL)
        }

        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        val responseJMSMessage = session.createTextMessage()
        responseJMSMessage.setJMSCorrelationID(jmsMessageId)
        responseJMSMessage.setText(jsonResponse.toJSONString())

        logger.debug("Sending JMS Response to Request JMS Message [" + jmsMessageId + "] on Destination [" + replyDestination + ']')

        replyProducer.send(replyDestination, responseJMSMessage)
        logger.info("JMS Response to Request JMS Message [" + jmsMessageId + "] sent")
      }

    }

  }

  private def callService(jmsMessageContext: Map[String, Any], serviceInstance: IRemoteService, jsonRequest: JSONRPC2Request): JSONRPC2Response = {
    assert((serviceInstance != null), "callService() serviceInstance is null")
    assert((jsonRequest != null), "callService() jsonRequest is null")

    val serviceName = serviceInstance.getClass.getName

    val jsonRequestId = jsonRequest.getID

    if (Thread.interrupted()) {
      val errorMessage = "Thread interrupted before calling Service [" + serviceName + ']'
      logger.warn(errorMessage)

      new JSONRPC2Response(buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, errorMessage), jsonRequestId)
    } else {

      try {
        logger.debug("Calling Service [" + serviceName + "] with JSON Request [" + jsonRequest + ']')

        /* Notify */
        var jmsMessageId: String = null

        val value = jmsMessageContext.getOrElse(JMSConstants.JMS_MESSAGE_ID_KEY, null)
        if (value.isInstanceOf[String]) {
          jmsMessageId = value.asInstanceOf[String]
        }

        val serviceEvent = new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_START)

        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        serviceInstance.service(jmsMessageContext, jsonRequest)
      } catch {

        case intEx: InterruptedException => {
          val errorMessage = "Thread interrupted running Service [" + serviceName + ']'
          logger.warn(errorMessage, intEx)

          new JSONRPC2Response(buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, errorMessage, intEx), jsonRequestId)
        }

        /* Catch all Throwables */
        case t: Throwable => {
          val errorMessage = "Error calling Service [" + serviceName + ']'
          logger.error(errorMessage, t)

          new JSONRPC2Response(buildJSONRPC2Error(JMSConstants.SERVICE_ERROR_CODE, errorMessage, t), jsonRequestId)
        }

      }

    } // End if (Thread is not interrupted)

  }

}

class SingleThreadedServiceRunner(queue: Queue, connection: Connection, serviceMonitoringNotifier: MonitoringTopicPublisherRunner, serviceIdent : String, useThreadIdent : Boolean = false)
  extends ServiceRunner(queue, connection, serviceMonitoringNotifier) {

  import ServiceRunner._

  /* Constructor checks */
  require(!StringUtils.isEmpty(serviceIdent), "Invalid single thread service identification ")

  val handledServices = retrieveHandledServices()

  require(((handledServices != null) && !handledServices.isEmpty), "No SingleThreadedServices for  [" + serviceIdent + ']')

  protected override def buildSelectorString(): String = {
    /* NON-Parallelizable ServiceRunner */
   buildConcreteSelectorString(handledServices, false)
  }

  /* Private methods */
  private def retrieveHandledServices(): List[IRemoteService] = {
    val singleThreadedServicesPerName = if(useThreadIdent) { ServiceRegistry.getSingleThreadedServicesByThreadIdent() } 
                                            else { ServiceRegistry.getSingleThreadedServices()   }
    
    singleThreadedServicesPerName.getOrElse(serviceIdent, null)
  }

}

