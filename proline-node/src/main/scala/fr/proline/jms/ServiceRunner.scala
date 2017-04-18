package fr.proline.jms

import java.util.concurrent.Future

import scala.collection.mutable

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.profi.util.exception.SerializableStackTraceElement
import fr.proline.jms.service.api.IRemoteBytesMsgService
import fr.proline.jms.service.api.IRemoteJsonRPC2Service
import fr.proline.jms.service.api.IRemoteServiceIdentity
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.jms.util.IServiceMonitoringNotifier
import fr.proline.jms.util.JMSConstants.JMSCorrelationID
import fr.proline.jms.util.JMSConstants.JMSDestination
import fr.proline.jms.util.JMSConstants.JMSMessageID
import fr.proline.jms.util.JMSConstants.JMSReplyTo
import fr.proline.jms.util.JMSConstants.JMSTimestamp
import fr.proline.jms.util.JMSConstants.MESSAGE_ERROR_CODE
import fr.proline.jms.util.JMSConstants.PROLINE_NODE_ID_KEY
import fr.proline.jms.util.JMSConstants.PROLINE_SERVICE_DESCR_KEY
import fr.proline.jms.util.JMSConstants.PROLINE_SERVICE_NAME_KEY
import fr.proline.jms.util.JMSConstants.PROLINE_SERVICE_SOURCE_KEY
import fr.proline.jms.util.JMSConstants.PROLINE_SERVICE_VERSION_KEY
import fr.proline.jms.util.JMSConstants.SERVICE_ERROR_CODE
import fr.proline.jms.util.JMSConstants.CANCELLED_MSG_ERROR_CODE
import fr.proline.jms.util.MonitoringTopicPublisherRunner
import fr.proline.jms.util.NodeConfig
import javax.jms.BytesMessage
import javax.jms.Connection
import javax.jms.JMSException
import javax.jms.Message
import javax.jms.MessageProducer
import javax.jms.Queue
import javax.jms.Session
import javax.jms.TextMessage

object ServiceRunner extends LazyLogging {

  /* Constants */

  /* Static methods */
  def buildJSONRPC2Error(code: Int, t: Throwable): JSONRPC2Error = {
    
    val stackTrace = t.getStackTrace.map { traceElem =>
      SerializableStackTraceElement(traceElem)
    }
    
    new JSONRPC2Error(code, t.getMessage, stackTrace)
  }

  def buildConcreteSelectorString(handledServices: Seq[IRemoteServiceIdentity], parallelizableRunner: Boolean): String = {
    require(handledServices != null, "handledServices List is null")

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
          // TODO: DBO => why throwing a RuntimeException instead of an Exception ?
          throw new RuntimeException("Parallelizable ServiceRunner does not accept ISingleThreadedService")
        }

      }

      if (first) {
        first = false
      } else {
        buff.append("\n OR ")
      }

      buff.append("((").append(PROLINE_SERVICE_NAME_KEY)
      buff.append(" = \'").append(service.serviceName).append("\') AND (")
      buff.append(PROLINE_SERVICE_VERSION_KEY)
      buff.append(" = \'").append(service.serviceVersion).append("\'))")

      if (service.isDefaultVersion) {
        buff.append(" OR ")
        buff.append("((").append(PROLINE_SERVICE_NAME_KEY)
        buff.append(" = \'").append(service.serviceName).append("\') AND (")
        buff.append(PROLINE_SERVICE_VERSION_KEY)
        buff.append(" IS NULL))")
      }
      
    } // End loop for each service

    buff.toString
  }

  def buildJMSMessageContext(message: Message): Map[String, Any] = {
    require(message != null, "message is null")

    val mutableMap = mutable.Map.empty[String, Any]

    /* Add some Standard JMS header proprties */
    val jmsMessageId = message.getJMSMessageID
    mutableMap.put(JMSMessageID, jmsMessageId) // Should not be null on message Reception

    val jmsCorrelationId = message.getJMSCorrelationID
    if (jmsCorrelationId != null) {
      mutableMap.put(JMSCorrelationID, jmsCorrelationId)
    }

    val jmsTimestamp = message.getJMSTimestamp
    mutableMap.put(JMSTimestamp, jmsTimestamp) // Long primitive

    val jmsDestination = message.getJMSDestination
    mutableMap.put(JMSDestination, jmsDestination) // Should not be null on message Reception

    val jmsReplyTo = message.getJMSReplyTo
    if (jmsReplyTo != null) {
      mutableMap.put(JMSReplyTo, jmsReplyTo)
    }

    /* Add all other JMS properties (Provider and Proline specific) */
    val propertyNames = message.getPropertyNames

    while (propertyNames.hasMoreElements) {
      val elem = propertyNames.nextElement
      
      elem match {
        case propertyName: String => {
          val value = message.getObjectProperty(propertyName)
  
          val oldValue = mutableMap.put(propertyName, value)
          if (oldValue.isDefined) {
            logger.warn(s"JMS Message property [$propertyName] overridden")
          }
  
        }
        case _ => logger.warn("Invalid JMS Message property name")
      }

    }

    mutableMap.toMap
  }

}

/**
 * Builds JMS Consumer to run {{{IRemoteServiceIdentity}}} on given JMS {{{Queue}}}.
 */
class ServiceRunner(queue: Queue, connection: Connection, serviceMonitoringNotifier: IServiceMonitoringNotifier) extends Runnable with LazyLogging {

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
        throw new RuntimeException("No valid selector on [" + NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME + ']')
      }

      logger.debug(s"Consumer selector string:\n$selectorString")

      val consumer = session.createConsumer(queue, selectorString)

      /* ReplyProducer to send Response JMS Message to Client (Producer MUST be confined in current Thread) */
      val replyProducer = session.createProducer(null)

      logger.debug(s"Entering Consumer receive loop [${currentThread.getName}]")

      val resourceService = ServiceRegistry.resourceService
      val nodeId = NodeConfig.NODE_ID

      /* Infinite loop consuming JMS messages */
      var goOn = true // Optimistic initialization

      while (goOn) {

        try {
          val message = consumer.receive() // blocks indefinitely

          if (message == null) {
            goOn = false            
            logger.warn("Consumer connection is closed: exiting receive loop") 
          } else if (resourceService.serviceName.equals(message.getStringProperty(PROLINE_SERVICE_NAME_KEY)) &&
            nodeId.equals(message.getStringProperty(PROLINE_NODE_ID_KEY))) {
            /* Special ResourceService handling */
            resourceService.handleMessage(session, message, replyProducer, serviceMonitoringNotifier)
          } else {
            /* Normal Service Request handling */
            handleMessage(session, message, replyProducer)
          }

        } catch {
          /* Catch all Throwable to run INFINITE loop */
          case t: Throwable => logger.error("Got unexpected error while running the consumer reception loop", t)
        }

      } // End infinite loop on message receive

      logger.info("Exiting ServiceRunner loop")
      
    } finally {

      if (session != null) {
        try {
          session.close()
          logger.info("JMS Session has been successfully closed !")
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

  protected def getServiceInstance(serviceName: String, serviceVersion: String): Option[IRemoteServiceIdentity] = {
    ServiceRegistry.getService(serviceName, serviceVersion)
  }

  /* Private methods */
  private def handleMessage(session: Session, message: Message, replyProducer: MessageProducer) {
    require(session != null, "handleMessage() session is null")
    require(message != null, "handleMessage() message is null")
    require(replyProducer != null, "handleMessage() replyProducer is null")

    val jmsMessageId = message.getJMSMessageID

    logger.debug(s"Handling Request JMS Message [$jmsMessageId]")

    var jsonRequestId: java.lang.Object = null
    var jsonResponse: JSONRPC2Response = new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, jsonRequestId)
    
    var serviceName: String = null
    var serviceVersion: String = null
    var requestString : String = null
    val isTxtMsg = message.isInstanceOf[TextMessage]
    
    try {

      if (isTxtMsg || message.isInstanceOf[BytesMessage]) {

        val jmsMsgId = message.getJMSMessageID
        var jsonRequest: JSONRPC2Request = null
         if(isTxtMsg){
          requestString = message.asInstanceOf[TextMessage].getText
          jsonRequest = JSONRPC2Request.parse(requestString)
          jsonRequestId = jsonRequest.getID
        } else
          jsonRequestId = jmsMsgId
          
        
        jsonResponse.setID(jsonRequestId)

        serviceName = message.getStringProperty(PROLINE_SERVICE_NAME_KEY)
        serviceVersion = message.getStringProperty(PROLINE_SERVICE_VERSION_KEY)

        if (StringUtils.isEmpty(serviceName)) {
          /* Cannot occur if 'selectorString' is a valid filter for JMS Messages */
          val errorMessage = s"Invalid '$PROLINE_SERVICE_NAME_KEY' property"
          logger.error(errorMessage)
          jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR_CODE, new Exception(errorMessage)))

        } else {
          logger.debug("----- Search for Future for runnable " + this)
          val currentF: Future[_] = ServiceManager.getRunnableFuture(this)
          if (currentF != null) {
            logger.debug(s"----- Add new MsgID->Future mapping: $jmsMsgId -> $currentF")
            ServiceManager.addMsg2FutureEntry(jmsMsgId, currentF)
          }

          val serviceInstanceOpt = getServiceInstance(serviceName, serviceVersion)

          if (serviceInstanceOpt.isDefined) {
            val jmsMessageContext = buildJMSMessageContext(message)

            if (isTxtMsg) {
              jsonResponse = callTextMsgService(jmsMessageContext, jsonRequest, serviceInstanceOpt.get.asInstanceOf[IRemoteJsonRPC2Service])
            } else {
              jsonResponse = callBytesMsgService(jmsMessageContext, message.asInstanceOf[BytesMessage], serviceInstanceOpt.get.asInstanceOf[IRemoteBytesMsgService])
            }
          } else {
            /* Cannot occur if 'selectorString' is a valid filter for JMS Messages */
            val versionAsStr = if (StringUtils.isEmpty(serviceVersion)) "default version"
            else s"version [$serviceVersion]"

            val errorMessage = s"Unknown '$PROLINE_SERVICE_NAME_KEY' [$serviceName] for $versionAsStr"

            val logMessage = s"$errorMessage (##Message##_$jmsMessageId)"
            logger.error(logMessage)

            jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR_CODE, new Exception(errorMessage)))

          } // End if (serviceName map a valid ServiceInstance)

          if (currentF != null) {
            logger.debug(s"----- Remove link to Message $jmsMsgId for Future $currentF")
            ServiceManager.removeMsg2Future(jmsMsgId)
          }

        } // End if (serviceName property is valid)

      } else {
        val errorMessage = "Invalid request, unsupported JMS Message type"
        val logMessage = s"$errorMessage (##Message##_$jmsMessageId)"
        logger.error(logMessage)

        jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR_CODE, new Exception(errorMessage)))
      } // End if (JMS Message is a TextMessage)

    } catch {

      /* Catch all Throwables */
      case t: Throwable => {
        val errorMessage = s"Error handling Request JMS Message (##Message##_$jmsMessageId)"
        logger.error(errorMessage, t)

        jsonResponse = new JSONRPC2Response(buildJSONRPC2Error(MESSAGE_ERROR_CODE, t), jsonRequestId)
      }

    } finally {

      /* In all cases, try to send a JSON Response to the JMS source Client */
      val replyDestination = message.getJMSReplyTo

      if (replyDestination == null) {
        logger.warn("Request JMS Message has no 'JMSReplyTo' destination: cannot send JSON response to the client")
      } else {

        if (jsonResponse == null) {
          jsonResponse = new JSONRPC2Response(JSONRPC2Error.INTERNAL_ERROR, jsonRequestId)
        }

        /* Notify */
        val serviceSource = message.getStringProperty(PROLINE_SERVICE_SOURCE_KEY)
        val serviceSourceOpt = if  (StringUtils.isNotEmpty(serviceSource)) Some(serviceSource) else None 

        val serviceDescr = message.getStringProperty(PROLINE_SERVICE_DESCR_KEY)        
        val serviceDescrOpt = if  (StringUtils.isNotEmpty(serviceDescr)) Some(serviceDescr) else None 
        
        val serviceVersionOpt = if (StringUtils.isNotEmpty(serviceVersion)) Some(serviceVersion) else None 

        val serviceEvent = if (jsonResponse.indicatesSuccess) { // no error

          if (jsonResponse.getResult == null) {
            new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_FAIL, serviceVersionOpt, serviceSourceOpt, serviceDescrOpt)
          } else {
            new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_SUCCESS, serviceVersionOpt, serviceSourceOpt, serviceDescrOpt)
          }

        } else {
          new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_FAIL, serviceVersionOpt, serviceSourceOpt, serviceDescrOpt)
        }
        
        if(isTxtMsg)
          serviceEvent.setComplementaryInfo(requestString)
        
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        val responseJMSMessage = session.createTextMessage()
        responseJMSMessage.setJMSCorrelationID(jmsMessageId)
        responseJMSMessage.setText(jsonResponse.toJSONString())

        logger.debug(s"Sending JMS response for JMS request [$jmsMessageId] on destination [$replyDestination]")

        replyProducer.send(replyDestination, responseJMSMessage)
        logger.info(s"JMS response to request sent (##Message##_$jmsMessageId)")
      }

    }

  }
  
  private def callBytesMsgService(jmsMessageContext: Map[String, Any], message: BytesMessage, serviceInstance: IRemoteBytesMsgService): JSONRPC2Response = {
    require(message != null, "callBytesMsgService() message is null")
    require(serviceInstance != null, "callBytesMsgService() serviceInstance is null")
    
    this._callService(
      jmsMessageContext,
      message.getJMSMessageID,
      serviceInstance,
      s"Calling BytesMessage Service [${serviceInstance.serviceName}]",
      (serviceEvent: ServiceEvent) => {
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)
        serviceInstance.runService(message, jmsMessageContext)
      }
    )

  }
  
  private def callTextMsgService(jmsMessageContext: Map[String, Any], jsonRequest: JSONRPC2Request, serviceInstance: IRemoteJsonRPC2Service): JSONRPC2Response = {
    require(serviceInstance != null, "callService() serviceInstance is null")
    require(jsonRequest != null, "callService() jsonRequest is null")

    this._callService(
      jmsMessageContext,
      jsonRequest.getID,
      serviceInstance,
      s"Calling service [${serviceInstance.serviceName}] with JSON Request [$jsonRequest]",
      (serviceEvent: ServiceEvent) => {
        serviceEvent.setComplementaryInfo(jsonRequest.toJSONString())
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        serviceInstance.runService(jsonRequest, jmsMessageContext)
      }
    )

  }
  
  private def _getNonEmptyProperty(jmsMessageContext: Map[String, Any], propertyKey: String): Option[String] = {
    jmsMessageContext.get(propertyKey).map(_.asInstanceOf[String]).filter(StringUtils.isNotEmpty)
  }
  
  private def _callService(
    jmsMessageContext: Map[String, Any],
    jsonRequestId: AnyRef,
    serviceInstance: IRemoteServiceIdentity,
    loggingMessagePrefix: => String,
    serviceCallerFn: ServiceEvent => JSONRPC2Response
  ): JSONRPC2Response = {

    val serviceName = serviceInstance.serviceName

    if (Thread.interrupted()) {
      val errorMessage = s"Thread interrupted before calling Service [$serviceName]"
      logger.error(errorMessage)

      new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, new Exception(errorMessage)), jsonRequestId)
    } else {

      try {
        
        val serviceVersionOpt = _getNonEmptyProperty(jmsMessageContext, PROLINE_SERVICE_VERSION_KEY).orElse(Some("default"))
        val serviceSourceOpt = _getNonEmptyProperty(jmsMessageContext, PROLINE_SERVICE_SOURCE_KEY)
        val serviceDescrOpt = _getNonEmptyProperty(jmsMessageContext, PROLINE_SERVICE_DESCR_KEY)
        
        val jmsMessageIdValue = jmsMessageContext.getOrElse(JMSMessageID, null)
        val jmsMessageId = jmsMessageIdValue match {
          case str: String => str
          case _ => throw new Exception(s"Invalid JMS Message ID $jmsMessageIdValue")
        }
        
        val emptyLine = System.lineSeparator()+System.lineSeparator()
        logger.info(s"$emptyLine  $loggingMessagePrefix (##Message##_$jmsMessageId)")

        /* Notify */
        val serviceEvent = new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_START, serviceVersionOpt,serviceSourceOpt, serviceDescrOpt)
        serviceCallerFn(serviceEvent)
        
      } catch {

        case intEx: InterruptedException => {
          val errorMessage = s"Thread interrupted while running service [$serviceName]"
          logger.error(errorMessage, intEx)

          new JSONRPC2Response(buildJSONRPC2Error(CANCELLED_MSG_ERROR_CODE, intEx), jsonRequestId)
        }

        /* Catch all Throwables */
        case t: Throwable => {
          val errorMessage = s"An error occurred while calling service [$serviceName]"
          logger.error(errorMessage, t)

          new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, t), jsonRequestId)
        }

      }

    } // End else Thread is not interrupted

  }

}

class SingleThreadedServiceRunner(queue: Queue, connection: Connection, serviceMonitoringNotifier: MonitoringTopicPublisherRunner, serviceIdent: String, useThreadIdent: Boolean = false)
  extends ServiceRunner(queue, connection, serviceMonitoringNotifier) {

  import ServiceRunner._

  /* Constructor checks */
  require(!StringUtils.isEmpty(serviceIdent), "Invalid single thread service identification ")

  val handledServices = retrieveHandledServices()

  require(handledServices != null && !handledServices.isEmpty, s"No SingleThreadedService for [$serviceIdent]")

  protected override def buildSelectorString(): String = {
    /* NON-Parallelizable ServiceRunner */
    buildConcreteSelectorString(handledServices, false)
  }

  /* Private methods */
  private def retrieveHandledServices(): Seq[IRemoteServiceIdentity] = {
    val singleThreadedServicesByKey = if (useThreadIdent) { ServiceRegistry.getSingleThreadedServicesByThreadIdent() }
    else { ServiceRegistry.getSingleThreadedServicesByName() }

    singleThreadedServicesByKey.getOrElse(serviceIdent, Seq() )
  }

}

