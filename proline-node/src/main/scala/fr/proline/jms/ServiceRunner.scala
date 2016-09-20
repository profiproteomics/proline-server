package fr.proline.jms

import java.util.concurrent.Future

import scala.collection.mutable

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.jms.service.api.IRemoteBytesMsgService
import fr.proline.jms.service.api.IRemoteJsonRPC2Service
import fr.proline.jms.service.api.IRemoteServiceIdentity
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.jms.util.IServiceMonitoringNotifier
import fr.proline.jms.util.JMSConstants._
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
  def buildJSONRPC2Error(code: Int, baseErrorMessage: String, t: Throwable = null): JSONRPC2Error = {
    require(!StringUtils.isEmpty(baseErrorMessage), "Invalid baseErrorMessage")

    if (t == null) {
      new JSONRPC2Error(code, baseErrorMessage)
    } else {
      /* Put full stack trace string in {{{JSONRPC2Error}}} data field */
      new JSONRPC2Error(code, baseErrorMessage + " : " + t, t.getStackTraceString)
    }

  }

  def buildConcreteSelectorString(handledServices: Seq[IRemoteServiceIdentity], parallelizableRunner: Boolean): String = {
    require((handledServices != null), "HandledServices List is null")

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
    require((message != null), "Message is null")

    val mutableMap = mutable.Map.empty[String, Any]

    /* Add some Standard JMS header proprties */
    val jmsMessageId = message.getJMSMessageID
    mutableMap.put(JMS_MESSAGE_ID_KEY, jmsMessageId) // Should not be null on message Reception

    val jmsCorrelationId = message.getJMSCorrelationID
    if (jmsCorrelationId != null) {
      mutableMap.put(JMS_CORRELATION_ID_KEY, jmsCorrelationId)
    }

    val jmsTimestamp = message.getJMSTimestamp
    mutableMap.put(JMS_TIMESTAMP_KEY, jmsTimestamp) // Long primitive

    val jmsDestination = message.getJMSDestination
    mutableMap.put(JMS_DESTINATION_KEY, jmsDestination) // Should not be null on message Reception

    val jmsReplyTo = message.getJMSReplyTo
    if (jmsReplyTo != null) {
      mutableMap.put(JMS_REPLY_TO_KEY, jmsReplyTo)
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
 * Builds JMS Consumer to run {{{IRemoteServiceIdentity}}} on given JMS {{{Queue}}}.
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

  protected def getServiceInstance(serviceName: String, serviceVersion: String): Option[IRemoteServiceIdentity] = {
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
    var serviceVersion: String = null
    val isTxtMsg = message.isInstanceOf[TextMessage]
    var requestString : String = null
    
    try {

      if (isTxtMsg || message.isInstanceOf[BytesMessage]) {
                     
        val jmsMsgId = message.getJMSMessageID
        var jsonRequest : JSONRPC2Request = null;
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
          val errorMessage = "Invalid \"" + PROLINE_SERVICE_NAME_KEY + "\" property"
          logger.warn(errorMessage)
          jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR_CODE, errorMessage))
          
        } else {
           logger.debug("----- Search for Future for runnable "+this)
            val currentF: Future[_] = ServiceManager.getRunnableFuture(this)
            if (currentF!=null) {
              logger.debug("----- Found  for Future "+currentF+" Add with MsgID "+jmsMsgId)
              ServiceManager.addMsg2FutureEntry(jmsMsgId,currentF)
            }
           
          val optionalServiceInstance = getServiceInstance(serviceName, serviceVersion)
         
          if (optionalServiceInstance.isDefined) {
            val jmsMessageContext = buildJMSMessageContext(message)

            if(isTxtMsg){
              jsonResponse = callService(jmsMessageContext, optionalServiceInstance.get.asInstanceOf[IRemoteJsonRPC2Service], jsonRequest)
            } else { 
              jsonResponse = callBytesMsgService(jmsMessageContext, message.asInstanceOf[BytesMessage], optionalServiceInstance.get.asInstanceOf[IRemoteBytesMsgService])
            }
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
            val logErrorMessageBuilder = new StringBuilder()
            logErrorMessageBuilder.append("##Message##_").append(jmsMessageId).append(" ").append(errorMessage)
            logger.warn(logErrorMessageBuilder.toString())

            jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR_CODE, errorMessage))
          } // End if (serviceName map a valid ServiceInstance)
          
          if(currentF!=null){
            logger.debug("----- Remove link to message "+jmsMsgId+" for Future "+currentF)
            ServiceManager.removeMsg2Future(jmsMsgId)
          }

        } // End if (serviceName property is valid)
        
      } else {
        val errorMessage = "Invalid Request JMS Message type"
        val logErrorMessageBuilder = new StringBuilder()
        logErrorMessageBuilder.append("##Message##_").append(jmsMessageId).append(" ").append(errorMessage)
        logger.warn(logErrorMessageBuilder.toString())

        jsonResponse.setError(buildJSONRPC2Error(MESSAGE_ERROR_CODE, errorMessage))
      } // End if (JMS Message is a TextMessage)

    } catch {

      /* Catch all Throwables */
      case t: Throwable => {
        val errorMessage = "##Message##_"+jmsMessageId+" Error handling Request JMS Message"
        logger.error(errorMessage, t)

        jsonResponse = new JSONRPC2Response(buildJSONRPC2Error(MESSAGE_ERROR_CODE, errorMessage, t), jsonRequestId)
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
        val serviceSource = message.getStringProperty(PROLINE_SERVICE_SOURCE_KEY)
        val serviceSourceOp = if  (StringUtils.isNotEmpty(serviceSource)) Some(serviceSource) else None 

        val serviceDescr = message.getStringProperty(PROLINE_SERVICE_DESCR_KEY)        
        val serviceDescrOp = if  (StringUtils.isNotEmpty(serviceDescr)) Some(serviceDescr) else None 
        
        val serviceVersionOp = if (StringUtils.isNotEmpty(serviceVersion)) Some(serviceVersion) else None 

        val serviceEvent = if (jsonResponse.getError == null) {

          if (jsonResponse.getResult == null) {
            new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_FAIL, serviceVersionOp, serviceSourceOp, serviceDescrOp)
          } else {
            new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_SUCCESS, serviceVersionOp, serviceSourceOp, serviceDescrOp)
          }

        } else {
          new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_FAIL, serviceVersionOp, serviceSourceOp, serviceDescrOp)
        }
        
        if(isTxtMsg)
          serviceEvent.setComplementaryInfo(requestString)
          
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        val responseJMSMessage = session.createTextMessage()
        responseJMSMessage.setJMSCorrelationID(jmsMessageId)
        responseJMSMessage.setText(jsonResponse.toJSONString())

        logger.debug("Sending JMS Response to Request JMS Message [" + jmsMessageId + "] on Destination [" + replyDestination + ']')

        replyProducer.send(replyDestination, responseJMSMessage)
        logger.info("##Message##_"+jmsMessageId+" JMS Response to Request sent")
      }

    }

  }

  private def callBytesMsgService(jmsMessageContext: Map[String, Any], message: BytesMessage, serviceInstance: IRemoteBytesMsgService): JSONRPC2Response = {
    assert((serviceInstance != null), "callService() serviceInstance is null")
    
    val serviceName = serviceInstance.serviceName
    val jsonRequestId = message.getJMSMessageID

    if (Thread.interrupted()) {
      val errorMessage = "Thread interrupted before calling Service [" + serviceName + ']'
      logger.warn(errorMessage)

      new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, errorMessage), jsonRequestId)
    } else {

      try {
        var jmsMessageId: String = null
        
        val serviceVersion = jmsMessageContext.getOrElse(PROLINE_SERVICE_VERSION_KEY, "default") 
        val serviceVersionOp = if(StringUtils.isNotEmpty(serviceVersion.asInstanceOf[String])) Some(serviceVersion.asInstanceOf[String]) else None
        
        val serviceSourceOp =  if (jmsMessageContext.contains(PROLINE_SERVICE_SOURCE_KEY) ) {
          val serviceSource = jmsMessageContext.get(PROLINE_SERVICE_SOURCE_KEY).get.asInstanceOf[String]
           if( StringUtils.isNotEmpty(serviceSource)) Some(serviceSource) else None
        } else None
        
        val serviceDescrOp =  if (jmsMessageContext.contains(PROLINE_SERVICE_DESCR_KEY) ) {
          val serviceDescr = jmsMessageContext.get(PROLINE_SERVICE_DESCR_KEY).get.asInstanceOf[String]
           if( StringUtils.isNotEmpty(serviceDescr)) Some(serviceDescr) else None
        } else None


        val value = jmsMessageContext.getOrElse(JMS_MESSAGE_ID_KEY, null)
        if (value.isInstanceOf[String]) {
          jmsMessageId = value.asInstanceOf[String]
        }               

         logger.info(" Calling ##Message##_"+jmsMessageId +"_ . BytesMessage Service [" + serviceName + "] ")
         
        /* Notify */
        val serviceEvent = new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_START, serviceVersionOp, serviceSourceOp, serviceDescrOp)
//        serviceEvent.setComplementaryInfo()
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        serviceInstance.runService(message, jmsMessageContext)
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

  private def callService(jmsMessageContext: Map[String, Any], serviceInstance: IRemoteJsonRPC2Service, jsonRequest: JSONRPC2Request): JSONRPC2Response = {
    assert((serviceInstance != null), "callService() serviceInstance is null")
    assert((jsonRequest != null), "callService() jsonRequest is null")

    val serviceName = serviceInstance.serviceName

    val jsonRequestId = jsonRequest.getID

    if (Thread.interrupted()) {
      val errorMessage = "Thread interrupted before calling Service [" + serviceName + ']'
      logger.warn(errorMessage)

      new JSONRPC2Response(buildJSONRPC2Error(SERVICE_ERROR_CODE, errorMessage), jsonRequestId)
    } else {

      try {
        
        val serviceVersion = jmsMessageContext.getOrElse(PROLINE_SERVICE_VERSION_KEY, "default") 
        val serviceVersionOp = if(StringUtils.isNotEmpty(serviceVersion.asInstanceOf[String])) Some(serviceVersion.asInstanceOf[String]) else None
        val serviceSourceOp =  if (jmsMessageContext.contains(PROLINE_SERVICE_SOURCE_KEY) ) {
          val serviceSource = jmsMessageContext.get(PROLINE_SERVICE_SOURCE_KEY).get.asInstanceOf[String]
           if( StringUtils.isNotEmpty(serviceSource)) Some(serviceSource) else None
        } else None
        
        val serviceDescrOp =  if (jmsMessageContext.contains(PROLINE_SERVICE_DESCR_KEY) ) {
          val serviceDescr = jmsMessageContext.get(PROLINE_SERVICE_DESCR_KEY).get.asInstanceOf[String]
           if( StringUtils.isNotEmpty(serviceDescr)) Some(serviceDescr) else None
        } else None
        
        var jmsMessageId: String = null
        val value = jmsMessageContext.getOrElse(JMS_MESSAGE_ID_KEY, null)
        if (value.isInstanceOf[String]) {
          jmsMessageId = value.asInstanceOf[String]
        }
                
        logger.info(" Calling ##Message##_"+jmsMessageId +" Service [" + serviceName + "] with JSON Request [" + jsonRequest + ']')

        /* Notify */
        val serviceEvent = new ServiceEvent(jmsMessageId, jsonRequestId, serviceName, ServiceEvent.EVENT_START, serviceVersionOp,serviceSourceOp, serviceDescrOp)
        serviceEvent.setComplementaryInfo(jsonRequest.toJSONString())
        serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

        serviceInstance.runService(jsonRequest, jmsMessageContext)
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

class SingleThreadedServiceRunner(queue: Queue, connection: Connection, serviceMonitoringNotifier: MonitoringTopicPublisherRunner, serviceIdent: String, useThreadIdent: Boolean = false)
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
  private def retrieveHandledServices(): Seq[IRemoteServiceIdentity] = {
    val singleThreadedServicesByKey = if (useThreadIdent) { ServiceRegistry.getSingleThreadedServicesByThreadIdent() }
    else { ServiceRegistry.getSingleThreadedServicesByName() }

    singleThreadedServicesByKey.getOrElse(serviceIdent, Seq() )
  }

}

