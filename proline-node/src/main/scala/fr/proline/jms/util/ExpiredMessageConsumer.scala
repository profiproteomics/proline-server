package fr.proline.jms.util

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging
import JMSConstants.EXPIRED_MSG_ERROR_CODE
import JMSConstants.PROLINE_SERVICE_NAME_KEY
import JMSConstants.PROLINE_SERVICE_SOURCE_KEY
import JMSConstants.PROLINE_SERVICE_VERSION_KEY
import fr.profi.util.ThreadLogger
import fr.proline.jms.ServiceEvent
import javax.jms.Connection
import javax.jms.JMSException
import javax.jms.Queue
import javax.jms.Session
import fr.profi.util.StringUtils
import javax.jms.TextMessage


class ExpiredMessageConsumer(
  queue: Queue, connection: Connection,
  serviceMonitoringNotifier: IServiceMonitoringNotifier
) extends Runnable with LazyLogging {
  
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
      val consumer = session.createConsumer(queue)

      /* ReplyProducer to send Response JMS Message to Client (Producer MUST be confined in current Thread) */
      val replyProducer = session.createProducer(null)
      logger.debug("Entering ExpiredMessage Consumer receive loop [" + currentThread.getName + ']')

      /* Infinite loop consuming JMS messages */
      var goOn: Boolean = true // Optimistic initialization

      while (goOn) {

        try {
          val jmsMessage = consumer.receive() // blocks indefinitely

          if (jmsMessage == null) {
            goOn = false
            logger.info("Consumer Connection is closed : exiting receive loop")
          } else {
            val jmsMessageId = jmsMessage.getJMSMessageID()
            val err: JSONRPC2Error = new JSONRPC2Error(EXPIRED_MSG_ERROR_CODE, "Message has expired.")
            val jsonResponse: JSONRPC2Response = new JSONRPC2Response(err, jmsMessageId)

            val replyDestination = jmsMessage.getJMSReplyTo()
            val serviceName = jmsMessage.getStringProperty(PROLINE_SERVICE_NAME_KEY)
            val serviceVersion = jmsMessage.getStringProperty(PROLINE_SERVICE_VERSION_KEY)
            val serviceVersionOp = if  (StringUtils.isNotEmpty(serviceVersion)) Some(serviceVersion) else None
            val serviceSource = jmsMessage.getStringProperty(PROLINE_SERVICE_SOURCE_KEY)
            val serviceSourceOp = if  (StringUtils.isNotEmpty(serviceSource)) Some(serviceSource) else None
            
            
            if (replyDestination == null) {
              logger.warn("Request JMS Message has no JMSReplyTo Destination : Cannot send JSON Response to Client")
            } else {

              /* Notify */
              val serviceEvent = new ServiceEvent(jsonResponse.getID.toString(), jmsMessageId, serviceName, ServiceEvent.EVENT_FAIL, serviceVersionOp, serviceSourceOp)
              if(jmsMessage.isInstanceOf[TextMessage])
                serviceEvent.setComplementaryInfo(jmsMessage.asInstanceOf[TextMessage].getText)
              serviceMonitoringNotifier.sendNotification(serviceEvent.toJSONRPCNotification(), null)

              val responseJMSMessage = session.createTextMessage()
              responseJMSMessage.setJMSCorrelationID(jmsMessageId)
              responseJMSMessage.setText(jsonResponse.toJSONString())

              logger.debug("Sending JMS Response to Request JMS Message [" + jmsMessageId + "] on Destination [" + replyDestination + ']')
              replyProducer.send(replyDestination, responseJMSMessage)
              logger.info("JMS Response to Request JMS Message [" + jmsMessageId + "] sent")

            }

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

}