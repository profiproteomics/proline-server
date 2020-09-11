package fr.proline.jms.util

import java.util.concurrent.LinkedBlockingQueue

import javax.jms.Connection
import javax.jms.JMSException
import javax.jms.Session


import com.thetransactioncompany.jsonrpc2.JSONRPC2Message
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import JMSConstants._

trait IServiceMonitoringNotifier {

  /**
   * {{{content}}} must not be empty.
   * Only set {{{jmsCorrelationId}}} if it is a direct response to a Request JMS Message, not to notify service execution.
   */
  def sendNotification(content: String, jmsCorrelationId: String)

  /**
   * {{{jsonRPCContent}}} must not be {{{null}}}.
   * Only set {{{jmsCorrelationId}}} if it is a direct response to a Request JMS Message, not to notify service execution.
   */
  def sendNotification(jsonRPCContent: JSONRPC2Message, jmsCorrelationId: String) {
    require((jsonRPCContent != null), "JsonRPCContent is null")

    sendNotification(jsonRPCContent.toJSONString(), jmsCorrelationId)
  }

}

class MonitoringTopicPublisherRunner(connection: Connection) extends IServiceMonitoringNotifier with Runnable with LazyLogging {

  // No need to synchronize on BlockingQueue (Memory consistency effects with all concurrent collections)
  private val m_pendingTopicMessages = new LinkedBlockingQueue[TopicMessage]()

  /* Constructor checks */
  require((connection != null), "Connection is null")

  /* Concrete Runnable.run() method */
  def run() {
    val currentThread = Thread.currentThread

    if (!currentThread.getUncaughtExceptionHandler.isInstanceOf[ThreadLogger]) {
      currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))
    }



    // Not transacted, AUTO_ACKNOWLEDGE
    val session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE)

    val prolineNotificationTopic = session.createTopic(SERVICE_MONITORING_NOTIFICATION_TOPIC_NAME)

    try {
      val topicPublisher = session.createProducer(prolineNotificationTopic)

      logger.info("Entering Notification Topic Publisher send loop [" + currentThread.getName + ']'+" for topic "+prolineNotificationTopic)

      val nodeId = NodeConfig.NODE_ID

      /* Infinite loop consuming JMS messages */
      var goOn: Boolean = true // Optimistic initialization

      while (goOn) {

        try {
          val topicMessage = m_pendingTopicMessages.take() // Blocking

          val message = session.createTextMessage()

          if (!StringUtils.isEmpty(topicMessage.jmsCorrelationId)) {
            message.setJMSCorrelationID(topicMessage.jmsCorrelationId)
          }

          /* Alway set nodeId Property on broadcast messages */
          message.setStringProperty(PROLINE_NODE_ID_KEY, nodeId)

          message.setText(topicMessage.content)

          try {
            topicPublisher.send(message)
          } catch {

            case jmsEx: JMSException => {
              goOn = false
              logger.error("Error sending Notification Topic Message", jmsEx)
            }

          }

        } catch {

          case intEx: InterruptedException => {
            goOn = false
            logger.warn("Topic Publisher Monitoring was interrupted")
          }

          /* Catch all Throwables to run INFINITE loop */
          case t: Throwable => logger.error("Error running Notification Topic Publisher send loop", t)

        }

      } // End infinite loop on topicMessage send

      logger.info("Exiting MonitoringTopicPublisherRunner loop")
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

  /**
   * Send is asynchrone.
   */
  def sendNotification(content: String, jmsCorrelationId: String) {
    require(!StringUtils.isEmpty(content), "Invalid content")

    val topicMessage = new TopicMessage(content, jmsCorrelationId)
    m_pendingTopicMessages.put(topicMessage)
  }

}

/* Store Topic Message to send in BlockingQueue */
case class TopicMessage(content: String, jmsCorrelationId: String) {

  /* Constructor checks */
  require(!StringUtils.isEmpty(content), "Invalid content")

}
