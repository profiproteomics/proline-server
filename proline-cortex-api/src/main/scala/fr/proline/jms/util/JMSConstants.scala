package fr.proline.jms.util

import javax.jms.MessageHeader.{ Header => JmsHeader }

// TODO: move to another package because it is not related to utilities but rather to Proline/JMS API
object JMSConstants {

  val MAX_JMS_SERVER_PORT = 65535
  
  /* Monitoring Topic names */
  val SERVICE_MONITORING_NOTIFICATION_TOPIC_NAME = "ProlineServiceMonitoringNotificationTopic" 

  /* JMS Standard MessageHeader Property keys (JMS_ ) */
  val JMSCorrelationID = JmsHeader.JMSCorrelationID.toString()
  val JMSCorrelationIDAsBytes = JmsHeader.JMSCorrelationIDAsBytes.toString()
  val JMSDeliveryMode = JmsHeader.JMSDeliveryMode.toString()
  val JMSDeliveryTime = JmsHeader.JMSDeliveryTime.toString()
  val JMSDestination = JmsHeader.JMSDestination.toString()
  val JMSExpiration = JmsHeader.JMSExpiration.toString()
  val JMSMessageID = JmsHeader.JMSMessageID.toString()
  val JMSPriority = JmsHeader.JMSPriority.toString()
  val JMSRedelivered = JmsHeader.JMSRedelivered.toString()
  val JMSReplyTo = JmsHeader.JMSReplyTo.toString()
  val JMSTimestamp = JmsHeader.JMSTimestamp.toString()
  val JMSType = JmsHeader.JMSType.toString()
  
  @deprecated  (message = " use the Standard Key 'JMSCorrelationID' instead", since = "ProlineSuite 2.0")
  val JMS_CORRELATION_ID_KEY = JMSCorrelationID
  @deprecated (message = " use the Standard Key 'JMSDestination' instead", since = "ProlineSuite 2.0")
  val JMS_DESTINATION_KEY = JMSDestination
  @deprecated (message = " use the Standard Key 'JMSMessageID' instead", since = "ProlineSuite 2.0")
  val JMS_MESSAGE_ID_KEY = JMSMessageID
  @deprecated (message = " use the Standard Key 'JMSMessageID' JMSReplyTo", since = "ProlineSuite 2.0")
  val JMS_REPLY_TO_KEY = JMSReplyTo
  @deprecated  (message = " use the Standard Key 'JMSTimestamp' JMSReplyTo", since = "ProlineSuite 2.0")
  val JMS_TIMESTAMP_KEY = JMSTimestamp

  /* JMS Message Property keys (Proline_ ) */
  val PROLINE_NODE_ID_KEY = "Proline_NodeId"
  val PROLINE_SERVICE_NAME_KEY = "Proline_ServiceName"
  val PROLINE_SERVICE_PROJECT_ID_KEY = "Proline_Service_ProjectId"
  val PROLINE_SERVICE_VERSION_KEY = "Proline_ServiceVersion"
  val PROLINE_SERVICE_SOURCE_KEY = "Proline_ServiceSource"
  val PROLINE_SERVICE_DESCR_KEY = "Proline_ServiceDescription"  


  /* JSON-RPC Proline custom error codes */
  val MESSAGE_ERROR_CODE = -32001
  val SERVICE_ERROR_CODE = -32002
  val EXPIRED_MSG_ERROR_CODE = -32003
  val CANCELLED_MSG_ERROR_CODE = -32004
}

