package fr.proline.cortex

object Constants {

  val MAX_PORT = 65535

  /* JMS Standard header Property keys (JMS_ ) */
  val JMS_MESSAGE_ID_KEY = "JMSMessageID"
  val JMS_CORRELATION_ID_KEY = "JMSCorrelationID"
  val JMS_TIMESTAMP_KEY = "JMSTimestamp"
  val JMS_DESTINATION_KEY = "JMSDestination"
  val JMS_REPLY_TO_KEY = "JMSReplyTo"

  /* JMS Message Property keys (Proline_ ) */
  val PROLINE_NODE_ID_KEY = "Proline_NodeId"
  val PROLINE_SERVICE_NAME_KEY = "Proline_ServiceName"
  val PROLINE_SERVICE_VERSION_KEY = "Proline_ServiceVersion"

}
