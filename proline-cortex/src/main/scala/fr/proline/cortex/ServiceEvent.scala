package fr.proline.cortex

import java.util.Date

import scala.collection.JavaConversions._
import scala.collection.mutable

import com.thetransactioncompany.jsonrpc2.JSONRPC2Notification

import fr.profi.util.StringUtils

object ServiceEvent {

  /* Constants */
  val NOTIFY_METHOD_NAME = "notify_service_event"

  val EVENT_TIMESTAMP_KEY = "event_timestamp"

  val REQUEST_JMS_MESSAGE_ID_KEY = "request_jms_message_id"

  val JSON_RPC_REQUEST_ID_KEY = "json_rpc_request_id"

  val SERVICE_NAME_KEY = "service_name"

  val EVENT_TYPE = "event_type"

  val EVENT_START = "Start"

  val EVENT_SUCCESS = "Success"

  val EVENT_FAIL = "Fail"

}

case class ServiceEvent(requestJMSMessageId: String, jsonRPCRequestId: java.lang.Object, serviceName: String, eventType: String) {

  import ServiceEvent._

  /* Constructor checks */
  require(!StringUtils.isEmpty(requestJMSMessageId), "Invalid requestJMSMessageId")

  require(!StringUtils.isEmpty(eventType), "Invalid eventType")

  private val m_eventTimestamp = new Date()

  def getEventTimestamp() = {
    (m_eventTimestamp.clone()).asInstanceOf[Date]
  }

  def toJSONRPCNotification(): JSONRPC2Notification = {
    val namedParams = mutable.Map.empty[String, java.lang.Object]
    namedParams.put(EVENT_TIMESTAMP_KEY, java.lang.Long.valueOf(getEventTimestamp.getTime))
    namedParams.put(REQUEST_JMS_MESSAGE_ID_KEY, requestJMSMessageId)

    if (jsonRPCRequestId != null) {
      namedParams.put(JSON_RPC_REQUEST_ID_KEY, jsonRPCRequestId)
    }

    if (!StringUtils.isEmpty(serviceName)) {
      namedParams.put(SERVICE_NAME_KEY, serviceName)
    }

    namedParams.put(EVENT_TYPE, eventType)

    val notification = new JSONRPC2Notification(NOTIFY_METHOD_NAME)
    notification.setNamedParams(namedParams)

    notification
  }

}
