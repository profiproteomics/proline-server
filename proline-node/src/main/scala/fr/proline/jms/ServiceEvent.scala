package fr.proline.jms

import java.util.Date
import scala.collection.JavaConversions._
import scala.collection.mutable
import com.thetransactioncompany.jsonrpc2.JSONRPC2Notification
import fr.profi.util.StringUtils
import javax.jms.TextMessage
import com.thetransactioncompany.jsonrpc2.JSONRPC2Message

object ServiceEvent {

  /* Constants */
  val NOTIFY_METHOD_NAME = "notify_service_event"

  val EVENT_TIMESTAMP_KEY = "event_timestamp"

  val REQUEST_JMS_MESSAGE_ID_KEY = "request_jms_message_id"

  val JSON_RPC_REQUEST_ID_KEY = "json_rpc_request_id"

  val SERVICE_NAME_KEY = "service_name"

  val SERVICE_VERSION_KEY = "service_version"

  val SERVICE_SOURCE_KEY = "service_source"

  val SERVICE_DESCR_KEY = "service_description"

  val SERVICE_MORE_INFO = "complementary_info"

  val EVENT_TYPE = "event_type"

  val EVENT_START = "Start"

  val EVENT_SUCCESS = "Success"

  val EVENT_FAIL = "Fail"
}

case class ServiceEvent(requestJMSMessageId: String, jsonRPCRequestId: java.lang.Object, serviceName: String, eventType: String, serviceVersion: Option[String] = None, 
  serviceSource: Option[String] = None, serviceDescription: Option[String] = None) {

  import ServiceEvent._

  /* Constructor checks */
  require(!StringUtils.isEmpty(requestJMSMessageId), "Invalid requestJMSMessageId")

  require(!StringUtils.isEmpty(eventType), "Invalid eventType")

  private var m_complementary_info: String = null
  private val m_eventTimestamp = new Date()

  def setComplementaryInfo(fullInfo: String) {
    m_complementary_info = fullInfo
  }

  def getComplementaryInfo = m_complementary_info;

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

    if (StringUtils.isNotEmpty(serviceName)) {
      namedParams.put(SERVICE_NAME_KEY, serviceName)
    }
    if (serviceSource.isDefined) {
      namedParams.put(SERVICE_SOURCE_KEY, serviceSource.get)
    }
    if (serviceVersion.isDefined) {
      namedParams.put(SERVICE_VERSION_KEY, serviceVersion.get)
    }
    if (serviceDescription.isDefined) {
      namedParams.put(SERVICE_DESCR_KEY, serviceDescription.get)
    }
    
    if (StringUtils.isNotEmpty(m_complementary_info))
      namedParams.put(SERVICE_MORE_INFO, m_complementary_info)

    namedParams.put(EVENT_TYPE, eventType)

    val notification = new JSONRPC2Notification(NOTIFY_METHOD_NAME)
    notification.setNamedParams(namedParams)

    notification
  }

}
