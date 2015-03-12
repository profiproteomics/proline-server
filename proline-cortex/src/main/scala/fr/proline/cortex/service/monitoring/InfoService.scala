package fr.proline.cortex.service.monitoring

import java.net.NetworkInterface
import java.util.UUID

import scala.Array.canBuildFrom

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.util.StringUtils.LINE_SEPARATOR
import fr.proline.cortex.NodeConfig
import fr.proline.cortex.ServiceRegistry
import fr.proline.cortex.service.IRemoteService
import fr.proline.util.version.VersionHelper

/**
 * Simple "info" Service : return "OK" on "test" method and an info string (instance and host IP) an any other method call.
 */
class InfoService extends IRemoteService with Logging {

  /* Constants */
  val SHORT_TAB = "  "

  /* JMS Service identification */
  val serviceName = "proline/monitoring/Info"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  /* Uniquely identify this instance */
  val instanceUniqueIdentifier = UUID.randomUUID().toString

  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {
    require((req != null), "Req is null")

    val requestId = req.getID
    val method = req.getMethod

    /* Method dispatcher */
    method match {
      case "test" => return new JSONRPC2Response("OK", requestId)

      case "version" => {
        val buff = new StringBuilder()
        appendVersions(buff)

        return new JSONRPC2Response(buff.toString, requestId)
      }

      case "error" => throw new RuntimeException("Fake Exception thrown by " + getClass.getSimpleName)

      case _       => return  new JSONRPC2Response(buildMessage(jmsMessageContext, requestId, method), requestId)
    }

  }

  private def buildMessage(jmsMessageContext: Map[String, Any], requestId: java.lang.Object, methodName: String): String = {
    val buff = new StringBuilder()

    /* This Node and Network infos */
    buff.append("Proline-Cortex JMS NODE_ID : ").append(NodeConfig.NODE_ID)
    buff.append(LINE_SEPARATOR)

    appendNetwork(buff)

    buff.append(LINE_SEPARATOR)

    /* Current Service instance and Thread */
    buff.append(getClass.getSimpleName).append(" instance UUID : ").append(instanceUniqueIdentifier)
    buff.append(LINE_SEPARATOR)

    val currentThread = Thread.currentThread
    buff.append("Thread #").append(currentThread.getId).append("  [").append(currentThread.getName).append(']')
    buff.append(LINE_SEPARATOR)

    buff.append(LINE_SEPARATOR)

    /* JMS Message Context */
    appendJMSMessageContext(buff, jmsMessageContext)
    buff.append(LINE_SEPARATOR)

    /* JSON Request infos */
    buff.append("JSON Request Id ")
    append(buff, requestId)
    buff.append(LINE_SEPARATOR)

    buff.append("JSON Request Method ")
    append(buff, methodName)
    buff.append(LINE_SEPARATOR)

    buff.append(LINE_SEPARATOR)

    /* Proline Module Versions */
    appendVersions(buff)

    buff.append(LINE_SEPARATOR)

    /* List all handled services : SingleThreaded and Parallelizable */

    val handledSingleThreadedServices = ServiceRegistry.getSingleThreadedServices
    if ((handledSingleThreadedServices == null) || handledSingleThreadedServices.isEmpty) {
      buff.append("NO SingleThreaded Services handled")
      buff.append(LINE_SEPARATOR)
    } else {
      buff.append("Handled SingleThreaded Services :")
      buff.append(LINE_SEPARATOR)

      for (entry <- handledSingleThreadedServices) {
        val servicesList = entry._2

        if ((servicesList != null) && !servicesList.isEmpty) {
          for (service <- servicesList) {
            buff.append(SHORT_TAB)
            appendService(buff, service)
            buff.append(LINE_SEPARATOR)
          } // End inner loop
        }

      }

    }

    buff.append(LINE_SEPARATOR)

    val handledParallelizableServices = ServiceRegistry.getParallelizableServices
    if ((handledParallelizableServices == null) || handledParallelizableServices.isEmpty) {
      buff.append("NO Parallelizable Services handled")
      buff.append(LINE_SEPARATOR)
    } else {
      buff.append("Handled Parallelizable Services :")
      buff.append(LINE_SEPARATOR)

      for (service <- handledParallelizableServices) {
        buff.append(SHORT_TAB)
        appendService(buff, service)
        buff.append(LINE_SEPARATOR)
      }

    }

    buff.toString
  }

  private def appendNetwork(sb: StringBuilder) {
    assert((sb != null), "appendNetwork() sb is null")

    try {

      val interfaces = NetworkInterface.getNetworkInterfaces
      if (interfaces != null) {

        while (interfaces.hasMoreElements) {
          val interface = interfaces.nextElement

          if (interface.getInetAddresses.hasMoreElements) {
            sb.append(SHORT_TAB)
            appendInterface(sb, interface)
            sb.append(LINE_SEPARATOR)
          }

        } // End loop for each interface

      } // End if (interfaces is not null)

    } catch {
      case ex: Exception => logger.error("Unable to retrieve Networking info of current Host", ex)
    }

  }

  private def appendInterface(sb: StringBuilder, intf: NetworkInterface) {
    assert((sb != null), "appendInterface() sb is null")
    assert((intf != null), "appendInterface() intf is null")

    sb.append('\"').append(intf.getName).append('\"')

    val mac = intf.getHardwareAddress
    if ((mac != null) && (mac.length > 0)) {
      sb.append(' ').append(formatMac(mac))
    }

    val addresses = intf.getInetAddresses

    var first: Boolean = true

    while (addresses.hasMoreElements) {

      if (first) {
        first = false
        sb.append(SHORT_TAB)
      } else {
        sb.append(", ")
      }

      val address = addresses.nextElement

      val rawIpAddress = address.getHostAddress
      sb.append(rawIpAddress)

      val canonicalHostName = address.getCanonicalHostName

      if (!rawIpAddress.equals(canonicalHostName)) {
        sb.append(" [").append(canonicalHostName).append(']')
      }

    } // End loop for each InetAddress

  }

  private def formatMac(mac: Array[Byte]): String = {
    assert((mac != null), "formatMac() mac Array is null")

    mac.map("%02X".format(_)).mkString("<", ":", ">")
  }

  private def appendJMSMessageContext(sb: StringBuilder, jmsMessageContext: Map[String, Any]) {
    assert((sb != null), "appendJMSMessageContext() sb is null")
    assert((jmsMessageContext != null), "appendJMSMessageContext() jmsMessageContext Map is null")

    sb.append("Request JMS Message Properties :")
    sb.append(LINE_SEPARATOR)

    for (entry <- jmsMessageContext) {
      sb.append('\"').append(entry._1).append("\" : ")

      val value = entry._2

      if (value == null) {
        sb.append("NULL")
      } else {
        val clazz = value.getClass

        if (clazz != null) {
          sb.append(clazz.getName).append(SHORT_TAB)
        }

        val isString = value.isInstanceOf[String]
        if (isString) {
          sb.append('[')
        }

        sb.append(value)

        if (isString) {
          sb.append(']')
        }

      }

      sb.append(LINE_SEPARATOR)
    }

  }

  private def appendVersions(sb: StringBuilder) {
    assert((sb != null), "appendVersions() sb is null")

    sb.append("Proline Module Versions")
    sb.append(LINE_SEPARATOR)

    val versions = VersionHelper.getVersions

    sb.append("Number of IVersion services : ").append(versions.length)
    sb.append(LINE_SEPARATOR)

    if (!versions.isEmpty) {

      for (v <- versions) {
        sb.append(v.getClass.getName).append("  Module: ").append(v.getModuleName).append("  Version: ").append(v.getVersion)
        sb.append(LINE_SEPARATOR)
      }

    }

  }

  private def appendService(sb: StringBuilder, service: IRemoteService) {
    assert((sb != null), "appendService() sb is null")
    assert((service != null), "appendService() service is null")

    append(sb, service.serviceName)
    sb.append(" version ")
    append(sb, service.serviceVersion)

    if (service.defaultVersion) {
      sb.append(" *")
    }

  }

  private def append(sb: StringBuilder, obj: AnyRef) {
    assert((sb != null), "append() sb is null")

    if (obj == null) {
      sb.append("NULL")
    } else {
      sb.append('[').append(obj).append(']')
    }

  }

}
