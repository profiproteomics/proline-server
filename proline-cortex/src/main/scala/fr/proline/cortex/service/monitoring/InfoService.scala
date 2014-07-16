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
  val SMALL_TAB = "  "

  /* JMS Service identification */
  val serviceName = "proline/monitoring/Info"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  /* Uniquely identify this instance */
  val instanceUniqueIdentifier = UUID.randomUUID().toString

  override def process(req: JSONRPC2Request): JSONRPC2Response = {
    require(req != null, "Req is null")

    val requestId = req.getID
    val method = req.getMethod

    /* Method dispatcher */
    method match {
      case "test" => new JSONRPC2Response("OK", requestId)

      case "version" => {
        val buff = new StringBuilder()

        appendVersions(buff)

        new JSONRPC2Response(buff.toString, requestId)
      }

      case "error" => throw new RuntimeException("Fake Exception thrown by " + getClass.getSimpleName)

      case _       => new JSONRPC2Response(buildMessage(requestId, method), requestId)
    }

  }

  private def buildMessage(requestId: java.lang.Object, methodName: String): String = {
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
    if (!handledSingleThreadedServices.isEmpty) {
      buff.append("Handled SingleThreaded Services :")
      buff.append(LINE_SEPARATOR)

      for (entry <- handledSingleThreadedServices) {
        val servicesList = entry._2

        if ((servicesList != null) && !servicesList.isEmpty) {
          buff.append(SMALL_TAB)

          var first: Boolean = true

          for (service <- servicesList) {

            if (first) {
              first = false
            } else {
              buff.append(", ")
            }

            appendService(buff, service)
          }

          buff.append(LINE_SEPARATOR)
        }

      }

    }

    buff.append(LINE_SEPARATOR)

    val handledParallelizableServices = ServiceRegistry.getParallelizableServices
    if (!handledParallelizableServices.isEmpty) {
      buff.append("Handled Parallelizable Services :")
      buff.append(LINE_SEPARATOR)

      for (service <- handledParallelizableServices) {
        buff.append(SMALL_TAB)
        appendService(buff, service)
        buff.append(LINE_SEPARATOR)
      }

    }

    buff.toString
  }

  private def appendNetwork(sb: StringBuilder) {

    try {

      val interfaces = NetworkInterface.getNetworkInterfaces
      if (interfaces != null) {

        while (interfaces.hasMoreElements) {
          val interface = interfaces.nextElement()

          if (interface.getInetAddresses.hasMoreElements) {
            sb.append(SMALL_TAB)
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
        sb.append(SMALL_TAB)
      } else {
        sb.append(", ")
      }

      val address = addresses.nextElement()

      val rawIpAddress = address.getHostAddress
      sb.append(rawIpAddress)

      val canonicalHostName = address.getCanonicalHostName

      if (!rawIpAddress.equals(canonicalHostName)) {
        sb.append(" [").append(canonicalHostName).append(']')
      }

    } // End loop for each InetAddress

  }

  private def formatMac(mac: Array[Byte]): String = {
    mac.map("%02X".format(_)).mkString("<", ":", ">")
  }

  private def appendVersions(sb: StringBuilder) {
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
    append(sb, service.serviceName)
    sb.append(" version ")
    append(sb, service.serviceVersion)

    if (service.defaultVersion) {
      sb.append(" *")
    }

  }

  private def append(sb: StringBuilder, obj: AnyRef) {

    if (obj == null) {
      sb.append("NULL")
    } else {
      sb.append('[').append(obj).append(']')
    }

  }

}
