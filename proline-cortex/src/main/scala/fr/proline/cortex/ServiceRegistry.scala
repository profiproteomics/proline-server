package fr.proline.cortex

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import fr.profi.util.StringUtils
import fr.proline.cortex.service.IRemoteService
import fr.proline.cortex.service.ISingleThreadedService
import fr.proline.cortex.service.misc.ResourceService

/**
 * Singleton Registry providing all {{{IRemoteService}}} instances.
 * Note :
 * - Services must be stateless, multiple threads can share the same service instance (like a Servlet container).
 * - Accesses to the Registry collection itself are thread safe (synchronized by the collection implicit lock).
 */
object ServiceRegistry {

  val resourceService = new ResourceService() // Unique instance of ResourceService

  // MultiMap
  private val m_servicesPerName = mutable.Map.empty[String, ArrayBuffer[IRemoteService]]

  def addService(service: IRemoteService) {
    require(service != null, "Service is null")
    require(!StringUtils.isEmpty(service.serviceName), "Invalid Service name")
    require(!StringUtils.isEmpty(service.serviceVersion), "Invalid Service version")

    m_servicesPerName.synchronized {
      val optionalList = m_servicesPerName.get(service.serviceName)

      val servicesList = if (optionalList.isDefined) {
        optionalList.get
      } else {
        val newList = ArrayBuffer.empty[IRemoteService]
        m_servicesPerName.put(service.serviceName, newList)
        newList
      }

      /* Check duplicate same version or multiple default version */
      for (currentService <- servicesList) {

        if (service.serviceVersion.equals(currentService.serviceVersion)) {
          throw new IllegalArgumentException("Sevice [" + service.serviceName + "] already present with version [" + service.serviceVersion + ']')
        }

        if (service.defaultVersion && currentService.defaultVersion) {
          throw new IllegalArgumentException("Sevice [" + service.serviceName + "] already present with a default version")
        }

      }

      servicesList += service
    } // End of synchronized block on m_servicesPerName

  }

  /**
   * If {{{serviceVersion}}} is null or empty, return default Service version for given {{{serviceName}}}.
   */
  def getService(serviceName: String, serviceVersion: String): Option[IRemoteService] = {
    require(!StringUtils.isEmpty(serviceName), "Invalid Service name")

    m_servicesPerName.synchronized {
      val optionalList = m_servicesPerName.get(serviceName)

      if (optionalList.isDefined) {
        val servicesList = optionalList.get

        if (StringUtils.isEmpty(serviceVersion)) {
          servicesList.find(svc => svc.defaultVersion)
        } else {
          servicesList.find(svc => serviceVersion.equals(svc.serviceVersion))
        }

      } else {
        None
      }

    } // End of synchronized block on m_servicesPerName

  }

  def getParallelizableServices(): List[IRemoteService] = {
    val buff = ArrayBuffer.empty[IRemoteService]

    m_servicesPerName.synchronized {

      for (entry <- m_servicesPerName) {
        val servicesList = entry._2

        if ((servicesList != null) && !servicesList.isEmpty) {
          for (service <- servicesList) {

            if (!service.isInstanceOf[ISingleThreadedService]) {
              buff += service
            }

          } // End loop for each service

        } // End if (servicesList is not empty)

      } // End loop for each entry

    } // End of synchronized block on m_servicesPerName

    /* Return an immutable List */
    buff.toList
  }

  def getSingleThreadedServices(): Map[String, List[IRemoteService]] = {
    val mutableMap = mutable.Map.empty[String, ArrayBuffer[IRemoteService]]

    m_servicesPerName.synchronized {

      for (entry <- m_servicesPerName) {
        val servicesList = entry._2

        if ((servicesList != null) && !servicesList.isEmpty) {
          for (service <- servicesList) {

            if (service.isInstanceOf[ISingleThreadedService]) {
              val optionalList = mutableMap.get(service.serviceName)

              val servicesList = if (optionalList.isDefined) {
                optionalList.get
              } else {
                val newList = ArrayBuffer.empty[IRemoteService]
                mutableMap.put(service.serviceName, newList)
                newList
              }

              /* Version consistency already checked by addService() */

              servicesList += service
            }

          } // End loop for each service

        } // End if (servicesList is not empty)

      } // End loop for each entry

    } // End of synchronized block on m_servicesPerName

    /* Return an immutable Map of (String, immutable List) */
    (mutableMap.mapValues(svcs => svcs.toList)).toMap
  }

}
