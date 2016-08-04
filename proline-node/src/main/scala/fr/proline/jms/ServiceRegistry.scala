package fr.proline.jms

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import fr.profi.util.StringUtils
import fr.proline.jms.service.api.IRemoteServiceIdentity
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.jms.service.misc.ResourceService

/**
 * Singleton Registry providing all {{{IRemoteServiceIdentity}}} instances.
 * Note :
 * - Services must be stateless, multiple threads can share the same service instance (like a Servlet container).
 * - Accesses to the Registry collection itself are thread safe (synchronized by the collection implicit lock).
 */
object ServiceRegistry {

  val resourceService = new ResourceService() // Unique instance of ResourceService

  // MultiMap @GuardedBy("m_servicesByName_lock")
  private val m_servicesByName = mutable.Map.empty[String, ArrayBuffer[IRemoteServiceIdentity]]
  private val m_servicesByName_lock = new Object()

  def addService(service: IRemoteServiceIdentity) {
    require(service != null, "service is null")
    require(StringUtils.isNotEmpty(service.serviceName), "Invalid Service name")
    require(StringUtils.isNotEmpty(service.serviceVersion), "Invalid Service version")

    m_servicesByName_lock.synchronized {
      val services = m_servicesByName.getOrElseUpdate(service.serviceName, ArrayBuffer() )

      /* Check duplicate same version or multiple default version */
      for (existingService <- services) {

        require(
          service.serviceVersion != existingService.serviceVersion,
          s"Service '${service.serviceName}' already present with version '${service.serviceVersion}'"
        )

        require(
          (service.isDefaultVersion && existingService.isDefaultVersion) == false,
          s"Service '${service.serviceName}' already present with a default version"
        )

      }

      services += service
      
    } // End of synchronized block on m_servicesByName_lock

  }

  /**
   * If {{{serviceVersion}}} is null or empty, return default Service version for given {{{serviceName}}}.
   */
  def getService(serviceName: String, serviceVersion: String): Option[IRemoteServiceIdentity] = {
    require(StringUtils.isNotEmpty(serviceName), "Invalid Service name")

    m_servicesByName_lock.synchronized {
      val servicesOpt = m_servicesByName.get(serviceName)

      if (servicesOpt.isDefined) {
        val services = servicesOpt.get

        if (StringUtils.isEmpty(serviceVersion)) {
          services.find(_.isDefaultVersion)
        } else {
          services.find(svc => svc.serviceVersion == serviceVersion)
        }

      } else {
        None
      }

    } // End of synchronized block on m_servicesByName

  }

  def getParallelizableServices(): Seq[IRemoteServiceIdentity] = {
    val buff = ArrayBuffer.empty[IRemoteServiceIdentity]

    m_servicesByName_lock.synchronized {

      for ( (serviceName,services) <- m_servicesByName) {

        if (services != null && services.nonEmpty) {
          for (service <- services) {

            if (!service.isInstanceOf[ISingleThreadedService]) {
              buff += service
            }

          } // End loop for each service

        } // End if (servicesList is not empty)

      } // End loop for each entry

    } // End of synchronized block on m_servicesByName

    /* Return an immutable List */
    buff
  }
  
  protected def getSingleThreadedServiceByKey(singleThreadedServiceToKey: ISingleThreadedService => String): Map[String, Seq[ISingleThreadedService]] = {
    val singleThreadedServicesByName = mutable.Map.empty[String, ArrayBuffer[ISingleThreadedService]]

    m_servicesByName_lock.synchronized {

      for ( (serviceName,services) <- m_servicesByName) {

        if ((services != null) && !services.isEmpty) {
          for (service <- services) {

            if (service.isInstanceOf[ISingleThreadedService]) {
                val singleThreadedService = service.asInstanceOf[ISingleThreadedService]
                // Append this service to the the singleThreadedServicesByName map
                // Note: version consistency has already been checked in the addService() method
                val key = singleThreadedServiceToKey(singleThreadedService)
                singleThreadedServicesByName.getOrElseUpdate(key, ArrayBuffer()) += singleThreadedService
             
            }

          } // End loop for each service

        } // End if (servicesList is not empty)

      } // End loop for each entry

    } // End of synchronized block on m_servicesByName

    /* Return an immutable Map of (String, immutable Seq) */
    singleThreadedServicesByName.toMap
  }

  def getSingleThreadedServicesByName(): Map[String, Seq[ISingleThreadedService]] = {
    getSingleThreadedServiceByKey( singleThreadedService => singleThreadedService.serviceName )
  }
  
  def getSingleThreadedServicesByThreadIdent(): Map[String, Seq[ISingleThreadedService]] = {
    getSingleThreadedServiceByKey( singleThreadedService => singleThreadedService.singleThreadIdent )
  }

}
