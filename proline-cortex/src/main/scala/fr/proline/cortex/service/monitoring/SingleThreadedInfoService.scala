package fr.proline.cortex.service.monitoring

import fr.proline.jms.service.api.ISingleThreadedService

class SingleThreadedInfoService extends InfoService with ISingleThreadedService {
  
  override val serviceName = "proline/monitoring/SingleThreadedInfo"
 
  val singleThreadIdent = "InfoThread"

  override val serviceVersion = "1.0"

  override val defaultVersion = true

}