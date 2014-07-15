package fr.proline.cortex.service.monitoring

import fr.proline.cortex.service.ISingleThreadedService

class SingleThreadedInfoService extends InfoService with ISingleThreadedService {
  
  override val serviceName = "proline/monitoring/SingleThreadedInfo"

  override val serviceVersion = "1.0"

  override val defaultVersion = true

}
