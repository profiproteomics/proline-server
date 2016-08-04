package fr.proline.cortex.service.monitoring

import fr.proline.jms.service.api.ISingleThreadedService

class SingleThreadedInfoService extends InfoService with ISingleThreadedService {
  
  override val serviceNamespace = "proline/monitoring"
  
  override val serviceLabel = "SingleThreadedInfo"
 
  val singleThreadIdent = "InfoThread"

  override val serviceVersion = "1.0"

  override val isDefaultVersion = true

}