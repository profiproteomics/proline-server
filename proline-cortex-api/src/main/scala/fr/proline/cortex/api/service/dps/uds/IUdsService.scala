package fr.proline.cortex.api.service.dps.uds

import fr.proline.jms.service.api.IRemoteProcessingService

trait IUdsService extends IRemoteProcessingService {
  val serviceNamespace = "proline/dps/uds"
}