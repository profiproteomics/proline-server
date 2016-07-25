package fr.proline.cortex.api.service.dps.msi

import fr.proline.jms.service.api.IRemoteProcessingService

trait IMsiService extends IRemoteProcessingService {
  val serviceNamespace = "proline/dps/msi"
}