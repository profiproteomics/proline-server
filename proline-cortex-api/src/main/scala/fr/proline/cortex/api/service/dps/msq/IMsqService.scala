package fr.proline.cortex.api.service.dps.msq

import fr.proline.jms.service.api.IRemoteProcessingService

trait IMsqService extends IRemoteProcessingService {
  val serviceNamespace = "proline/dps/msq"
}