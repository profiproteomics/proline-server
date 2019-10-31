package fr.proline.cortex.api.service.dps.msq

import fr.proline.jms.service.api.IRemoteServiceIdentity

trait IMsqService extends IRemoteServiceIdentity {
  val serviceNamespace = "proline/dps/msq"
}