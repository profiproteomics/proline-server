package fr.proline.cortex.api.service.dps.uds

import fr.proline.jms.service.api.IRemoteServiceIdentity

trait IUdsService extends IRemoteServiceIdentity {
  val serviceNamespace = "proline/dps/uds"
}