package fr.proline.cortex.api.service.dps.msi

import fr.proline.jms.service.api.IRemoteServiceIdentity

trait IMsiService extends IRemoteServiceIdentity {
  val serviceNamespace = "proline/dps/msi"
}