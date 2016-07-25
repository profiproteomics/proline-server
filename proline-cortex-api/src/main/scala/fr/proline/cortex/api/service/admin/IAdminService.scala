package fr.proline.cortex.api.service.admin

import fr.proline.jms.service.api.IRemoteServiceIdentity

trait IAdminService extends IRemoteServiceIdentity {
  val serviceNamespace = "proline/admin"
}