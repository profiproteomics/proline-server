package fr.proline.cortex.api.service.admin

import fr.proline.cortex.api.JSONRPC2ServiceDefinition

trait IAdminService extends JSONRPC2ServiceDefinition {
  val serviceNamespace = "proline/admin"
}