package fr.proline.cortex.api.service.admin

import fr.proline.cortex.api.service.dps.IDataProcessingService
import fr.proline.cortex.api.JSONRPC2Service

trait IAdminService extends JSONRPC2Service {
  val serviceNamespace = "proline/admin"
}