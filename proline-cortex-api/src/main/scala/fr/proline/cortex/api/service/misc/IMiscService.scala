package fr.proline.cortex.api.service.misc

import fr.proline.cortex.api.service.dps.IDataProcessingService
import fr.proline.cortex.api.JSONRPC2Service

trait IMiscService extends JSONRPC2Service {
  val serviceNamespace = "proline/misc"
}