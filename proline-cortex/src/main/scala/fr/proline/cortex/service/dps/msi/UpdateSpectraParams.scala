package fr.proline.cortex.service.dps.msi

import fr.proline.cortex.service.IRemoteService
import com.typesafe.scalalogging.slf4j.Logging
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.proline.cortex.service.AbstractRemoteProcessService
import fr.profi.util.serialization.ProfiJson._
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.service.msi.SpectraParamsUpdater
import fr.proline.cortex.util.DbConnectionHelper

/**
 *  Define JMS Service wich allows to update scan, cycle and time information of spectra belonging to specified peaklists.
 *  Information are extracted from the spectrum title string by applying the regexes corresponding to the given parsing rule.
 *
 * Input params :
 *  project_id : The id of the project the peaklists will be searched in
 *  peaklist_ids : The ids of the peaklists to update.
 *  spec_title_rule_id : The id of the spectrum title parsing rule to use.
 *
 * Output params :
 *  new project Id.
 *
 */
class UpdateSpectraParams extends AbstractRemoteProcessService with Logging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/UpdateSpectraParams"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require((paramsRetriever != null), "No Parameters specified")

    val projectId = paramsRetriever.getLong("project_id")
    val peaklistIds = paramsRetriever.getList("peaklist_ids").toArray.map { rf => deserialize[Long](serialize(rf)) }
    val specTitleRuleId = paramsRetriever.getLong("spec_title_rule_id")

    val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), projectId, false)

    var updatedSpectraCount: Integer = 0
    try {
      logger.info("UpdateSpectraParams WebService is starting...")

      updatedSpectraCount = SpectraParamsUpdater.updateSpectraParams(execCtx, projectId, peaklistIds, specTitleRuleId)

      logger.info(updatedSpectraCount + " spectra updated !")
    } finally {

      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    updatedSpectraCount
  }
}