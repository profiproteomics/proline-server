package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.algo.msq.config.profilizer.ProfilizerConfig
import fr.proline.core.service.msq.QuantProfilesComputer
import fr.proline.cortex.api.service.dps.msq.IComputeQuantProfilesService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  Define JMS Service which allows to computes quantitative profiles of peptides and protein sets
 *
 *  Input params :
 *    project_id : The id of project this quantitative dataset refers to.
 *    master_quant_channel_id: The id of master quant channel correspoding to this quantitative dataset.
 *    config: Configuration used for the computation of quantitative profiles.
 *
 *  Output params :
 *    Boolean for service run status
 */
@deprecated
class ComputeQuantProfiles extends AbstractRemoteProcessingService with IComputeQuantProfilesService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val mqcId = paramsRetriever.getLong(PROCESS_METHOD.MASTER_QUANT_CHANNEL_ID_PARAM)
    val quantConfAsStr = serialize(paramsRetriever.getMap(PROCESS_METHOD.CONFIG_PARAM))

    val quantProfilesConfig = deserialize[ProfilizerConfig](quantConfAsStr)
    logger.debug("ComputeQuantProfiles with following config: " + serialize(quantProfilesConfig))

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    try {
      val quantProfilesComputer = QuantProfilesComputer(execCtx, mqcId, quantProfilesConfig)

      this.logger.info("Starting ComputeQuantProfiles WS for Master Quant Channel with id=" + mqcId)
      quantProfilesComputer.run()
    } catch {
      case t: Throwable => {
        throw ExceptionUtils.wrapThrowable("Error while running quantitation post-processing", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}