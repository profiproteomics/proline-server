package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.{deserialize, serialize}
import fr.proline.admin.service.db.SetupProline
import fr.proline.core.algo.msq.config.profilizer.{PostProcessingConfig, PostProcessingConfigV2}
import fr.proline.core.service.msq.QuantPostProcessingComputer
import fr.proline.cortex.api.service.dps.msq.{IComputeQuantPostProcessingService, IComputeQuantPostProcessingServiceV2}
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

import java.io.File

/**
  *  Define JMS Service which allows to quantitation post processing of peptides and protein sets
  *
  *  Input params :
  *    project_id : The id of project this quantitative dataset refers to.
  *    master_quant_channel_id: The id of master quant channel correspoding to this quantitative dataset.
  *    config: Configuration used for the quantitation post processings.
  *
  *  Output params :
  *    Boolean for service run status
  */
class ComputeQuantPostProcessing extends AbstractRemoteProcessingService with IComputeQuantPostProcessingService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val mqcId = paramsRetriever.getLong(PROCESS_METHOD.MASTER_QUANT_CHANNEL_ID_PARAM)
    val quantConfAsStr = serialize(paramsRetriever.getMap(PROCESS_METHOD.CONFIG_PARAM))

    val quantPostProcessingConfig = deserialize[PostProcessingConfigV2](quantConfAsStr)
    logger.debug("Quantification Post Processing with following config: " + serialize(quantPostProcessingConfig))

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    try {
      val quantProfilesComputer = QuantPostProcessingComputer(execCtx, mqcId, quantPostProcessingConfig.toPostProcessingConfig)

      this.logger.info("Starting quantitation Post Processing for Master Quant Channel with id=" + mqcId)
      quantProfilesComputer.run()
    } catch {
      case t: Throwable =>
        throw ExceptionUtils.wrapThrowable("Error while running quantitation post-processing", t, appendCause = true)
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}

class ComputeQuantPostProcessingV2 extends AbstractRemoteProcessingService with IComputeQuantPostProcessingServiceV2 with LazyLogging {



  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val mqcId = paramsRetriever.getLong(PROCESS_METHOD.MASTER_QUANT_CHANNEL_ID_PARAM)
    val quantConfAsStr = serialize(paramsRetriever.getMap(PROCESS_METHOD.CONFIG_PARAM))

    val quantPostProcessingConfig = deserialize[PostProcessingConfig](quantConfAsStr)
    logger.debug("Quantification Post Processing with following config: " + serialize(quantPostProcessingConfig))
    //For tests only ! VDS FIXME get default purity correction matrix.. to remove
    val matrixFolder : Option[File] =  if(quantPostProcessingConfig.usePurityCorrectionMatrix && quantPostProcessingConfig.purityCorrectionMatrix.isEmpty){
      if(!SetupProline.getConfigParams().hasPath("proline-config") || !SetupProline.getConfigParams().getConfig("proline-config").hasPath("data-directory") )
        throw new RuntimeException(" Can not access to Data Directory to get default matrix !")

      val fileName = SetupProline.getConfigParams().getConfig("proline-config").getString("data-directory")
      Some(new File(fileName))
    } else None

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    try {
      val quantPostProcessComputer = QuantPostProcessingComputer(execCtx, mqcId, quantPostProcessingConfig, matrixFolder)

      this.logger.info("Starting quantitation Post Processing for Master Quant Channel with id=" + mqcId)
      quantPostProcessComputer.run()
    } catch {
      case t: Throwable =>
        throw ExceptionUtils.wrapThrowable("Error while running quantitation post-processing", t, appendCause = true)
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}
