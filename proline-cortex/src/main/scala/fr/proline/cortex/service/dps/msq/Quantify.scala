package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.algo.lcms._
import fr.proline.core.algo.msq.config.{ExtractionParams, LabelFreeQuantConfig, LabelFreeQuantConfigConverter}
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.lcms.IRunProvider
import fr.proline.core.om.provider.lcms.impl.SQLRunProvider
import fr.proline.core.om.provider.lcms.impl.SQLScanSequenceProvider
import fr.proline.core.service.uds.Quantifier
import fr.proline.cortex.api.service.dps.msq.IQuantifyService
import fr.proline.cortex.api.service.dps.msq.IQuantifyServiceV3
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointPathConverter
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.jms.util.NodeConfig
import scala.collection.JavaConverters._

/**
 *  Define JMS Service which allows to creates a new quantitation and perform the corresponding data analysis.
 *
 *  Input params :
 *    name : Name of the quantitation dataset that will be created for this quantitation.
 *    description: Description of the quantitation dataset that will be created for this quantitation.
 *    project_id: The id of the project the quantitation will be created in
 *    method_id: The id of the quantitative method to be used.
 *    experimental_design: The experimental design related to this quantitation.
 *    quantitation_config: The parameters to use in order to perform a specific quantitative method (see quantitative methods documentation).
 *
 *  Output params :
 *    Boolean for service run status
 */
class Quantify extends AbstractRemoteProcessingService with IQuantifyService with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.QUANTIFY_SINGLETHREAD_IDENT.toString

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val expDesign = deserialize[ExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))
    val quantConfigAsMap = paramsRetriever.getMap(PROCESS_METHOD.QUANTITATION_CONFIG_PARAM)

    logger.info("LabelFreeQuantConfig version 1.0 => "+ProfiJson.serialize(quantConfigAsMap))
    val newQuantConfigAsMap = ProfiJson.deserialize[Map[String,Any]](ProfiJson.serialize(quantConfigAsMap))
//    val quantConfigV2AsMap = LabelFreeQuantConfigConverter.convertFromV1(newQuantConfigAsMap)
    val quantConfigV2Str = ProfiJson.serialize(LabelFreeQuantConfigConverter.convertFromV1(newQuantConfigAsMap))
    logger.info("LabelFreeQuantConfig version 2.0 => "+quantConfigV2Str)
    val quantConfigV2AsMap = ProfiJson.deserialize[Map[String,Object]](quantConfigV2Str)


    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    // Register SQLRunProvider 
    val scanSeqProvider = new SQLScanSequenceProvider(execCtx.getLCMSDbConnectionContext)
    val lcMsRunProvider = new SQLRunProvider(
      execCtx.getUDSDbConnectionContext,
      Some(scanSeqProvider),
      Some(MountPointPathConverter)
    )
    val providerContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory
    providerContext.putProvider(classOf[IRunProvider], lcMsRunProvider)
    
    // TODO: remove me, temporary workarounds used until configuration files have been revised (see #15945,#15948)
    fr.proline.core.service.lcms.io.PeakelsDetector.setMzdbMaxParallelism(
      NodeConfig.MZDB_MAX_PARALLELISM
    )
    fr.proline.core.service.lcms.io.PeakelsDetector.setTempDirectory(
      new java.io.File(NodeConfig.PEAKELDB_TEMP_DIRECTORY)
    )
    
    var quantiId = -1L
    try {
      val quantifier = new Quantifier(
        executionContext = providerContext,
        name = paramsRetriever.getString(PROCESS_METHOD.NAME_PARAM),
        description = paramsRetriever.getString(PROCESS_METHOD.DESCRIPTION_PARAM),
        projectId = projectId,
        methodId = paramsRetriever.getLong(PROCESS_METHOD.METHOD_ID_PARAM),
        experimentalDesign = expDesign,
        quantConfigAsMap = quantConfigV2AsMap.asJava
      )
      quantifier.run()

      quantiId = quantifier.getQuantitationId()

    } catch {
      case t: Throwable => {
        throw ExceptionUtils.wrapThrowable("Error while quantifying the dataset", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
      
      // Run the garbage collector
      System.gc()
    }

    quantiId
  }
}

class QuantifyV3_0 extends AbstractRemoteProcessingService with  IQuantifyServiceV3 with LazyLogging with ISingleThreadedService {

  val singleThreadIdent: String = SingleThreadIdentifierType.QUANTIFY_SINGLETHREAD_IDENT.toString

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val expDesign = deserialize[ExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))
    val quantConfigAsMap = paramsRetriever.getMap(PROCESS_METHOD.QUANTITATION_CONFIG_PARAM)

    logger.info("LabelFreeQuantConfig version 2.0 => "+ProfiJson.serialize(quantConfigAsMap))

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    // Register SQLRunProvider
    val scanSeqProvider = new SQLScanSequenceProvider(execCtx.getLCMSDbConnectionContext)
    val lcMsRunProvider = new SQLRunProvider(
      execCtx.getUDSDbConnectionContext,
      Some(scanSeqProvider),
      Some(MountPointPathConverter)
    )
    val providerContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory
    providerContext.putProvider(classOf[IRunProvider], lcMsRunProvider)

    // TODO: remove me, temporary workarounds used until configuration files have been revised (see #15945,#15948)
    fr.proline.core.service.lcms.io.PeakelsDetector.setMzdbMaxParallelism(
      NodeConfig.MZDB_MAX_PARALLELISM
    )
    fr.proline.core.service.lcms.io.PeakelsDetector.setTempDirectory(
      new java.io.File(NodeConfig.PEAKELDB_TEMP_DIRECTORY)
    )

    var quantiId = -1L
    try {
      val quantifier = new Quantifier(
        executionContext = providerContext,
        name = paramsRetriever.getString(PROCESS_METHOD.NAME_PARAM),
        description = paramsRetriever.getString(PROCESS_METHOD.DESCRIPTION_PARAM),
        projectId = projectId,
        methodId = paramsRetriever.getLong(PROCESS_METHOD.METHOD_ID_PARAM),
        experimentalDesign = expDesign,
        quantConfigAsMap = quantConfigAsMap
      )
      quantifier.run()

      quantiId = quantifier.getQuantitationId()

    } catch {
      case t: Throwable => {
        throw ExceptionUtils.wrapThrowable("Error while quantifying the dataset", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    quantiId
  }
}