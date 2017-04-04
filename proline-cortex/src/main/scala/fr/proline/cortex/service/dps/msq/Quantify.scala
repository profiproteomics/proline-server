package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.lcms.IRunProvider
import fr.proline.core.om.provider.lcms.impl.SQLRunProvider
import fr.proline.core.om.provider.lcms.impl.SQLScanSequenceProvider
import fr.proline.core.service.uds.Quantifier
import fr.proline.cortex.api.service.dps.msq.IQuantifyService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointPathConverter
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.jms.util.NodeConfig
import fr.proline.core.util.CoreConfig
import fr.proline.cortex.service.SingleThreadIdentifierType

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
  val singleThreadIdent = SingleThreadIdentifierType.QUANTIFY_SINGLETHREAD_IDENT.toString()

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val expDesign = deserialize[ExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))
    val quantConfigAsMap = paramsRetriever.getMap(PROCESS_METHOD.QUANTITATION_CONFIG_PARAM)

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    // Register SQLRunProvider 
    val scanSeqProvider = new SQLScanSequenceProvider(execCtx.getLCMSDbConnectionContext())
    val lcMsRunProvider = new SQLRunProvider(
      execCtx.getUDSDbConnectionContext(),
      Some(scanSeqProvider),
      Some(MountPointPathConverter)
    )
    val providerContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory
    providerContext.putProvider(classOf[IRunProvider], lcMsRunProvider)
    
    //  Get nbr XIC files to process in parallel from NodeConfig     
    CoreConfig.mzdbMaxParallelism = NodeConfig.NBR_MZDB_FILES_IN_PARALLEL 
    
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

      quantiId = quantifier.getQuantitationId

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

/**
 *  Define JMS Service which allows to creates a new quantitation and perform the corresponding data analysis.
 *
 *  Input params :
 *    name : Name of the quantitation dataset that will be created for this quantitation.
 *    description: Description of the quantitation dataset that will be created for this quantitation.
 *    project_id: The id of the project the quantitation will be created in
 *    ref_rsm_id: The id of the reference result summary used for this spectral counting computation.
 *    ref_ds_id: The id of the reference dataset used for this spectral counting computation.
 *    method_id: The id of the quantitative method to be used.
 *    experimental_design: The experimental design related to this quantitation.
 *    quantitation_config: The parameters to use in order to perform a specific quantitative method (see quantitative methods documentation).
 *
 *  Output params :
 *    Boolean for service run status
 */
// TODO: remove me because this version is now useless
@deprecated
class QuantifyV2_0 extends AbstractRemoteProcessingService with IQuantifyService with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  override val serviceVersion = "2.0"
  override val isDefaultVersion = false
  
  val singleThreadIdent = SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString()

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong("project_id")
    val expDesign = deserialize[ExperimentalDesign](serialize(paramsRetriever.getMap("experimental_design")))
    // val refRSMIdParam = paramsRetriever.getLong("ref_rsm_id")
    // val refDSIdParam = paramsRetriever.getLong("ref_ds_id")
    val quantConfigAsMap = paramsRetriever.getMap("quantitation_config")

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    // Register SQLRunProvider 
    val scanSeqProvider = new SQLScanSequenceProvider(execCtx.getLCMSDbConnectionContext())
    val lcMsRunProvider = new SQLRunProvider(
      execCtx.getUDSDbConnectionContext(),
      Some(scanSeqProvider),
      Some(MountPointPathConverter)
    )
    val providerContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory
    providerContext.putProvider(classOf[IRunProvider], lcMsRunProvider)

    var quantiId = -1L
    try {
      val quantifier = new Quantifier(
        executionContext = providerContext,
        name = paramsRetriever.getString("name"),
        description = paramsRetriever.getString("description"),
        projectId = projectId,
        methodId = paramsRetriever.getLong("method_id"),
        experimentalDesign = expDesign,
        quantConfigAsMap = quantConfigAsMap
      // note: previous fields refDSIdOp and refRSMIdOpmust be provided using the expDesign
      // (MasterQuantChannel identResultSummaryId and identDatasetId)
      )
      quantifier.run()

      quantiId = quantifier.getQuantitationId

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