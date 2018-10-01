package fr.proline.cortex.service.dps.msq

import scala.collection.JavaConversions.asScalaBuffer
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.primitives.toLong
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.algo.msq.config.SpectralCountConfig
import fr.proline.core.om.model.msq.LabelFreeQuantMethod
import fr.proline.core.om.model.msq.SimplifiedExperimentalDesign
import fr.proline.core.orm.uds.Dataset
import fr.proline.core.service.msq.quantify.BuildMasterQuantChannelQuantifier
import fr.proline.core.service.uds.CreateSCQuantitation
import fr.proline.cortex.api.service.dps.msq.IQuantifySCService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.core.service.uds.CreateQuantitation
import fr.proline.core.orm.uds.QuantitationMethod
import fr.proline.core.orm.uds.repository.QuantitationMethodRepository
import fr.proline.cortex.api.service.dps.msq.IQuantifySCService_V2
import fr.proline.core.service.uds.Quantifier
import fr.profi.util.exception.ExceptionUtils
import java.util.HashMap
import fr.proline.core.om.model.msq.QuantMethodType
import fr.proline.core.om.model.msq.AbundanceUnit

/**
 *  Define JMS Service which allows to compute spectral count for proteins of result summaries associated to experimental design's QuantChannel.
 *  This service return the id of the created dataset (dataset_quanti_id) and the JSON result containing spectral count values (spectral_count_result).
 *
 *  Input params :
 *    name : Name of the quantitation dataset that will be created for this quantitation.
 *    description: Description of the quantitation dataset that will be created for this quantitation.
 *    project_id: The id of the project the quantitation will be created in.
 *    ref_rsm_id: The id of the reference result summary used for this spectral counting computation.
 *    ref_ds_id: The id of the reference dataset used for this spectral counting computation.
 *    experimental_design: The experimental design related to this quantitation.
 *    peptide_ref_rsm_ids: List of result summary ID where the spectral count specificity and weight should be calculated.
 *
 *  Output params :
 *    Boolean for service run status
 */
@deprecated
class QuantifySC extends AbstractRemoteProcessingService with IQuantifySCService with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.QUANTIFYSC_SINGLETHREAD_IDENT.toString

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val refRSMId = paramsRetriever.getLong(PROCESS_METHOD.REF_RSM_ID_PARAM)
    val refDSId = paramsRetriever.getLong(PROCESS_METHOD.REF_DS_ID_PARAM)
    val pepRedRSMIds: Array[Long] = if (paramsRetriever.hasParam(PROCESS_METHOD.PEPTIDE_REF_RSM_IDS_PARAM)) paramsRetriever.getList(PROCESS_METHOD.PEPTIDE_REF_RSM_IDS_PARAM).toArray.map(toLong(_)) else Array.empty[Long]

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context
    val udsDbCtx = execCtx.getUDSDbConnectionContext
    val udsEM = udsDbCtx.getEntityManager

    val simplifiedExpDesign = deserialize[SimplifiedExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))

    logger.debug(" expr Design => " + simplifiedExpDesign)
    logger.debug(" expr Design masterQuantChannels => " + simplifiedExpDesign.masterQuantChannels)
    if (simplifiedExpDesign.masterQuantChannels.length != 1) {
      throw new Exception("Spectral Count could be run on only one MasterQuantChannel")
    }

    // Register quantitation in the UDSdb
    val expDesign = simplifiedExpDesign.toExperimentalDesign()   
    val qm =  QuantitationMethodRepository.findQuantMethodForTypeAndAbundanceUnit(udsEM, QuantitationMethod.Type.LABEL_FREE.toString, "spectral_counts")
    
    val quantiCreator = new CreateSCQuantitation(
      executionContext = execCtx,
      name = paramsRetriever.getString("name"),
      description = paramsRetriever.getString("description"),
      projectId = projectId,
      experimentalDesign = expDesign
    )
    quantiCreator.runService()
    val udsQuantitationId = quantiCreator.getUdsQuantitation.getId
    
    val udsQuantitation = udsEM.find(classOf[Dataset], udsQuantitationId)

    // Retrieve master quant channel (Should only be one )
    val udsMasterQuantChannel = udsQuantitation.getMasterQuantitationChannels.get(0)

    val scCfg = new SpectralCountConfig(
      identResultSummaryId = Some(refRSMId),
      identDatasetId = Some(refDSId),
      weightsRefRsmIds = pepRedRSMIds
    )

    val mqcQuantifier = BuildMasterQuantChannelQuantifier(
      execCtx,
      udsMasterQuantChannel,
      expDesign,
      LabelFreeQuantMethod,
      scCfg
    )
    mqcQuantifier.quantify()

    val resultMapBuilder = Map.newBuilder[String, Any]

    resultMapBuilder += ("quant_dataset_id" -> udsQuantitationId)
//    resultMapBuilder += ("spectral_count_result" -> mqcQuantifier.getResultAsJSON())
    resultMapBuilder += ("spectral_count_result" -> "No More used ! ")

    DbConnectionHelper.tryToCloseExecContext(execCtx)

    resultMapBuilder.result
  }
}


/**
 *  Define JMS Service which allows to compute spectral count for proteins of result summaries associated to experimental design's QuantChannel.
 *  This service return the id of the created dataset (dataset_quanti_id)
 *
 *  Input params :
 *    name : Name of the quantitation dataset that will be created for this quantitation.
 *    description: Description of the quantitation dataset that will be created for this quantitation.
 *    project_id: The id of the project the quantitation will be created in.
 *    experimental_design: The experimental design related to this quantitation (with The id of the reference result summary & dataset in MasterQuantChannel).
 *    peptide_ref_rsm_ids: List of result summary ID where the spectral count specificity and weight should be calculated.
 *
 *  Output params :
 *    Boolean for service run status
 */
class QuantifySC_V02 extends AbstractRemoteProcessingService with IQuantifySCService_V2 with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.QUANTIFYSC_SINGLETHREAD_IDENT.toString

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val pepRedRSMIds: Array[Long] = if (paramsRetriever.hasParam(PROCESS_METHOD.PEPTIDE_REF_RSM_IDS_PARAM)) paramsRetriever.getList(PROCESS_METHOD.PEPTIDE_REF_RSM_IDS_PARAM).toArray.map(toLong(_)) else Array.empty[Long]

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context
    val udsDbCtx = execCtx.getUDSDbConnectionContext
    val udsEM = udsDbCtx.getEntityManager

    val simplifiedExpDesign = deserialize[SimplifiedExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))

    logger.debug(" expr Design => " + simplifiedExpDesign)
    logger.debug(" expr Design masterQuantChannels => " + simplifiedExpDesign.masterQuantChannels)
    if (simplifiedExpDesign.masterQuantChannels.length != 1) {
      throw new Exception("Spectral Count could be run on only one MasterQuantChannel")
    }

    // Register quantitation in the UDSdb
    val expDesign = simplifiedExpDesign.toExperimentalDesign()   
    val qm =  QuantitationMethodRepository.findQuantMethodForTypeAndAbundanceUnit(udsEM, QuantMethodType.LABEL_FREE.toString, AbundanceUnit.SPECTRAL_COUNTS.toString)
    val spectralCountConfig : java.util.Map[String,Object]  = new HashMap[String,Object]()
    spectralCountConfig.put("weights_ref_rsm_ids",pepRedRSMIds)

    var quantiId = -1L
    
    try {
      val quantifier = new Quantifier(
        executionContext = execCtx,
        name = paramsRetriever.getString(PROCESS_METHOD.NAME_PARAM),
        description = paramsRetriever.getString(PROCESS_METHOD.DESCRIPTION_PARAM),
        projectId = projectId,
        methodId = qm.getId,
        experimentalDesign = expDesign,
        quantConfigAsMap = spectralCountConfig
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