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
class QuantifySC extends AbstractRemoteProcessingService with IQuantifySCService with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  // TODO: create an enumeration of singleThreadIdentifiers
  val singleThreadIdent = "quantifySCThread"

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require((paramsRetriever != null), "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val refRSMId = paramsRetriever.getLong(PROCESS_METHOD.REF_RSM_ID_PARAM)
    val refDSId = paramsRetriever.getLong(PROCESS_METHOD.REF_DS_ID_PARAM)
    val pepRedRSMIds: Seq[Long] = if (paramsRetriever.hasParam(PROCESS_METHOD.PEPTIDE_REF_RSM_IDS_PARAM)) paramsRetriever.getList(PROCESS_METHOD.PEPTIDE_REF_RSM_IDS_PARAM).toSeq.map(toLong(_)) else Seq.empty[Long]

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context
    val udsDbCtx = execCtx.getUDSDbConnectionContext()
    val udsEM = udsDbCtx.getEntityManager()

    val simplifiedExpDesign = deserialize[SimplifiedExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))

    logger.debug(" expr Design => " + simplifiedExpDesign)
    logger.debug(" expr Design masterQuantChannels => " + simplifiedExpDesign.masterQuantChannels)
    if (simplifiedExpDesign.masterQuantChannels.length != 1) {
      throw new Exception("Spectral Count could be run on only one MasterQuantChannel")
    }

    // Register quantitation in the UDSdb
    val expDesign = simplifiedExpDesign.toExperimentalDesign()
    val quantiCreator = new CreateSCQuantitation(
      executionContext = execCtx,
      name = paramsRetriever.getString("name"),
      description = paramsRetriever.getString("description"),
      projectId = projectId,
      experimentalDesign = expDesign
    )
    quantiCreator.runService()

    val udsQuantitation = udsEM.find(classOf[Dataset], quantiCreator.getUdsQuantitation.getId)

    // Retrieve master quant channel (Should only be one )
    val udsMasterQuantChannel = udsQuantitation.getMasterQuantitationChannels.get(0)

    val scCfg = new SpectralCountConfig(
      parentRSMId = Some(refRSMId),
      parentDSId = Some(refDSId),
      weightRefRSMIds = pepRedRSMIds
    )

    /*val mqcQuantifier = new QuantifyMasterQuantChannel(
      executionContext = execCtx,
      experimentalDesign = null,
      masterQuantChannelId = udsMasterQuantChannel.getId,
      quantConfig = scCfg
    )
    mqcQuantifier.runService()*/
    val mqcQuantifier = BuildMasterQuantChannelQuantifier(
      execCtx,
      udsMasterQuantChannel,
      expDesign,
      LabelFreeQuantMethod,
      scCfg
    )
    mqcQuantifier.quantify()

    val resultMapBuilder = Map.newBuilder[String, Any]

    val quantDsId = quantiCreator.getUdsQuantitation.getId
    resultMapBuilder += ("quant_dataset_id" -> quantDsId)
    resultMapBuilder += ("spectral_count_result" -> mqcQuantifier.getResultAsJSON())

    DbConnectionHelper.tryToCloseExecContext(execCtx)

    resultMapBuilder.result
  }
}