package fr.proline.cortex.service.dps.msq

import scala.collection.JavaConversions.asScalaBuffer
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.primitives.toLong
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.orm.uds.Dataset
import fr.proline.core.service.uds.CreateSCQuantitation
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.core.om.model.msq.LabelFreeQuantMethod
import fr.proline.core.om.model.msq.SimplifiedExperimentalDesign
import fr.proline.core.service.msq.quantify.BuildMasterQuantChannelQuantifier
import fr.proline.core.algo.msq.config.SpectralCountConfig

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
class QuantifySC extends AbstractRemoteProcessService with LazyLogging with ISingleThreadedService{
	/* JMS Service identification */
	val serviceName = "proline/dps/msq/QuantifySC";
	val serviceVersion = "1.0";   // !!corresponds to V2 in webCore
	val singleThreadIdent = "quantifySCThread"
	override val defaultVersion = true; 
	
	override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

			require((paramsRetriever != null), "no parameter specified");
			
			val projectId = paramsRetriever.getLong("project_id");
			val refRSMId = paramsRetriever.getLong("ref_rsm_id");
			val refDSId = paramsRetriever.getLong("ref_ds_id")
			val pepRedRSMIds : Seq[Long] = if(paramsRetriever.hasParam("peptide_ref_rsm_ids") ) paramsRetriever.getList("peptide_ref_rsm_ids").toSeq.map(toLong(_)) else Seq.empty[Long]
    
			val execCtx =  DbConnectionHelper.createJPAExecutionContext(projectId) ; // Use JPA context
			val udsDbCtx = execCtx.getUDSDbConnectionContext();
			val udsEM = udsDbCtx.getEntityManager();
			
			val simplifiedExpDesign = deserialize[SimplifiedExperimentalDesign](serialize(paramsRetriever.getMap("experimental_design")));

			logger.debug(" expr Design => " + simplifiedExpDesign);
			logger.debug(" expr Design masterQuantChannels => " + simplifiedExpDesign.masterQuantChannels);
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
			quantiCreator.runService();
			
			val udsQuantitation = udsEM.find(classOf[Dataset], quantiCreator.getUdsQuantitation.getId);

			// Retrieve master quant channel (Should only be one )
			val udsMasterQuantChannel = udsQuantitation.getMasterQuantitationChannels.get(0);
    
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
			mqcQuantifier.runService();*/
      val mqcQuantifier = BuildMasterQuantChannelQuantifier(
        execCtx,
        udsMasterQuantChannel,
        expDesign,
        LabelFreeQuantMethod,
        scCfg
      )
      mqcQuantifier.quantify()

			val resultMapBuilder = Map.newBuilder[String, Any];

			val quantDsId = quantiCreator.getUdsQuantitation.getId;
			resultMapBuilder += ("quant_dataset_id" -> quantDsId); 
			resultMapBuilder += ("spectral_count_result" -> mqcQuantifier.getResultAsJSON());
    
			try {
					execCtx.closeAll();
			} catch {
					case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
			}

    		resultMapBuilder.result
	}
}