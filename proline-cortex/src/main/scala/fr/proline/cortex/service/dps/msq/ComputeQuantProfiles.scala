package fr.proline.cortex.service.dps.msq

import fr.proline.cortex.service.AbstractRemoteProcessService
import com.typesafe.scalalogging.slf4j.Logging
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.profi.util.serialization.ProfiJson._
import fr.proline.core.algo.msq.ProfilizerConfig
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.core.service.msq.QuantProfilesComputer


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
class ComputeQuantProfiles  extends AbstractRemoteProcessService with Logging{
	/* JMS Service identification */
	val serviceName = "proline/dps/msq/ComputeQuantProfiles";
	val serviceVersion = "1.0"; // !!corresponds to V2 in webCore
	override val defaultVersion = true; 
	
	override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

			require((paramsRetriever != null), "no parameter specified");

			val projectId = paramsRetriever.getLong("project_id");
			val mqcId = paramsRetriever.getLong("master_quant_channel_id");
			val quantConfAsStr = serialize(paramsRetriever.getMap("config"));
    
			val quantProfilesConfig = deserialize[ProfilizerConfig](quantConfAsStr);
			logger.debug("ComputeQuantProfiles with following config: " + serialize(quantProfilesConfig))
    
			val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, projectId, true); // Use JPA context
			
			
			var result : java.lang.Boolean = true;
			try {
				val quantProfilesComputer = QuantProfilesComputer(execCtx,mqcId,quantProfilesConfig);
      
				this.logger.info("Starting ComputeQuantProfiles WS with Master Quant Channel with id="+mqcId);
				quantProfilesComputer.run()
			} catch {
				case ex: Exception => {
					result = false;
					logger.error("Error runningComputeQuantProfiles", ex);
					val msg = if (ex.getCause() != null) { "Error running ComputeQuantProfiles " + ex.getCause().getMessage() } else { "Error running ComputeQuantProfiles " + ex.getMessage() };
					throw new Exception(msg)
				}
			} finally {
				try {
					execCtx.closeAll();
				} catch {
					case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
				}
			}

			result;
	}
}