package fr.proline.cortex.service.dps.msq


import com.typesafe.scalalogging.LazyLogging
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.proline.core.algo.msq.ProfilizerConfig
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.cortex.util.DbConnectionHelper
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.provider.lcms.impl.SQLScanSequenceProvider
import fr.proline.core.om.provider.lcms.impl.SQLRunProvider
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.lcms.IRunProvider
import fr.proline.core.service.uds.Quantifier
import fr.proline.cortex.util.MountPointPathConverter
import fr.proline.jms.service.api.AbstractRemoteProcessService
import fr.proline.jms.service.api.ISingleThreadedService


/**
 *  Define JMS Service which allows to creates a new quantitation and perform the corresponding data analysis.
 *
 *  Input params :
 *    name : The quantitation name.
 *    description: TThe quantitation description.
 *    project_id: The id of the project the quantitation will be created in
 *    method_id: The id of the quantitative method to be used.
 *    experimental_design: The experimental design related to this quantitation.
 *    quantitation_config: The parameters to use in order to perform a specific quantitative method (see quantitative methods documentation).
 *
 *  Output params :
 *    Boolean for service run status
 */
class Quantify  extends AbstractRemoteProcessService with LazyLogging with ISingleThreadedService{
	/* JMS Service identification */
	val serviceName = "proline/dps/msq/Quantify";
	val serviceVersion = "1.0"; 
	override val defaultVersion = true; 
	
	override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

			require((paramsRetriever != null), "no parameter specified");

			val projectId = paramsRetriever.getLong("project_id");
			val expDesign = deserialize[ExperimentalDesign](serialize(paramsRetriever.getMap("experimental_design")));
			val quantConfigAsMap = paramsRetriever.getMap("quantitation_config")
    
			
			val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, projectId, true); // Use JPA context
			
			// Register SQLRunProvider 
			val scanSeqProvider = new SQLScanSequenceProvider(execCtx.getLCMSDbConnectionContext());
			val lcMsRunProvider = new SQLRunProvider(
					execCtx.getUDSDbConnectionContext(),
					Some(scanSeqProvider),
					Some(MountPointPathConverter)
					);
			val providerContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory
			providerContext.putProvider(classOf[IRunProvider],lcMsRunProvider )
    
			var result : java.lang.Boolean = true;
			var quantiId: java.lang.Long = -1;
			try {
			  val quantifier = new Quantifier(
					  executionContext = providerContext,
					  name = paramsRetriever.getString("name"),
					  description = paramsRetriever.getString("description"),
					  projectId = projectId,
					  methodId = paramsRetriever.getLong("method_id"),
					  experimentalDesign = expDesign,
					  quantConfigAsMap = quantConfigAsMap
					  );
			  quantifier.run();
      
			  quantiId = quantifier.getQuantitationId;
				
			} catch {
				case ex: Exception => {
					result = false;
					logger.error("Error running Quantify", ex);
					val msg = if (ex.getCause() != null) { "Error running Quantify " + ex.getCause().getMessage() } else { "Error running Quantify " + ex.getMessage() };
					throw new Exception(msg)
				}
			} finally {
				try {
					execCtx.closeAll();
				} catch {
					case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
				}
			}

			return quantiId;
	}
}