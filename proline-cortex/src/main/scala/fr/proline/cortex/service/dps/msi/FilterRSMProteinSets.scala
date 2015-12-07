package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.cortex.util.DbConnectionHelper
import fr.profi.util.primitives._
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.service.msi.RSMProteinSetFilterer
import fr.proline.core.algo.msi.filtering.IProteinSetFilter
import fr.proline.core.algo.msi.validation.BuildProteinSetFilter
import fr.proline.jms.service.api.AbstractRemoteProcessService

/**
 *  Define JMS Service which Filters ProteinSets of a given Result Summary
 *
 *  Input params :
 *    project_id : The id of the project the result summary belongs to.
 *    result_summary_id: The id of the result summary to filter.
 *    prot_set_filters: List of Proteins set filters to apply (name, threshold).
 *
 *  Output params :
 *    Boolean for service run status
 */

class FilterRSMProteinSets extends AbstractRemoteProcessService with LazyLogging {

	/* JMS Service identification */
	val serviceName = "proline/dps/msi/FilterRSMProteinSets";
	val serviceVersion = "1.0";
	override val defaultVersion = true;

	override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

			require((paramsRetriever != null), "no parameter specified");

			val projectId = paramsRetriever.getLong("project_id");
			val resultSummaryId = paramsRetriever.getLong("result_summary_id");
			
			val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, projectId, true); // Use JPA context
			var msiDbConnectionContext: DatabaseConnectionContext = null;
			var msiDbTransacOk: Boolean = false;
    
			var result : java.lang.Boolean = true;
			try {
				val filterConfigs = this.parseProtSetFilters(paramsRetriever);
	  
				if(filterConfigs.isDefined){
    
					// Begin transaction
					msiDbConnectionContext = execCtx.getMSIDbConnectionContext;
					msiDbConnectionContext.beginTransaction();
					msiDbTransacOk = false;

					// Instantiate a result set validator
					val rsmFilterer  = RSMProteinSetFilterer(
							execCtx = execCtx,
							targetRsmId = resultSummaryId,
							protSetFilters = filterConfigs.get
							);
	
					rsmFilterer.run;
	      
					//Commit transaction
					msiDbConnectionContext.commitTransaction();
					msiDbTransacOk = true;
				}
			} catch {
				case ex: Exception => {
					result = false;
					logger.error("Error running Filter RSM Protein Sets", ex);
					val msg = if (ex.getCause() != null) { "Error running Filter RSM Protein Sets " + ex.getCause().getMessage() } else { "Error running Filter RSM Protein Sets " + ex.getMessage() };
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
			
	def parseProtSetFilters(params: NamedParamsRetriever): Option[Seq[IProteinSetFilter]] = {
			if (params.hasParam("prot_set_filters")) {
				val protSetFiltersConfigs = params.getList("prot_set_filters").toArray.map(parseFilterConfig(_))
      
				Some(protSetFiltersConfigs.map(fc => { 
					logger.debug(" ---- TRY to BuildProteinSetFilter for "+fc.parameter+" threshold "+ fc.threshold.asInstanceOf[AnyVal] )
					val proSetFilter= BuildProteinSetFilter(fc.parameter, fc.threshold.asInstanceOf[AnyVal])
					proSetFilter
				}).toSeq)
      
			} else None
  }
	
	def parseFilterConfig(paramsMap: Object): FilterConfig = {
			val configAsMap = deserialize[Map[String, AnyRef]](serialize(paramsMap))
			if(configAsMap.contains("post_validation"))
				new FilterConfig(configAsMap("parameter").asInstanceOf[String], configAsMap("threshold").asInstanceOf[AnyVal],configAsMap("post_validation").asInstanceOf[Boolean] )
			else
				new FilterConfig(configAsMap("parameter").asInstanceOf[String], configAsMap("threshold").asInstanceOf[AnyVal])
  }

}