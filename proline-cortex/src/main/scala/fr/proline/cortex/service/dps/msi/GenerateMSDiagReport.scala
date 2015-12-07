package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.cortex.util.DbConnectionHelper
import fr.profi.util.primitives._
import fr.proline.module.quality.msdiag._
import fr.proline.module.quality.msdiag.service.MSDiagReportGenerator
import fr.proline.jms.service.api.AbstractRemoteProcessService

/**
 *  Define JMS Service which generates MSDiag report
 *
 *  Input params :
 *    project_id : The id of the project
 *    result_set_id : The id of the Result Set containing the peptide matches and spectrum.
 *    msdiag_settings: settings for msDiag report
 *
 *  Output params :
 *    JSON with report's results
 */

class GenerateMSDiagReport extends AbstractRemoteProcessService with LazyLogging {

	/* JMS Service identification */
	val serviceName = "proline/dps/msi/GenerateMSDiagReport";
	val serviceVersion = "1.0";
	override val defaultVersion = true;

	override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
			logger.debug("GenerateMSDiagReport JMS WS: running doProcess");
			require((paramsRetriever != null), "no parameter specified");

			val projectId = paramsRetriever.getLong("project_id");
			val resultSetId = paramsRetriever.getLong("result_set_id");
			val msdiagSettingsAsOptStr = Option(paramsRetriever.getOptMap("msdiag_settings", true, null)).map(serialize(_));
			val msdiagSettings = msdiagSettingsAsOptStr.map(deserialize[Map[String, Any]](_))
    
			val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, projectId, true); // Use JPA context
			
			logger.debug("GenerateMSDiagReport WS: going to launch MSDiag generator");

			val msDiagReportGenerator = new MSDiagReportGenerator(
					execCtx,
					resultSetId,
					msdiagSettings
					);
			logger.debug("GenerateMSDiagReport WS: report generated !");
			
			/// ************************************
			var result : java.lang.Boolean = true;
			try {
				result = msDiagReportGenerator.runService
			} catch {
				case ex: Exception => {
					result = false;
					logger.error("Error running MSDiag data Generator", ex);
					val msg = if (ex.getCause() != null) "Error running MSDiag report Generator " + ex.getCause().getMessage()
							else "Error running MS Diag report Generator " + ex.getMessage();
					throw new Exception(msg)
				}
			} finally {
				try {
					execCtx.closeAll();
				} catch {
					case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
				}
			}

			msDiagReportGenerator.resultHashMapJson;
	}

}