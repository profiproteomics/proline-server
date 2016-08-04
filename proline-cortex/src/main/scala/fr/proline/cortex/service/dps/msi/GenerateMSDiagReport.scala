package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.cortex.api.service.dps.msi.IGenerateMSDiagReportService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.module.quality.msdiag.service.MSDiagReportGenerator

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

class GenerateMSDiagReport extends AbstractRemoteProcessingService with IGenerateMSDiagReportService with LazyLogging {

	def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
		logger.debug("GenerateMSDiagReport JMS WS: running doProcess")
		require((paramsRetriever != null), "no parameter specified")

		val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
		val resultSetId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SET_ID_PARAM)
		val msdiagSettingsAsOptStr = Option(paramsRetriever.getOptMap(PROCESS_METHOD.MSDIAG_SETTINGS_PARAM, true, null)).map(serialize(_))
		val msdiagSettings = msdiagSettingsAsOptStr.map(deserialize[Map[String, Any]](_))
  
		val execCtx =  DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context
		
		logger.debug("GenerateMSDiagReport WS: going to launch MSDiag generator")

		val msDiagReportGenerator = new MSDiagReportGenerator(
			execCtx,
			resultSetId,
			msdiagSettings
		)
		logger.debug("GenerateMSDiagReport WS: report generated !")
		
		/// ************************************
		var result = true
		try {
			result = msDiagReportGenerator.runService
		} catch {
			case ex: Exception => {
				result = false
				logger.error("Error running MSDiag data Generator", ex)
				val msg = if (ex.getCause() != null) "Error running MSDiag report Generator " + ex.getCause().getMessage()
						else "Error running MS Diag report Generator " + ex.getMessage()
				throw new Exception(msg)
			}
		} finally {
			try {
				execCtx.closeAll()
			} catch {
				case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
			}
		}

		msDiagReportGenerator.resultHashMapJson
	}

}