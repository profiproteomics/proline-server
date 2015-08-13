package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.cortex.service.AbstractRemoteProcessService
import fr.proline.cortex.util.DbConnectionHelper
import fr.profi.util.primitives._
import fr.proline.module.fragment_match.service.SpectrumMatchesGenerator

/**
 *  Define JMS Service which allows to generate spectrum matches
 *
 *  Input params :
 *    project_id : The id of the project
 *    result_set_id : The id of the Result Set containing the peptide matches and spectrum.
 *    result_summary_id: The id of the Result Summary containing the set of peptide matches for which spectrum matches will be generated.
 *    peptide_match_ids: The array of peptide_match Ids to consider.
 *    force_insert: Specify if existing spectrum matches should be replaced.
 *
 *  Output params :
 *    Boolean for service run status
 */

class GenerateSpectrumMatches extends AbstractRemoteProcessService with Logging {

	/* JMS Service identification */
	val serviceName = "proline/dps/msi/GenerateSpectrumMatches";
	val serviceVersion = "1.0";
	override val defaultVersion = true;

	override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

			require((paramsRetriever != null), "no parameter specified");

			val projectId = paramsRetriever.getLong("project_id");
			val resultSetId = paramsRetriever.getLong("result_set_id");
			val resultSummaryId = if (paramsRetriever.hasParam("result_summary_id")) Some(paramsRetriever.getLong("result_summary_id")) else None;
			val peptideMatchIds = Option(paramsRetriever.getOptList("peptide_match_ids", null)).map { _.toArray.map(toLong(_)) };
			val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, projectId, true); // Use JPA context
			val forceInsert = paramsRetriever.getOptBoolean("force_insert", false);

			val spectrumMatchesGenerator = new SpectrumMatchesGenerator(execCtx, resultSetId, resultSummaryId, peptideMatchIds, forceInsert);

			var result : java.lang.Boolean = true;
			try {
				result = spectrumMatchesGenerator.runService;
			} catch {
				case ex: Exception => {
					result = false;
					logger.error("Error running Spectrum Matches Generator", ex);
					val msg = if (ex.getCause() != null) { "Error running Spectrum Matches Generator " + ex.getCause().getMessage() } else { "Error running Spectrum Matches Generator " + ex.getMessage() };
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