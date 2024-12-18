package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.primitives.toLong
import fr.proline.cortex.api.service.dps.msi.IGenerateSpectrumMatchesService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.module.fragmentmatch.service.SpectrumMatchesGenerator

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

class GenerateSpectrumMatches extends AbstractRemoteProcessingService with IGenerateSpectrumMatchesService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSetId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SET_ID_PARAM)
    val resultSummaryId = if (paramsRetriever.hasParam(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)) Some(paramsRetriever.getLong(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)) else None
    val peptideMatchIds = Option(paramsRetriever.getOptList(PROCESS_METHOD.PEPTIDE_MATCH_IDS_PARAM, null)).map { _.toArray.map(toLong(_)) }
    val forceInsert = paramsRetriever.getOptBoolean(PROCESS_METHOD.FORCE_INSERT_PARAM, false)
    val fragmentationRuleSetIdOpt = if(paramsRetriever.hasParam(PROCESS_METHOD.FRAGMENTATION_RULE_SET_ID_PARAM)) Some(paramsRetriever.getLong(PROCESS_METHOD.FRAGMENTATION_RULE_SET_ID_PARAM)) else None

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context
    val spectrumMatchesGenerator = new SpectrumMatchesGenerator(execCtx, resultSetId, resultSummaryId, peptideMatchIds,fragmentationRuleSetIdOpt, forceInsert)

    try {
      spectrumMatchesGenerator.runService()
    } catch {
      case t: Throwable => {
        throw ExceptionUtils.wrapThrowable("Error while generating Spectrum Matches", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

  }

}