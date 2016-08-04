package fr.proline.cortex.service.dps.uds

import scala.Array.canBuildFrom

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.primitives.toLong
import fr.proline.core.service.uds.IdentificationTreeValidator
import fr.proline.cortex.api.service.dps.uds.IValidateIdentDSInTreeService
import fr.proline.cortex.service.dps.msi.ValidateResultSet
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  Define JMS Service which validates all RS associated to Identification dataset of specific DS hierarchy, and creates appropriates ResultSummaries. 
 *  Specified PSMs, Proteins filters and validations are applied. 
 * 
 * Input params :
 *  project_id : The id of the project result set to validate belongs to.
 *  parent_dataset_ids : The id list of parent datasets.
 *  merge_result_sets : If true, merge operation is perfomed on result sets. Otherwise it is performed on result summaries.
 *  pep_match_filters : List of PSM filters to use (parameter, threshold and post_validation)
 *  pep_match_validator_config : PSM validation configuration (as PepMatchValidatorConfig : parameter, threshold, expectedFdr)
 *  pep_set_score_type : PeptideSet Scoring to use, one of PepSetScoring (mascot:standard score, mascot:modified mudpit score) 
 *  prot_set_filters : List of ProteinSet filters to use (parameter, threshold)
 *  prot_set_validator_config : ProteinSet validation configuration  (as ProtSetValidatorConfig : validation_method, parameter, thresholds, expectedFdr)
 * 
 * Output params :
 *   generated ResultSummary ID 
 */
// TODO: rename the file to ValidateIdentDSInTree.scala
class ValidateIdentDSInTree extends AbstractRemoteProcessingService with IValidateIdentDSInTreeService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    require(paramsRetriever != null, "No parameter specified")

    val projectId = paramsRetriever.getLong(PROJECT_ID_PARAM)
    val parentDsIds = paramsRetriever.getList(PROCESS_METHOD.PARENT_DATASET_IDS_PARAM).toArray.map { id => toLong(id) }
    val mergeResultSets = paramsRetriever.getBoolean(PROCESS_METHOD.MERGE_RESULT_SETS_PARAM)
//    val useTdCompet = paramsRetriever.getOptBoolean("use_td_competition", false)
    val validationConfig = ValidateResultSet.parseValidationConfig(paramsRetriever)
   
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)
    
    try {
      
      IdentificationTreeValidator.validateIdentificationTrees(
        execCtx,
        parentDsIds,
        mergeResultSets,
        false, //useTdCompet : DEPRECATED 
        validationConfig
      )
      
    } finally {
       try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }
   
    true.asInstanceOf[Object]
  }

}