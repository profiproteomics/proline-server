package fr.proline.cortex.service.dps.uds

import scala.Array.canBuildFrom
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.primitives.toLong
import fr.proline.cortex.service.AbstractRemoteProcessService
import fr.proline.cortex.service.dps.msi.ValidateResultSet
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.service.uds.IdentificationTreeValidator

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
class ValidateIdentDSInTree extends AbstractRemoteProcessService with LazyLogging {

  /* JMS Service identification */
  val serviceName = "proline/dps/uds/ValidateIdentDSInTree"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require((paramsRetriever != null), "No parameter specified")

    val projectId = paramsRetriever.getLong("project_id")
    val parentDsIds = paramsRetriever.getList("parent_dataset_ids").toArray.map { id => toLong(id) }
    val mergeResultSets = paramsRetriever.getBoolean("merge_result_sets")
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