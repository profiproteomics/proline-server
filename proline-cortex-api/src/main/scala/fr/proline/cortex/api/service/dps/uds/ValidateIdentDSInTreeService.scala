package fr.proline.cortex.api.service.dps.uds

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.cortex.util.jsonrpc.JSONRPC2MethodResult
import fr.proline.cortex.api.service.dps.msi.ValidateResultSetService
import fr.proline.cortex.api.service.dps.msi.IValidateResultSetServiceParams

object ValidateIdentDSInTreeService extends IValidateIdentDSInTreeService

trait IValidateIdentDSInTreeService extends IValidateResultSetServiceParams with IUdsService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "ValidateIdentDSInTree"
  this.serviceDescription = Some(
    "Validates all result sets associated to an identification dataset hierarchy." +
    "Creates appropriate result summaries after having applied provided filters. " 
  )
  
  /* Configure the service interface */
  val serviceParams = List(
    PROJECT_ID_PARAM,
    PARENT_DATASET_IDS_PARAM,
    MERGE_RESULT_SETS_PARAM,
    USE_TD_COMPETITION_PARAM,
    PEP_MATCH_FILTERS_PARAM,
    PEP_MATCH_VALIDATOR_CONFIG_PARAM,
    PEP_SET_SCORE_TYPE_PARAM,
    PROT_SET_FILTERS_PARAM,
    PROT_SET_VALIDATOR_CONFIG_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    typeOf[String],
    "The JSON String corresponding to the default export configuration of specified mode."
  )
  
  val PROJECT_ID_PARAM = ValidateResultSetService.PROJECT_ID_PARAM
  
  object PARENT_DATASET_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "parent_dataset_ids"
    val description = "The id list of parent datasets to validate."
    val scalaType = typeOf[Array[Long]]
  }
  object MERGE_RESULT_SETS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "merge_result_sets"
    val description = "If true, merge operation is perfomed on result sets. Otherwise it is performed on result sumamries."
    val scalaType = typeOf[Boolean]
  }
  
}
