package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.util.jsonrpc._

case class FilterConfig(
  parameter: String,
  threshold: AnyVal,
  postValidation : Boolean = false
)

case class PepMatchValidatorConfig(
  parameter: String,
  threshold: Option[AnyVal] = None,
  @JsonDeserialize(contentAs = classOf[java.lang.Float] )
  expectedFdr: Option[Float] = None
)

case class ProtSetValidatorConfig(
  validationMethod: String,
  parameter: String,
  thresholds: Option[Map[String, AnyVal]] = None,
  @JsonDeserialize(contentAs = classOf[java.lang.Float] )
  expectedFdr: Option[Float] = None
)

object ValidateResultSetService extends IValidateResultSetService

trait IValidateResultSetService extends IValidateResultSetServiceParams with IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "ValidateResultSet"
  this.serviceDescription = Some("Filters and validates a Result Set for a given set of rules.")
  
  // Configure service interface
  val serviceParams = List(
    PROJECT_ID_PARAM,
    RESULT_SET_ID_PARAM,
    DESCRIPTION_PARAM,
    USE_TD_COMPETITION_PARAM,
    PEP_MATCH_FILTERS_PARAM,
    PEP_MATCH_VALIDATOR_CONFIG_PARAM,
    PEP_SET_SCORE_TYPE_PARAM,
    PROT_SET_FILTERS_PARAM,
    PROT_SET_VALIDATOR_CONFIG_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    description = "The generated ResultSummary ID.",
    scalaType = typeOf[Long]
  )
  
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project the result set belongs to."
    val scalaType = typeOf[Long]
  }

}

trait IValidateResultSetServiceParams {
  object RESULT_SET_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_set_id"
    val description = "The id of the result set to validate."
    val scalaType = typeOf[Long]
  }
  object DESCRIPTION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "description"
    val description = "Description associated to the created result summary."
    val scalaType = typeOf[String]
  }
  object USE_TD_COMPETITION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "use_td_competition"
    val description = "[DEPRECATED] enable the use of Target Decoy Competition when validating PSMs."
    val scalaType = typeOf[Boolean]
    optional = true
  }
  object PEP_MATCH_FILTERS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "pep_match_filters"
    val description = "List of PSMs filters to apply (name, threshold)."
    val scalaType = typeOf[Array[FilterConfig]]
    optional = true
  }
  object PEP_MATCH_VALIDATOR_CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "pep_match_validator_config"
    val description = "PSM Validator config to use: filter type + expected FDR."
    val scalaType = typeOf[PepMatchValidatorConfig]
    optional = true
  }
  object PEP_SET_SCORE_TYPE_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "pep_set_score_type"
    val description = 
      "The type of score to use for peptide sets. "+
      "Valid values are: 'mascot:standard score', 'mascot:mudpit score', 'mascot:modified mudpit score'."
    val scalaType = typeOf[String]
    optional = true
  }
  object PROT_SET_FILTERS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "prot_set_filters"
    val description = "List of Proteins set filters to apply (name, threshold)."
    val scalaType = typeOf[Array[FilterConfig]]
    optional = true
  }
  object PROT_SET_VALIDATOR_CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "prot_set_validator_config"
    val description = "Protein Set validator config to use: expected FDR by category."
    val scalaType = typeOf[ProtSetValidatorConfig]
    optional = true
  }
}