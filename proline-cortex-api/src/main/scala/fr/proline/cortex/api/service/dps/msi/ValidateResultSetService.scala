package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import com.fasterxml.jackson.databind.annotation.JsonDeserialize

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

case class FilterConfig(
  parameter: String,
  threshold: AnyVal,
  postValidation: Boolean = false
) 
  

case class PepMatchValidatorConfig(
  parameter: String,
  threshold: Option[AnyVal] = None,
  @JsonDeserialize(contentAs = classOf[java.lang.Float])
  expectedFdr: Option[Float] = None
)

case class ProtSetValidatorConfig(
  validationMethod: String,
  parameter: String,
  thresholds: Option[Map[String, AnyVal]] = None,
  @JsonDeserialize(contentAs = classOf[java.lang.Float])
  expectedFdr: Option[Float] = None
)

object ValidateResultSetService extends IValidateResultSetService {
  val PARAMETER_PARAM_NAME = "parameter"
  val THRESHOLD_PARAM_NAME = "threshold"
  val THRESHOLDS_PARAM_NAME = "thresholds"
  val POST_VALIDATION_PARAM_NAME = "post_validation"
  val EXPECTED_FDR_PARAM_NAME = "expected_fdr"
  val VALIDATION_METHOD_PARAM_NAME = "validation_method"
}

trait IValidateResultSetService extends IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "ValidateResultSet"
  this.serviceDescription = Some("Filters and validates a Result Set for a given set of rules.")
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IValidateResultSetServiceParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get
    
    // Configure service interface
    val parameters = List(
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
    val returns = JSONRPC2MethodResult(
      description = "The generated ResultSummary ID.",
      scalaType = typeOf[Long]
    )

  }
}

trait IValidateResultSetServiceV2 extends IMsiService  {

  /* JMS Service identification */
  val serviceLabel = "ValidateResultSet"
  this.serviceDescription = Some("Filters and validates a Result Set for a given set of rules and eventually propagate the validation to child ResultSets")
  val serviceVersion= "2.0"
   
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IValidateResultSetServiceParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get
    
    // Configure service interface
    val parameters = List(
      PROJECT_ID_PARAM,
      RESULT_SET_ID_PARAM,
      DESCRIPTION_PARAM,
      USE_TD_COMPETITION_PARAM,
      PEP_MATCH_FILTERS_PARAM,
      PEP_MATCH_VALIDATOR_CONFIG_PARAM,
      PEP_SET_SCORE_TYPE_PARAM,
      PROT_SET_FILTERS_PARAM,
      PROT_SET_VALIDATOR_CONFIG_PARAM,
      PROPAGATE_PEP_MATCH_VALIDATION_PARAM,
      PROPAGATE_PROT_SET_VALIDATION_PARAM
    )
    val returns = JSONRPC2MethodResult(
      description = "The generated ResultSummary ID.",
      scalaType = typeOf[Long]
    )

  }
}



trait IValidateResultSetServiceParams {
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project the result set belongs to."
    val scalaType = typeOf[Long]
  }
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
      "The type of score to use for peptide sets. " +
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
  object PROPAGATE_PROT_SET_VALIDATION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "propagate_prot_set_filters"
    val description = "specify if protein set filters should be propagated to child resultset."
    val scalaType = typeOf[Boolean]
    optional = true
  }
  object PROPAGATE_PEP_MATCH_VALIDATION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "propagate_pep_match_filters"
    val description = "specify if peptide matches filters should be propagated to child resultset."
    val scalaType = typeOf[Boolean]
    optional = true
  }
  
}