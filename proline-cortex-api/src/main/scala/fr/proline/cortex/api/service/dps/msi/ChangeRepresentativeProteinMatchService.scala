package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._
import fr.proline.cortex.util.reflect.FieldDescription

case class RepresentativeProteinMatchRule(
  @FieldDescription(
    content = "Specifies the regular expression to use."
  )
  ruleRegex: String,
  
  @FieldDescription(
    content = "Indicates if the rule should be tested on the 'accession' (true) or on the 'description' (false)."
  )
  ruleOnAc: Boolean // TODO: rename to applyOnAc
)

object ChangeRepresentativeProteinMatchService extends IChangeRepresentativeProteinMatchService

trait IChangeRepresentativeProteinMatchService extends IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  // TODO: rename to "ChangeRepresentativeProteinMatch"
  val serviceLabel = "ChangeTypicalProteinMatch"
  this.serviceDescription = Some(
    "Selects the protein match representative of each protein set using a specified regular expressions. "+
    "These rules could be applied on protein accession or description, and will be applied using order priority."
  )
  
  /* Configure the service interface */
  val serviceParams = List(PROJECT_ID_PARAM, RESULT_SUMMARY_ID_PARAM, CHANGE_TYPICAL_RULES_PARAM)
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Boolean],
    "True if the service ran successfully, false otherwise."
  )
  
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project used for data importation."
    val scalaType = typeOf[Long]
  }
  object RESULT_SUMMARY_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_summary_id"
    val description = "The id of the Result Summary where the representative protein matches will be changed."
    val scalaType = typeOf[Long]
  }  
  object CHANGE_TYPICAL_RULES_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "change_typical_rules" // TODO: rename to 'rules'
    val description = "List of rules used to select the representative protein match of each protein set. "
    val scalaType = typeOf[RepresentativeProteinMatchRule]
  }
  
}

