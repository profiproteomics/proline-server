package fr.proline.cortex.api.service.dps.msq

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object QuantifyService extends IQuantifyService

trait IQuantifyService extends IMsqService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "Quantify"
  this.serviceDescription = Some(
    "Creates a new quantitation and perform the corresponding data analysis."
  )
  
  /* Configure the service interface */
  val serviceParams = List(
    NAME_PARAM,
    DESCRIPTION_PARAM,
    PROJECT_ID_PARAM,
    METHOD_ID_PARAM,
    EXPERIMENTAL_DESIGN_PARAM,
    QUANTITATION_CONFIG_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Boolean],
    "True if the service ran successfully, false otherwise."
  )
  
  object NAME_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "name"
    val description = "The quantitation name."
    val scalaType = typeOf[String]
  }
  object DESCRIPTION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "description"
    val description = "The quantitation description."
    val scalaType = typeOf[String]
  }
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project the quantitation will be created in."
    val scalaType = typeOf[Long]
  }
  object METHOD_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "method_id"
    val description = "The id of the quantitative method to be used."
    val scalaType = typeOf[Long]
  }
  object EXPERIMENTAL_DESIGN_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "experimental_design"
    val description = "The experimental design related to this quantitation."
    val scalaType = typeOf[Object] // ExperimentalDesign
  }
  object QUANTITATION_CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "quantitation_config"
    val description = "The parameters to use in order to perform a specific quantitative method (see quantitative methods documentation)."
    val scalaType = typeOf[Map[String,Any]]
  }
  
}

