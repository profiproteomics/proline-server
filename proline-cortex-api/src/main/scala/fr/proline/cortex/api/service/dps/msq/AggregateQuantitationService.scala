package fr.proline.cortex.api.service.dps.msq

import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult


object AggregateQuantitationService extends IQuantifyService

trait IAggregateQuantitationService extends IMsqService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "AggregateQuantitation"
  this.serviceDescription = Some("Creates a new quantitation by aggregation existing quantitation.")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get
    
    // Configure method interface
    val parameters = List(
      PROJECT_ID_PARAM,
      NAME_PARAM,
      DESCRIPTION_PARAM,
      EXPERIMENTAL_DESIGN_PARAM,
      QUANTITATION_CONFIG_PARAM
    )
    val returns = JSONRPC2MethodResult(
      typeOf[Long],
      "True ID of the created quantitation."
    )

    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The id of the project the quantitation will be created in."
      val scalaType = typeOf[Long]
    }
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
    object EXPERIMENTAL_DESIGN_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "experimental_design"
      val description = "The experimental design related to this quantitation."
      val scalaType = typeOf[Object] // ExperimentalDesign
    }
    object QUANTITATION_CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "quantitation_config"
      val description = "The parameters to use in order to perform the aggregation."
      val scalaType = typeOf[Map[String, Any]]
    }
  }
}

