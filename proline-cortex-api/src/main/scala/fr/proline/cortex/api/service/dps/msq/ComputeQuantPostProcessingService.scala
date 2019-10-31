package fr.proline.cortex.api.service.dps.msq

import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity

import scala.reflect.runtime.universe.typeOf

object ComputeQuantPostProcessingService extends IComputeQuantPostProcessingService

trait IComputeQuantPostProcessingService extends  IMsqService with IDefaultServiceVersion {

  val serviceLabel = "PostProcessQuantitation"
  this.serviceDescription = Some(
    "Computes post process quantitative of peptides and protein sets."
  )

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)
  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get

    // Configure method interface
    val parameters = List(PROJECT_ID_PARAM, MASTER_QUANT_CHANNEL_ID_PARAM, CONFIG_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "True if the service ran successfully, false otherwise."
    )

    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The id of project this quantitative data set refers to."
      val scalaType = typeOf[Long]
    }
    object MASTER_QUANT_CHANNEL_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "master_quant_channel_id"
      val description = "The id of master quant channel corresponding to this quantitative dataset."
      val scalaType = typeOf[Long]
    }
    object CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "config"
      val description = "Configuration used for the computation of quantitative post process."
      val scalaType = typeOf[Map[String, Any]] // PostProcessingConfig
    }
  }
}
