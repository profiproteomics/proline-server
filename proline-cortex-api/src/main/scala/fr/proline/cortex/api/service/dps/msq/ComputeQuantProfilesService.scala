package fr.proline.cortex.api.service.dps.msq

import scala.reflect.runtime.universe.typeOf
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object ComputeQuantProfilesService extends IComputeQuantProfilesService

trait IComputeQuantProfilesService extends IMsqService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "ComputeQuantProfiles"
  this.serviceDescription = Some(
    "Computes quantitative profiles of peptides and protein sets."
  )
  
  /* Configure the service interface */
  val serviceParams = List(PROJECT_ID_PARAM, MASTER_QUANT_CHANNEL_ID_PARAM, CONFIG_PARAM)
  val serviceResult = JSONRPC2MethodResult(
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
    val description = "The id of master quant channel correspoding to this quantitative dataset."
    val scalaType = typeOf[Long]
  }
  object CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "config"
    val description = "Configuration used for the computation of quantitative profiles."
    val scalaType = typeOf[Object] // ProfilizerConfig
  }
  
}

