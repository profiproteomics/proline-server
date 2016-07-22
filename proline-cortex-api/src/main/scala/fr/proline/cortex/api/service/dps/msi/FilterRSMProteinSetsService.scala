package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object FilterRSMProteinSetsService extends IFilterRSMProteinSetsService

trait IFilterRSMProteinSetsService extends IMsiService with IRemoteProcessingService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "FilterRSMProteinSets"
  this.serviceDescription = Some(
    "Filters protein sets of a given result summary."
  )
  
  /* Configure the service interface */
  val serviceParams = List(
    PROJECT_ID_PARAM,
    RESULT_SUMMARY_ID_PARAM,
    PROT_SET_FILTERS_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Boolean],
    "True if the service ran successfully, false otherwise."
  )
  
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The ID of the project the result summary belongs to."
    val scalaType = typeOf[Long]
  }
  object RESULT_SUMMARY_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_summary_id"
    val description = "The ID of the result summary to filter."
    val scalaType = typeOf[Long]
  }
  object PROT_SET_FILTERS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "prot_set_filters"
    val description = "List of Proteins set filters to apply (name, threshold)."
    val scalaType = typeOf[Array[FilterConfig]]    
  }
  
}

