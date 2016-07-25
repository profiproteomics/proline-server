package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

//object UpdateSpectraParamsService extends IUpdateSpectraParamsService

trait IUpdateSpectraParamsService extends IMsiService {
  
  /* JMS Service identification */
  val serviceLabel = "UpdateSpectraParams"
  this.serviceDescription = Some(
    "Update scan, cycle and time information of spectra belonging to specified peaklists. " +
    "Information are extracted from the spectrum title string by applying the regexes corresponding to the given parsing rule."
  )
  
  /* Configure the service interface */
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Int],
    "The number of updated spectra."
  )
  
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project the peaklists will be searched in."
    val scalaType = typeOf[Long]
  }
  
}

object UpdateSpectraParamsServiceV1_0 extends IUpdateSpectraParamsServiceV1_0

trait IUpdateSpectraParamsServiceV1_0 extends IUpdateSpectraParamsService with IDefaultServiceVersion {
  
  /* Configure the service interface */
  val serviceParams = List(PROJECT_ID_PARAM, PEAKLIST_IDS_PARAM, SPEC_TITLE_RULE_ID_PARAM)
  
  object PEAKLIST_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "peaklist_ids"
    val description = "The ids of the peaklists to update."
    val scalaType = typeOf[Array[Long]]
  }
  object SPEC_TITLE_RULE_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "spec_title_rule_id"
    val description = "The id of the spectrum title parsing rule to use."
    val scalaType = typeOf[Long]
  }
  
}

object UpdateSpectraParamsServiceV2_0 extends IUpdateSpectraParamsServiceV2_0

trait IUpdateSpectraParamsServiceV2_0 extends IUpdateSpectraParamsService {
  
  /* JMS Service identification */
  val serviceVersion = "2.0"
  val isDefaultVersion = false
  
  /* Configure the service interface */
  val serviceParams = List(PROJECT_ID_PARAM, RESULT_SET_IDS_PARAM, PEAKLIST_SOFTWARE_ID_PARAM)
  
  object RESULT_SET_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "resultset_ids"
    val description = "The ids of the resultsets to update. These RS should be SEARCH ResultSet."
    val scalaType = typeOf[Array[Long]]
  }
  object PEAKLIST_SOFTWARE_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "peaklist_software_id"
    val description = "The id in datastore of the software to use to update spectra params."
    val scalaType = typeOf[Long]
  }
  
}
