package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult

object FilterRSMProteinSetsService extends IFilterRSMProteinSetsService

trait IFilterRSMProteinSetsService extends IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "FilterRSMProteinSets"
  this.serviceDescription = Some(
    "Filters protein sets of a given result summary.")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Filters protein sets of a given result summary."
    val parameters = List(
      PROJECT_ID_PARAM,
      RESULT_SUMMARY_ID_PARAM,
      PROT_SET_FILTERS_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "True if the service ran successfully, false otherwise.")

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

}

