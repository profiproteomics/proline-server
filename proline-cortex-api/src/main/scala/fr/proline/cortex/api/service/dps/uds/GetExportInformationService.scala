package fr.proline.cortex.api.service.dps.uds

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult

object GetExportInformationService extends IGetExportInformationService

trait IGetExportInformationService extends IUdsService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "GetExportInformation"
  this.serviceDescription = Some("Get information about the export default configuration file for a dataset.")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Creates a new Proline project"
    val parameters = List(PROJECT_ID_PARAM, DATASET_ID_PARAM, EXTRA_PARAMS_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[String],
      "The JSON String corresponding to the default export configuration of specified mode.")

    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The project ID."
      val scalaType = typeOf[Long]
    }

    object DATASET_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "dataset_id"
      val description = "The dataset ID."
      val scalaType = typeOf[Long]
    }

    object EXTRA_PARAMS_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "extra_params"
      val description = "A map of specific parameters : export_mode can contain the mode (IDENT, QUANT_SC or QUANT_XIC)."
      val scalaType = typeOf[Map[String, Any]]
    }
  }
}
