package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult

object ImportMaxQuantResultsService extends IImportMaxQuantResultsService

trait IImportMaxQuantResultsService extends IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "ImportMaxQuantResultsService"
  this.serviceDescription = Some(
    "Import MaxQuant result file in the MSIdb corresponding to the provided project id.")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Creates a new Proline project"
    val parameters = List(
      PROJECT_ID_PARAM,
      RESULT_FILES_DIR_PARAM,
      INSTRUMENT_CONFIG_ID_PARAM,
      PEAKLIST_SOFTWARE_ID_PARAM)
    val returns = JSONRPC2MethodResult(
      // TODO: create a case class for these parameters
      typeOf[Map[String, Any]],
      "A map corresponding to import result:\n" +
        "- result_set_ids: list of all created result set ids\n" +
        "- warning_msg: information message that could be of interest for to caller (GUI for instance)")

    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The id of the project used for data importation."
      val scalaType = typeOf[Long]
    }
    object RESULT_FILES_DIR_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "result_files_dir"
      val description = "The path to folder or Zip file containing Result files to be imported."
      val scalaType = typeOf[String]
    }
    object INSTRUMENT_CONFIG_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "instrument_config_id"
      val description = null
      val scalaType = typeOf[Long]
    }
    object PEAKLIST_SOFTWARE_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "peaklist_software_id"
      val description = null
      val scalaType = typeOf[Long]
    }
  }
}
