package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

object GenerateMSDiagReportService extends IGenerateMSDiagReportService

trait IGenerateMSDiagReportService extends IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "GenerateMSDiagReport"
  this.serviceDescription = Some("Generate MSDiag report.")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get
    
    // Configure method interface
    val parameters = List(PROJECT_ID_PARAM, RESULT_SET_ID_PARAM, MSDIAG_SETTINGS_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[String],
      "The report results as a JSON string."
    )

    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = null
      val scalaType = typeOf[Long]
    }
    object RESULT_SET_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "result_set_id"
      val description = "The id of the Result Set containing the peptide matches and spectrum."
      val scalaType = typeOf[Long]
    }
    object MSDIAG_SETTINGS_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "msdiag_settings"
      val description = "Settings for msDiag report."
      val scalaType = typeOf[Map[String, Any]]
    }
  }
}

