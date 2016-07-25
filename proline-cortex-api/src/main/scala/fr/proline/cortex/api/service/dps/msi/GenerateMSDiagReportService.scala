package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object GenerateMSDiagReportService extends IGenerateMSDiagReportService

trait IGenerateMSDiagReportService extends IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "GenerateMSDiagReport"
  this.serviceDescription = Some(
    "Generate MSDiag report."
  )
  
  /* Configure the service interface */
  val serviceParams = List(PROJECT_ID_PARAM, RESULT_SET_ID_PARAM, MSDIAG_SETTINGS_PARAM)
  val serviceResult = JSONRPC2MethodResult(
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
    val description = null
    val scalaType = typeOf[Map[String, Any]]
  }
  
}

