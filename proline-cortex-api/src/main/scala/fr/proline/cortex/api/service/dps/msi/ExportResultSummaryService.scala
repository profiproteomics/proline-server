package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.profi.util.lang.EnhancedEnum
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult
import fr.profi.util.reflect.FieldDescription

object ExportFileFormat extends EnhancedEnum {
  val MZIDENTML, TEMPLATED, PRIDE, SPECTRA_LIST = Value
}

object ExportTemplateName extends EnhancedEnum {
  val ALL_PEP_MATCHES_XLSX, IRMA_LIKE_TSV, IRMA_LIKE_XLSX, IRMA_LIKE_FULL_XLSX, PROLINE_XLSX = Value
}

object ExportOutputMode extends EnhancedEnum {
  val FILE, STREAM = Value
}

case class ExportResultSummaryIdentifier(

  @FieldDescription(
    content = "The ID of the project this result summary belongs to."
  )
  projectId: Long,

  @FieldDescription(
    content = "The ID of the data set corresponding to the result summary being exported."
  )
  dsId: Option[Long],

  @FieldDescription(
    content = "The ID of the result summary to export."
  )
  rsmId: Long
)

trait IExportResultSummaryService extends IMsiService {

  /* JMS Service identification */
  val serviceLabel = "ExportResultSummary"
  this.serviceDescription = Some(
    "Exports result summaries on server side in various files formats."
  )

  val serviceResult = JSONRPC2MethodResult(
    typeOf[AnyRef],
    "The file paths in FILE output mode. The file paths and PROLINE_NODE_ID in STREAM output mode." +
    "Call the ResourceService using the PROLINE_NODE_ID and the file paths."
  )

  object FILE_FORMAT_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "file_format"
    val description = "The expected file format. Valid values are: MZIDENTML, TEMPLATED, PRIDE."
    val scalaType = typeOf[String]
  }
  object FILE_NAME_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "file_name"
    val description = "The desired name for the file. If not provided, a name beginning with 'IdentificationSummaryExport_' will be generated."
    val scalaType = typeOf[String]
    optional = true
  }
  object FILE_DIRECTORY_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "file_directory"
    val description = "The desired output directory. If not provided, files will be exported in a temporary directory."
    val scalaType = typeOf[String]
    optional = true
  }
  object OUTPUT_MODE_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "output_mode"
    val description = "Specify the output mode to be used. Valid values are: FILE, STREAM."
    val scalaType = typeOf[String]
    optional = true
  }
  object EXTRA_PARAMS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "extra_params"
    val description = "A map of parameters specific to the used file format. Possible keys for the TEMPLATED format: template_name."
    val scalaType = typeOf[Map[String, Any]]
    optional = true
  }
}

object ExportResultSummaryServiceV1_0 extends IExportResultSummaryServiceV1_0

trait IExportResultSummaryServiceV1_0 extends IExportResultSummaryService with IDefaultServiceVersion {

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {
    
    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Exports result summaries on server side in various files formats."
    
    // Configure method interface
    val parameters = List(
      RSM_IDENTIFIER_PARAM,
      FILE_FORMAT_PARAM,
      FILE_NAME_PARAM,
      FILE_DIRECTORY_PARAM,
      OUTPUT_MODE_PARAM, // FILE || STREAM
      EXTRA_PARAMS_PARAM
    )
    val returns = serviceResult

    object RSM_IDENTIFIER_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "rsm_identifier"
      val description = "A tuple containing the project id and the result summary id."
      val scalaType = typeOf[Array[ExportResultSummaryIdentifier]]
    }
  }
}

object ExportResultSummariesServiceV2_0 extends IExportResultSummariesServiceV2_0

trait IExportResultSummariesServiceV2_0 extends IExportResultSummaryService {

  /* JMS Service identification */
  // TODO: rename the serviceLabel to ExportResultSummaries
  val serviceVersion = "2.0"

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Exports result summaries on server side in various files formats."
    
    // Configure method interface
    val parameters = List(
      RSM_IDENTIFIERS_PARAM,
      FILE_FORMAT_PARAM,
      FILE_NAME_PARAM,
      FILE_DIRECTORY_PARAM,
      OUTPUT_MODE_PARAM, // FILE || STREAM
      EXTRA_PARAMS_PARAM
    )
    val returns = serviceResult

    object RSM_IDENTIFIERS_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "rsm_identifiers"
      val description = "A tuple containing the project id, the datasetId, and the result summary id."
      val scalaType = typeOf[Array[ExportResultSummaryIdentifier]]
    }
  }

}

