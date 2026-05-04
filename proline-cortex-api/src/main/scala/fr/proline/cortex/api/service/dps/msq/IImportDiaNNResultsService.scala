package fr.proline.cortex.api.service.dps.msq

import fr.profi.util.jsonrpc.{IJSONRPC2Method, JSONRPC2DefaultMethod, JSONRPC2DefaultMethodParameter, JSONRPC2MethodResult}
import fr.profi.util.reflect.FieldDescription
import fr.proline.jms.service.api.{IDefaultServiceVersion, RemoteServiceIdentity}

import scala.collection.immutable
import scala.reflect.runtime.universe.typeOf


case class ImportedDiaNNResult(
                                @FieldDescription(content = "Map of RSM Ids by RS id.")
    rsmIdsByRsId: Map[java.lang.Long, java.lang.Long],

                                @FieldDescription(content = "Parent dataset Id containing RSM/RS.")
    identDataSetId: Long,

                                @FieldDescription(content = "ID of created quantitation dataset.")
    var quantDataSetId: Long = -1L

)

trait IImportDiaNNResultsService extends IMsqService with IDefaultServiceVersion {
  val serviceLabel = "ImportDiaNNResults"
  this.serviceDescription = Some(
    "Import DiaNN results and create associated Search results, Identification Summary and Quantitation datasets."
  )
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {
    // Method description
    val name: String = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description: String = serviceDescription.get

    // Configure method interface
    val parameters: immutable.Seq[JSONRPC2DefaultMethodParameter] = List(
      PROJECT_ID_PARAM,
      PARENT_DS_ID_PARAM,
      RESULT_FILES_DIR_PARAM,
      INSTRUMENT_CONFIG_ID_PARAM,
      PEAKLIST_SOFTWARE_ID_PARAM
    )
    val returns = JSONRPC2MethodResult(
      typeOf[ImportedDiaNNResult],
      "a DiaNNResult: list of created result summaries ids + Quantitation Dataset Id."
    )

    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The id of the project used for data importation."
      val scalaType = typeOf[Long]
    }
    object PARENT_DS_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "parent_dataset_id"
      val description = "The id of the parent dataset to import diaNN result to. If null or -1, import as root."
      val scalaType = typeOf[Long]
    }
    object RESULT_FILES_DIR_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "result_files_dir"
      val description = "The path to folder containing Result files to be imported."
      val scalaType = typeOf[String]
    }
    object INSTRUMENT_CONFIG_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "instrument_config_id"
      val description = "The id in the datastore of the instrument config used for result file acquisition."
      val scalaType = typeOf[Long]
    }
    object PEAKLIST_SOFTWARE_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "peaklist_software_id"
      val description = "The id in the datastore of the software used to generate the peaklist."
      val scalaType = typeOf[Long]
    }
  }
}