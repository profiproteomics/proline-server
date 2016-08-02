package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import com.fasterxml.jackson.databind.annotation.JsonDeserialize

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult
import fr.profi.util.reflect.FieldDescription

trait IResultFileDescriptor {

  val format: String
  val path: String
  val peaklistId: Option[Long]
}

/**
 * format :  The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx').
 * path : The relative path of the file to be imported. The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file.
 * decoyStrategy : The Regular expression used to detect decoy protein matches.
 * peaklistId : The id of the software which has been used to generate the peaklist.
 */
case class ResultFileDescriptorsDecoyRegExp(

  // format parameter
  @FieldDescription(
    content = "The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx', 'xtandem.xml')."
  )
  format: String,

  // path parameter
  @FieldDescription( content =
    "The relative path of the file to be imported." +
    "The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file."
  )
  path: String,

  // decoyStrategy parameter
  @FieldDescription(
    content = "The Regular expression used to detect decoy protein matches."
  )
  decoyStrategy: Option[String] = None,

  // peaklistId parameter
  @FieldDescription(content = "The id of the peaklist that should correspond to this result file.")
  @JsonDeserialize(contentAs = classOf[java.lang.Long])
  peaklistId: Option[Long] = None
  
) extends IResultFileDescriptor

/**
 * format :  The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx').
 * path : The relative path of the file to be imported. The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file.
 * protMatchDecoyRuleId : The id of the rule to be used to detect decoy protein matches.
 * peaklistId : The id of the software which has been used to generate the peaklist.
 */
case class ResultFileDescriptorRuleId(

  // format parameter
  @FieldDescription(
    content = "The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx', 'xtandem.xml')."
  )
  format: String,

  // path parameter
  @FieldDescription(content =
    "The relative path of the file to be imported." +
    "The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file."
  )
  path: String,

  // peaklistId parameter
  @FieldDescription(content = "The id of the peaklist that should correspond to this result file.")
  @JsonDeserialize(contentAs = classOf[java.lang.Long])
  peaklistId: Option[Long] = None,

  // protMatchDecoyRuleId parameter
  @FieldDescription(content = "The id of the rule to be used to detect decoy protein matches.")
  @JsonDeserialize(contentAs = classOf[java.lang.Long])
  protMatchDecoyRuleId: Option[Long] = None
  
) extends IResultFileDescriptor

//// Static object only defined to reuse its params in other static objects
//object ImportResultFilesService extends IImportResultFilesService {
//  val serviceVersion = "NONE"
//}

trait IImportResultFilesService extends IMsiService with IImportResultFilesServiceParams {

  /* JMS Service identification */
  val serviceLabel = "ImportResultFiles"
  this.serviceDescription = Some(
    "Import a result file in the MSIdb corresponding to the provided project id."
  )
  
}

trait IImportResultFilesServiceParams {
  object RESULT_FILES_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_files"
    val description = "The list of the result files to be imported."
    val scalaType = typeOf[Array[IResultFileDescriptor]]
  }
  object INSTRUMENT_CONFIG_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "instrument_config_id"
    val description = "ID in datastore of the instrument config used for result file acquisition."
    val scalaType = typeOf[Long]
  }
  object PEAKLIST_SOFTWARE_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "peaklist_software_id"
    val description = "ID in datastore of the software use to generate peaklist."
    val scalaType = typeOf[Long]
  }
  object SAVE_SPECTRUM_MATCHES_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "save_spectrum_matches"
    val description = "If true, fragment matches of MS/MS spectra will be stored in the MSIdb."
    val scalaType = typeOf[Long]
  }
  object IMPORTER_PROPERTIES_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "importer_properties"
    val description = "Map of properties for importer, specific to result files format."
    val scalaType = typeOf[Map[String, Any]]
    optional = true
  } 
  object IMPORT_PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project used for data importation."
    val scalaType = typeOf[Long]
  }

  case class ImportedResultFile(
    @FieldDescription(content = "The path of the imported file.")
    path: String,

    @FieldDescription(content = "ID of created target result set.")
    var targetResultSetId: Long = -1L
  )
}


object ImportResultFilesServiceV1_0 extends IImportResultFilesServiceV1_0

trait IImportResultFilesServiceV1_0 extends IImportResultFilesService with IDefaultServiceVersion {

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)
  
  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IImportResultFilesServiceParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Import a result file in the MSIdb corresponding to the provided project id."
    
    // Configure method interface
    val parameters = List(
      IMPORT_PROJECT_ID_PARAM,
      RESULT_FILES_PARAM_V1_0,
      INSTRUMENT_CONFIG_ID_PARAM,
      PEAKLIST_SOFTWARE_ID_PARAM,
      SAVE_SPECTRUM_MATCHES_PARAM,
      IMPORTER_PROPERTIES_PARAM
    )
    val returns = JSONRPC2MethodResult(
      // TODO: create a case class for these parameters
      typeOf[Array[ImportedResultFile]],
      "List of ImportedResultFile: path of imported file and id of created target RS."
    )

    object RESULT_FILES_PARAM_V1_0 extends JSONRPC2DefaultMethodParameter {
      val name = "result_files"
      val description = "The list of the result files to be imported."
      val scalaType = typeOf[Array[ResultFileDescriptorsDecoyRegExp]]
    }
  }
}

object ImportResultFilesServiceV2_0 extends IImportResultFilesServiceV2_0

trait IImportResultFilesServiceV2_0 extends IImportResultFilesService {

  val serviceVersion = "2.0"
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)
  
  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IImportResultFilesServiceParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Import a result file in the MSIdb corresponding to the provided project id."
    
    // Configure method interface
    val parameters = List(
      IMPORT_PROJECT_ID_PARAM,
      RESULT_FILES_PARAM_V2_0,
      INSTRUMENT_CONFIG_ID_PARAM,
      PEAKLIST_SOFTWARE_ID_PARAM,
      SAVE_SPECTRUM_MATCHES_PARAM,
      IMPORTER_PROPERTIES_PARAM
    )
    val returns = JSONRPC2MethodResult(
      // TODO: create a case class for these parameters
      typeOf[Array[ImportedResultFile]],
      "List of ImportedResultFile: path of imported file and id of created target RS."
    )

    object RESULT_FILES_PARAM_V2_0 extends JSONRPC2DefaultMethodParameter {
      val name = "result_files"
      val description = "The list of the result files to be imported."
      val scalaType = typeOf[Array[ResultFileDescriptorRuleId]]
    }
  }
}

