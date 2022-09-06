package fr.proline.cortex.api.service.dps.msi

import fr.profi.util.jsonrpc.{IJSONRPC2Method, JSONRPC2DefaultMethod, JSONRPC2DefaultMethodParameter, JSONRPC2MethodResult}
import fr.proline.jms.service.api.{IDefaultServiceVersion, RemoteServiceIdentity}

import scala.reflect.runtime.universe.typeOf

object CertifyResultFilesService extends ICertifyResultFilesService

trait ICertifyResultFilesService extends IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "CertifyResultFiles"
  this.serviceDescription = Some(
    "Verify result files integrity before importing them in the MSIdb. "+
    "This service should be called before the service ImportResultFiles."
  )
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Verify result files integrity before importing them in the MSIdb."
    
    // Configure method interface
    val parameters = List(PROJECT_ID_PARAM, RESULT_FILES_PARAM, IMPORTER_PROPERTIES_PARAM)
    val returns =  JSONRPC2MethodResult(
      typeOf[String],
      "OK if service ran successfully; Error message if service was not successful"
    )
 
    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
      val description = "The id of the project used for data importation."
      val scalaType = typeOf[Long]
    }
    object RESULT_FILES_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "result_files"
      val description = "The list of the result files to be imported."
      val scalaType = typeOf[Array[IResultFileDescriptor]]
    }
    object IMPORTER_PROPERTIES_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "importer_properties"
      val description = "Property map specific to the result file type."
      val scalaType = typeOf[Map[String, Any]]
      optional = true
    }
  } 
}

