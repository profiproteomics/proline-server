package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult
import fr.proline.jms.util.reflect.FieldDescription

object CertifyResultFilesService extends ICertifyResultFilesService

case class ResultFileDescriptor(
  @FieldDescription(
    content = "The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx', 'xtandem.xml')."
  )
  format: String,

  @FieldDescription(content =
    "The relative path of the file to be imported." +
    "The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file."
  )
  path: String
)

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
     val description = "Verify result files integrity before importing them in the MSIdb"
     val parameters =List(PROJECT_ID_PARAM, RESULT_FILES_PARAM, IMPORTER_PROPERTIES_PARAM)
     val returns =  JSONRPC2MethodResult(
        typeOf[Boolean],
        "True if the service ran successfully, false otherwise."
      )   
 
      object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
        val name = "project_id"
        val description = "The id of the project used for data importation."
        val scalaType = typeOf[Long]
      }
      object RESULT_FILES_PARAM extends JSONRPC2DefaultMethodParameter {
        val name = "result_files"
        val description = "The list of the result files to be imported."
        val scalaType = typeOf[Array[ResultFileDescriptor]]
      }
      object IMPORTER_PROPERTIES_PARAM extends JSONRPC2DefaultMethodParameter {
        val name = "importer_properties"
        val description = "Property map specific to the result file type."
        val scalaType = typeOf[Map[String, Any]]
        optional = true
      }
  } 
}
