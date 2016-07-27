package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult


object DeleteOrphanDataService extends IDeleteOrphanDataService

trait IDeleteOrphanDataService extends IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "DeleteOrphanData"
  this.serviceDescription = Some(
    "Remove orphan data from the MSI database (result summaries and result sets)."
  )
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)
 
   object PROCESS_METHOD extends JSONRPC2DefaultMethod {
      
     // Method description
     val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
     val description = "Creates a new Proline project"
    val parameters = List(PROJECT_ID_PARAM, RESULT_SET_IDS_PARAM, RESULT_SUMMARY_IDS_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "True if the service ran successfully, false otherwise."
    )
  
      object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
        val name = "project_id"
        val description = "The id of the project te orphan RS/RSM will be searched in."
        val scalaType = typeOf[Long]
      }
      object RESULT_SET_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
        val name = "result_set_ids"
        val description = "The list of the result sets to be removed."
        val scalaType = typeOf[Array[Long]]
        optional = true
      }
      object RESULT_SUMMARY_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
        val name = "result_summary_ids"
        val description = "The list of the result summaries to be removed."
        val scalaType = typeOf[Array[Long]]
        optional = true
      }
  }
}

