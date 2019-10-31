package fr.proline.cortex.api.service.misc

import scala.reflect.runtime.universe.typeOf

import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult
import fr.proline.jms.service.api.IDefaultServiceVersion

object CancelService extends ICancelService

trait ICancelService extends IMiscService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "CancelService"
  this.serviceDescription = Some("Cancels a given running service.")
  
  /* List the handled methods */
  val methodDefinitions = List(CANCEL_METHOD)
  
  object CANCEL_METHOD extends JSONRPC2DefaultMethod {    
    val name = "cancel"
    val description = "Return hard-coded Proline directory types (result_files, raw_files, mzdb_files)."
    val parameters = List(MESSAGE_ID_PARAM)
    val returns = JSONRPC2MethodResult(typeOf[Boolean])
    
    // Method parameters definitions
    object MESSAGE_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "message_id"
      val description = "The JMS message ID corresponding to the service request that was performed."
      val scalaType = typeOf[String]
    }
  }

}
