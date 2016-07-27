package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult

object GetConnectionTemplateService extends IGetConnectionTemplateService

trait IGetConnectionTemplateService extends IAdminService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "GetConnectionTemplate"
  this.serviceDescription = Some("Returns a set of properties needed to access the Proline DataStore (UDS db access and JMS Server information).")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "A Map of JDBC connection properties and JMS server information."
    val parameters = List()
    val returns = JSONRPC2MethodResult(
      description = "A Map of JDBC connection properties and JMS server information.",
      scalaType = typeOf[Map[String, Object]])

  }

}
