package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.util.jsonrpc._
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteProcessingService

object GetConnectionTemplateService extends IGetConnectionTemplateService

trait IGetConnectionTemplateService extends IAdminService with IRemoteProcessingService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "GetConnectionTemplate"
  this.serviceDescription = Some("Returns a set of properties needed to access the Proline DataStore (UDS db access and JMS Server information).")

  val serviceParams = List()
  val serviceResult = JSONRPC2MethodResult(
    description = "A Map of JDBC connection properties and JMS server information.",
    scalaType = typeOf[Map[String,Object]]
  )
}
