package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe.typeOf
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object CreateProjectService extends ICreateProjectService

trait ICreateProjectService extends IAdminService with IRemoteProcessingService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "CreateProject"
  this.serviceDescription = Some(
    "Creates a new Proline project of a given name.\n" +
    "This Service will store project related data in the UDSdb and will create required project databases (MSIdb, LCMSdb...)"
  )
  
  /* Configure the service interface */
  val serviceParams = List(NAME_PARAM, DESCRIPTION_PARAM, OWNER_ID_PARAM)
  val serviceResult = JSONRPC2MethodResult(typeOf[Long], "The ID of the created project.")
  
  object NAME_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "name"
    val description = "The project name"
    val scalaType = typeOf[String]
  }
  
  object DESCRIPTION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "description"
    val description = "The project description"
    val scalaType = typeOf[String]
    optional = true
  }
  
  object OWNER_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "owner_id"
    val description = "The project owner ID"
    val scalaType = typeOf[Long]
  }
  
}

