package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.proline.jms.util.jsonrpc.IJSONRPC2Method
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.jms.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.jms.util.jsonrpc.JSONRPC2MethodResult

object CreateProjectService extends ICreateProjectService

trait ICreateProjectService extends IAdminService  with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "CreateProject"
  this.serviceDescription = Some(
    "Creates a new Proline project of a given name.\n" +
    "This Service will store project related data in the UDSdb and will create required project databases (MSIdb, LCMSdb...)"
  )
  
 // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)
 
   object PROCESS_METHOD extends JSONRPC2DefaultMethod {
      
     // Method description
     val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
     val description = "Creates a new Proline project"
     val parameters = List(NAME_PARAM, DESCRIPTION_PARAM, OWNER_ID_PARAM)
     val returns = JSONRPC2MethodResult(typeOf[Long], "The ID of the created project.")     
     
  
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
}

