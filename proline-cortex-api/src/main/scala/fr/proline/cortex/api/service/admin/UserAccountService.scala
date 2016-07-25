package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe.typeOf
import fr.profi.util.lang.EnhancedEnum
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.cortex.util.jsonrpc._

object UserAccountService extends IUserAccountService

trait IUserAccountService extends IAdminService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "UserAccount"
  this.serviceDescription = Some("Service providing methods for user accounts mangement. ")
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(CREATE_METHOD,CHANGE_PASSWORD_METHOD,AUTHENTICATE_METHOD)
  
  val UDS_AUTH_METHOD = "UDS_HASH"
  
  // Description of the Create service method
  object CREATE_METHOD extends JSONRPC2DefaultMethod { // Emulate an enumeration of objects
    
    // Method description
    val name = "create"
    val description = "Creates a new user account."
    val parameters = List(LOGIN_PARAM,PASSWORD_HASH_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Long],
      description = "The ID of the created user account."
    )
    
    // Method parameters definitions
    object LOGIN_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "login"
      val description = "The new user login."
      val scalaType = typeOf[String]
    }    
    object PASSWORD_HASH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "password_hash"
      val description = "The password hash of the new user."
      val scalaType = typeOf[String]
    }
    
  }
  
  object CHANGE_PASSWORD_METHOD extends JSONRPC2DefaultMethod {
    
    val name = "change_password"
    val description = "Change the password of the specified user."
    val parameters = List(LOGIN_PARAM,OLD_PASSWORD_HASH_PARAM,NEW_PASSWORD_HASH_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "Boolean for service status."
    )
    
    object LOGIN_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "login"
      val description = "The login of the user whom password will be changed."
      val scalaType = typeOf[String]
    }
    object OLD_PASSWORD_HASH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "old_password_hash"
      val description = "The current password hash of the specified user."
      val scalaType = typeOf[String]
    }
    object NEW_PASSWORD_HASH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "new_password_hash"
      val description = "The new password hash of the specified user."
      val scalaType = typeOf[String]
    }
    
  }
  
  object AUTHENTICATE_METHOD extends JSONRPC2DefaultMethod {
    
    val name = "authenticate"
    val description = "Authenticate a user using its login and its password hash."
    val parameters = List(
      LOGIN_PARAM,
      PASSWORD_HASH_PARAM,
      RETURN_DB_PASSWORD_PARAM,
      PUBLIC_KEY_PARAM
    )
    val returns = JSONRPC2MethodResult(
      typeOf[AnyRef],
      "Returned value may be a boolean value or the encrypted database password (if 'return_db_password' is set to true)."
    )
    
    object LOGIN_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "login"
      val description = "The login of the user to authenticate."
      val scalaType = typeOf[String]
    }    
    object PASSWORD_HASH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "password_hash"
      val description = "The password hash of the specified user."
      val scalaType = typeOf[String]
    }    
    object RETURN_DB_PASSWORD_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "return_db_password"
      val description = "Specify if database password should be returned or not."
      val scalaType = typeOf[Boolean]
      optional = true
    }
    object PUBLIC_KEY_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "public_key"
      val description = "The public key to use for encryption of returned database password."
      val scalaType = typeOf[String]
      optional = true
    }
    
  }

}

