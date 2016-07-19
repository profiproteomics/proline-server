package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe.typeOf
import fr.profi.util.lang.EnhancedEnum
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.cortex.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.cortex.util.jsonrpc.JSONRPC2MethodResult

trait IUserAccountService extends IAdminService {
  
  /* JMS Service identification */
  val serviceName = "UserAccount"
  this.serviceDescription = Some("Filters and validates a Result Set for a given set of rules.")
  
}

/**
 * JMS Service to manage UserAccount. 
 * 
 * Create method to add a new user in Proline Suite
 *   Input params :
 *      "login" : new User login
 *      "password_hash" : encrypted user password
 *  
 *   Output params : 
 *      new user Id. 
 *      
 * Change Password method to update existing user password
 *   Input params :
 *      "login" : existing User login
 *      "old_password_hash" : actual encrypted user password
 *      "new_password_hash" : new encrypted user password
 *  
 *   Output params : 
 *      Boolean for service status. 
 *  
 * Authenticate method 
 *   Input params :
 *      "login" : existing User login
 *      "password_hash" : encrypted user password
 *      "need_db_pwd" : specify if database password should be retrieve to caller 
 *      "public_key" : The public key to use for encryption of returned database password
 *  
 *   Output params : 
 *      Boolean for service status. 
 */
object UserAccountService extends IUserAccountService with IDefaultServiceVersion {
  
  // List the handled methods
  val handledMethods = List(Create,ChangePassword,Authenticate)
  
  val UDS_AUTH_METHOD = "UDS_HASH"
  
  private object UserAccountMethodName extends EnhancedEnum {
    val create = Value("create")
    val update = Value("update")
    val change_password = Value("change_password")
    val authenticate = Value("authenticate")
  }
  
  // Description of the Create service method
  object Create extends JSONRPC2DefaultMethod { // Emulate an enumeration of objects
    
    // Method description
    val name = UserAccountMethodName.create.toString
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
  
  object ChangePassword extends JSONRPC2DefaultMethod {
    
    val name = "change_password"
    val description = "Change the password of the specified user."
    val parameters = List(LOGIN_PARAM,OLD_PASSWORD_HASH_PARAM,NEW_PASSWORD_HASH_PARAM)
    val returns = JSONRPC2MethodResult(typeOf[Boolean])
    
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
  
  object Authenticate extends JSONRPC2DefaultMethod {
    
    val name = "authenticate"
    val description = "Authenticate a user using its login and its password hash."
    val parameters = List(
      LOGIN_PARAM,
      PASSWORD_HASH_PARAM,
      RETURN_DB_PASSWORD_PARAM,
      PUBLIC_KEY_PARAM
    )
    val returns = JSONRPC2MethodResult(typeOf[AnyRef])
    
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