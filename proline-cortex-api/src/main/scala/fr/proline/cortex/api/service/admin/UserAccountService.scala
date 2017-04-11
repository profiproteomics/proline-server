package fr.proline.cortex.api.service.admin

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

object UserAccountService extends IUserAccountService

trait IUserAccountService extends IAdminService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "UserAccount"
  this.serviceDescription = Some("Service providing methods for user accounts mangement. ")
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(CREATE_METHOD,CHANGE_PASSWORD_METHOD,RESET_PASSWORD_METHOD,AUTHENTICATE_METHOD,MODIFY_USER_GROUP_METHOD)
  
  val UDS_AUTH_METHOD = "UDS_HASH"
  
  // Description of the Create service method
  object CREATE_METHOD extends JSONRPC2DefaultMethod { // Emulate an enumeration of objects
    
    // Method description
    val name = "create"
    val description = "Creates a new user account."
    
    // Configure method interface
    val parameters = List(LOGIN_PARAM,PASSWORD_HASH_PARAM,IS_USER_GROUP_PARAM)
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
    
    object IS_USER_GROUP_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "is_user_group"
      val description = "If set to true, the user belongs to the user group, otherwise to the admin group"
      val scalaType = typeOf[Boolean]
      optional = true
    }
    
  }
  
  object CHANGE_PASSWORD_METHOD extends JSONRPC2DefaultMethod {
    
    // Method description
    val name = "change_password"
    val description = "Change the password of the specified user."
    
    // Configure method interface
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
  
  object RESET_PASSWORD_METHOD extends JSONRPC2DefaultMethod {
    
    // Method description
    val name = "reset_password"
    val description = "Reset the password of the specified user."
    
    // Configure method interface
    val parameters = List(LOGIN_ID_PARAM, NEW_PASSWORD_HASH_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "Boolean for service status."
    )
    
    object LOGIN_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "login_id"
      val description = "The login id of the user whom password will be reset."
      val scalaType = typeOf[Long]
    }
    object NEW_PASSWORD_HASH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "new_password_hash"
      val description = "The new password hash of the specified user."
      val scalaType = typeOf[String]
    }
    
  }
  
  object MODIFY_USER_GROUP_METHOD extends JSONRPC2DefaultMethod {
    
    // Method description
    val name = "modify_user_group"
    val description = "Modify the user group of the specified user."
    
    // Configure method interface
    val parameters = List(LOGIN_ID_PARAM, IS_USER_GROUP_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "Boolean for service status."
    )
    
    object LOGIN_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "login_id"
      val description = "The login id of the user whom password will be reset."
      val scalaType = typeOf[Long]
    }
    object IS_USER_GROUP_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "is_user_group"
      val description = "True is the new user group of the user is USER."
      val scalaType = typeOf[Boolean]
    }
    
  }
  
  object AUTHENTICATE_METHOD extends JSONRPC2DefaultMethod {
    
    // Method description
    val name = "authenticate"
    val description = "Authenticate a user using its login and its password hash."
    
    // Configure method interface
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

