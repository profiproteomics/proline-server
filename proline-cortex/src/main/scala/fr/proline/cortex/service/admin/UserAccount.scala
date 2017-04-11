package fr.proline.cortex.service.admin

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.jsonrpc.BuildJSONRPC2Response
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.profi.util.jsonrpc.ProfiJSONRPC2Response
import fr.profi.util.security.EncryptionManager
import fr.proline.admin.service.db.SetupProline
import fr.proline.admin.service.user.CreateUser
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.service.uds.UserAuthenticator
import fr.proline.core.service.uds.UserUpdator
import fr.proline.admin.service.user.ResetPassword
import fr.proline.admin.service.user.ModifyUserGroup
import fr.proline.cortex.api.service.admin.IUserAccountService
import fr.proline.cortex.api.service.admin.UserAccountService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.IRemoteJsonRPC2Service
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.cortex.service.SingleThreadIdentifierType

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
 *      "return_db_password" : specify if database password should be retrieve to caller 
 *      "public_key" : The public key to use for encryption of returned database password
 *  
 *   Output params : 
 *      Boolean for service status. 
 */
class UserAccount extends IUserAccountService with IRemoteJsonRPC2Service  with ISingleThreadedService  with LazyLogging {
  
  /* JMS Service identification */
  val singleThreadIdent= SingleThreadIdentifierType.SHORT_SERVICES_SINGLETHREAD_IDENT.toString()
 
  
  override def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require(jsonRequest != null, "jsonRequest is null")
    
    val requestId = jsonRequest.getID()
    val methodName = jsonRequest.getMethod()

    /* Method dispatch */
    methodName match {

      case CREATE_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doCreateUserAccount(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }

      case CHANGE_PASSWORD_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doChangePassword(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }
      
      case RESET_PASSWORD_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doResetPassword(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }

      case MODIFY_USER_GROUP_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doModifyUserGroup(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }
      
      case AUTHENTICATE_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doAuthenticate(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }
      // Method name not supported
      case _ => return BuildJSONRPC2Response.forMethodNotFound(requestId)
    }

    return BuildJSONRPC2Response.forMethodNotFound(requestId)
  }
  
  /* Define the doCreateUserAccount method : Create User in UDS DB throw Admin service*/
  def doCreateUserAccount(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")
    
    val userLogin = paramsRetriever.getString(CREATE_METHOD.LOGIN_PARAM)
    val userPassword = paramsRetriever.getString(CREATE_METHOD.PASSWORD_HASH_PARAM)
    val isGroupUser = paramsRetriever.getOptBoolean(CREATE_METHOD.IS_USER_GROUP_PARAM, true)
    
    var result: Long = -1L
    val udsDbConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory.getUdsDbConnector())
    
    try {
      val userCreator = new CreateUser(udsDbConnectionContext, userLogin, userPassword, isGroupUser)
      userCreator.run()
      result = userCreator.userId
    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbConnectionContext)
    }
    
    result.asInstanceOf[Object]
  }
  
  
  /* Define the doChangePassword method: change User in UDS DB using OMP service */
  def doChangePassword(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")
    
    val userLogin = paramsRetriever.getString(CHANGE_PASSWORD_METHOD.LOGIN_PARAM)
    val newPassword = paramsRetriever.getString(CHANGE_PASSWORD_METHOD.NEW_PASSWORD_HASH_PARAM)
    val oldPassword = paramsRetriever.getString(CHANGE_PASSWORD_METHOD.OLD_PASSWORD_HASH_PARAM)

    var result = false
    val udsDbConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory.getUdsDbConnector())
    try {
      val userUpdator = new UserUpdator(udsDbConnectionContext, userLogin, newPassword, oldPassword)
      userUpdator.run()
      result = userUpdator.getUpdateResult
      if (!result)
        throw new Exception("Error while updating password: " + userUpdator.getUpdatorResultMessage())

    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbConnectionContext)
    }
    
    result.asInstanceOf[Object]
  }
  
  /* Define the doResetPassword method: Reset User Password in UDS DB throw Admin service */
  def doResetPassword(paramsRetriever: NamedParamsRetriever): Object =  {
    require(paramsRetriever != null, "no parameter specified")
    
    val userId = paramsRetriever.getLong(RESET_PASSWORD_METHOD.LOGIN_ID_PARAM)
    val newPassword = paramsRetriever.getString(RESET_PASSWORD_METHOD.NEW_PASSWORD_HASH_PARAM)

    var result = true
    val udsDbConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory.getUdsDbConnector())
    try {
      val resetPassword = new ResetPassword(udsDbConnectionContext, userId, newPassword)
      resetPassword.run()


    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbConnectionContext)
    }
    
    
    result.asInstanceOf[Object]

  }
  
    /* Define the doModifyUserGroup method: Modify the User Group of a User in UDS DB throw Admin service */
  def doModifyUserGroup(paramsRetriever: NamedParamsRetriever): Object =  {
    require(paramsRetriever != null, "no parameter specified")
    
    val userId = paramsRetriever.getLong(MODIFY_USER_GROUP_METHOD.LOGIN_ID_PARAM)
    val isUserGroup = paramsRetriever.getBoolean(MODIFY_USER_GROUP_METHOD.IS_USER_GROUP_PARAM)

    var result = true
    val udsDbConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory.getUdsDbConnector())
    try {
      val modifyUserGroup = new ModifyUserGroup(udsDbConnectionContext, userId, isUserGroup)
      modifyUserGroup.run()


    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbConnectionContext)
    }
    
    
    result.asInstanceOf[Object]

  }
  
  
  
  /* Define the doCreateUserAccount method : Create User in UDS DB throw Admin service*/
  def doAuthenticate(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")
    
    val AUTHENTICATION_CONFIG_KEY = "authentication"
    val AUTHENTICATION_METHOD_CONFIG_KEY = "method"
    
    val authMethod = if (SetupProline.getConfigParams.hasPath(AUTHENTICATION_CONFIG_KEY) == false) UserAccountService.UDS_AUTH_METHOD
    else SetupProline.getConfigParams.getConfig(AUTHENTICATION_CONFIG_KEY).getString(AUTHENTICATION_METHOD_CONFIG_KEY).toUpperCase()

    val needPgPwd = paramsRetriever.getOptBoolean(AUTHENTICATE_METHOD.RETURN_DB_PASSWORD_PARAM, false)

    if (needPgPwd) {
      require(
        paramsRetriever.hasParam(AUTHENTICATE_METHOD.PUBLIC_KEY_PARAM),
        "Authentication failed: public key should be specified !"
      )
    }

    require(
      authMethod == UserAccountService.UDS_AUTH_METHOD,
      "Authentication failed: provided authentication method is invalid !"
    )

    var serviceResult: Object = null

    val udsCtx = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory.getUdsDbConnector())
    try {

      val userLogin = paramsRetriever.getString(AUTHENTICATE_METHOD.LOGIN_PARAM)
      val userPassword = paramsRetriever.getString(AUTHENTICATE_METHOD.PASSWORD_HASH_PARAM)
      val authenticator = new UserAuthenticator(udsCtx, userLogin, userPassword)
      authenticator.run()
      
      val result = authenticator.getAuthenticateResult
      
      if (!result) {
        throw new Exception("Authentication Error: " + authenticator.getAuthenticateResultMessage)
      }
      else if (!needPgPwd) serviceResult = java.lang.Boolean.TRUE
      else {
        val pubKey = paramsRetriever.getString(AUTHENTICATE_METHOD.PUBLIC_KEY_PARAM)
        
        if (SetupProline.getConfigParams.hasPath("auth-config") == false ) {
          throw new Exception("Authentication Error: no database password in configuration file !")
        } else {
          val password = SetupProline.getConfigParams.getConfig("auth-config").getString("password")
          serviceResult = EncryptionManager.encrypt(password, pubKey)
        }
      }
    
    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsCtx)
    }
    
    serviceResult
  }
  

}