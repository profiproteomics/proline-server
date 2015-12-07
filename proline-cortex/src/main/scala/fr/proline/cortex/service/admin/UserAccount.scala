package fr.proline.cortex.service.admin

import java.lang.Boolean
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.security.EncryptionManager
import fr.proline.admin.service.db.SetupProline
import fr.proline.admin.service.user.CreateUser
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.service.uds.UserAuthenticator
import fr.proline.core.service.uds.UserUpdator
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.IRemoteService
import fr.proline.jms.util.jsonrpc.JSONRPC2Utils
import fr.proline.jms.util.jsonrpc.ProfiJSONRPC2Response

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
class UserAccount extends IRemoteService with LazyLogging {

  final val UDS_AUTH_METHOD = "UDS_HASH"

  /* JMS Service identification */
  val serviceName = "proline/admin/UserAccount"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {
    require((req != null), "Req is null")
    logger.debug("service UserAccount");

    val requestId = req.getID
    val methodName = req.getMethod

    /* Method dispatch */
    methodName match {

      case "create" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)
        val result = doCreateUserAccount(paramsRetriever)
        return new ProfiJSONRPC2Response(result, requestId)
      }

      case "change_password" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)
        val result = doChangePassword(paramsRetriever)
        return new ProfiJSONRPC2Response(result, requestId)
      }

      case "authenticate" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)
        val result = doAuthenticate(paramsRetriever)
        return new ProfiJSONRPC2Response(result, requestId)
      }

      // Method name not supported
      case _ => return new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
    }
  }

  /* Define the doCreateUserAccount method : Create User in UDS DB throw Admin service*/
  def doCreateUserAccount(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "no parameter specified")
    
    val userLogin = paramsRetriever.getString("login")
    val userPassword = paramsRetriever.getString("password_hash")
    var result: Long = -1L
    val udsDbConnectionContext: DatabaseConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getIDataStoreConnectorFactory.getUdsDbConnector())
    try {
      val userCreator = new CreateUser(udsDbConnectionContext, userLogin, userPassword)
      userCreator.run()
      result = userCreator.userId
    } finally {
      try {
        udsDbConnectionContext.close()
      } catch {
        case exClose: Exception => logger.error("Error closing UDS Context", exClose)
      }
    }
    result.asInstanceOf[Object]
  }

  /* Define the doChangePassword method : change User in UDS DB using OMP service*/
  def doChangePassword(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "no parameter specified")
    
    logger.debug("doChangePassword");
    val userLogin = paramsRetriever.getString("login")
    val newPassword = paramsRetriever.getString("new_password_hash")
    val oldPassword = paramsRetriever.getString("old_password_hash")

    var result = false
    var udsDbConnectionContext: DatabaseConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getIDataStoreConnectorFactory.getUdsDbConnector())
    try {
      val userUpdator = new UserUpdator(udsDbConnectionContext, userLogin, newPassword, oldPassword)
      userUpdator.run()
      result = userUpdator.getUpdateResult
      if (!result)
        throw new RuntimeException(" Error updating password : " + userUpdator.getUpdatorResultMessage())

    } finally {
      try {
        udsDbConnectionContext.close()
      } catch {
        case exClose: Exception => logger.error("Error closing UDS Context", exClose)
      }
    }
    result.asInstanceOf[Object]
  }

  /* Define the doCreateUserAccount method : Create User in UDS DB throw Admin service*/
  def doAuthenticate(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "no parameter specified")
    
    val authMethod = if (SetupProline.getConfigParams.hasPath("authentication") == false) UDS_AUTH_METHOD
    else SetupProline.getConfigParams.getConfig("authentication").getString("method").toUpperCase()

    val needPgPaswd = if (paramsRetriever.hasParam("return_db_password")) paramsRetriever.getBoolean("return_db_password") else false

    if (needPgPaswd && !paramsRetriever.hasParam("public_key")) {
      throw new RuntimeException("Aunthentication failed : public key should be specified.")
    }

    if (!authMethod.equals(UDS_AUTH_METHOD)) {
      throw new RuntimeException("Aunthentication failed : Invalid authentication method specified.")
    }

    var serviceResult : Object = null

    val udsCtxt = new DatabaseConnectionContext(DbConnectionHelper.getIDataStoreConnectorFactory.getUdsDbConnector())
    try {

      val userLogin = paramsRetriever.getString("login")
      val userPassword = paramsRetriever.getString("password_hash")
      val authenticator = new UserAuthenticator(udsCtxt, userLogin, userPassword)
      authenticator.run()
      val result = authenticator.getAuthenticateResult
      if (!result) {
        throw new RuntimeException(" Authentication Error : " + authenticator.getAuthenticateResultMessage)
      } else if (needPgPaswd) {
        val pubKey = paramsRetriever.getString("public_key")
        if (SetupProline.getConfigParams.hasPath("auth-config") == false ) {
              throw new RuntimeException(" Authentication Error : No database password configured ")
        } else {
              val password = SetupProline.getConfigParams.getConfig("auth-config").getString("password")
              serviceResult = EncryptionManager.encrypt(password, pubKey)
        }                 
      } else {
        serviceResult = Boolean.TRUE
      }
    
    } finally {
      try {
        udsCtxt.close()
      } catch {
        case exClose: Exception => logger.error("Error closing UDS Context", exClose)
      }
    }
    serviceResult
  }
}