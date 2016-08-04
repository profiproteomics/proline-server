package fr.profi.util.jsonrpc

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever

object JSONRPC2Utils {

  /***
   * Return NamedParamsRetriever specified in JSONRPC2Request or null if none was specified
   */
  def buildParamsRetriever(request: JSONRPC2Request): NamedParamsRetriever = {
    require(request != null, "request is null")

    val namedParams = request.getNamedParams

    if(namedParams != null)
      new NamedParamsRetriever(namedParams)
    else
      null
  }
  
  def buildStringMethodParameter(name : String, description: String, optional: Boolean, defaultVal :  Option[String] ) : JSONRPC2MethodParameter = {
    new JSONRPC2MethodParameter(name, description, typeOf[String], optional, defaultVal)
  }
  
  def buildArrayLongMethodParameter(name : String, description: String, optional: Boolean, defaultVal :  Option[String] ) : JSONRPC2MethodParameter = {
    new JSONRPC2MethodParameter(name, description, typeOf[Array[Long]], optional, defaultVal)
  }
    
  def buildBooleanMethodParameter(name : String, description: String, optional: Boolean, defaultVal :  Option[String] ) : JSONRPC2MethodParameter = {
    new JSONRPC2MethodParameter(name, description, typeOf[Boolean], optional, defaultVal)
  }
  
  def buildBooleanMethodReturn(description: String ) : JSONRPC2MethodResult = {
    new JSONRPC2MethodResult( typeOf[Boolean], description)
  }

}
