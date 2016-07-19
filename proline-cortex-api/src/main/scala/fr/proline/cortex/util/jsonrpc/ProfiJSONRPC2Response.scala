package fr.proline.cortex.util.jsonrpc

import scala.beans.BeanProperty
import com.fasterxml.jackson.annotation.JsonProperty
import com.thetransactioncompany.jsonrpc2._
import fr.profi.util.serialization.ProfiJson
import fr.profi.util.StringUtils

object ProfiJSONRPC2Response {
  
  def apply( error: JSONRPC2Error, id: java.lang.Object ): ProfiJSONRPC2Response  = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, id)
    resp.setError( error )
    resp
  }
  
  def forError( message: String, code: Int = -1, data: java.lang.Object = null )( requestId: java.lang.Object ): ProfiJSONRPC2Response = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( new JSONRPC2Error(code, message, data) )
    resp
  }
  
  def forThrowable( t: Throwable, code: Int = -1 )( requestId: java.lang.Object ): ProfiJSONRPC2Response  = {
    val messageBuilder = new StringBuilder()
    messageBuilder.append(t)

    val exMessage = t.getMessage
    if (!StringUtils.isEmpty(exMessage)) {
      messageBuilder.append(StringUtils.LINE_SEPARATOR)
      messageBuilder.append(exMessage)
    }
    
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( new JSONRPC2Error(code, messageBuilder.toString, t.getStackTraceString) )
    
    resp
  }
  
  def forInternalError( requestId: java.lang.Object ): ProfiJSONRPC2Response  = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( JSONRPC2Error.INTERNAL_ERROR )
    resp
  }
  
  def forInvalidParams( requestId: java.lang.Object ): ProfiJSONRPC2Response  = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( JSONRPC2Error.INVALID_PARAMS )
    resp
  }
  
  def forInvalidRequest( requestId: java.lang.Object ): ProfiJSONRPC2Response  = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( JSONRPC2Error.INVALID_REQUEST )
    resp
  }
  
  def forMethodNotFound( requestId: java.lang.Object ): ProfiJSONRPC2Response  = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( JSONRPC2Error.METHOD_NOT_FOUND )
    resp
  }
  
  def forParseError( requestId: java.lang.Object ): ProfiJSONRPC2Response  = {
    val resp = ProfiJSONRPC2Response(null: java.lang.Object, requestId)
    resp.setError( JSONRPC2Error.PARSE_ERROR )
    resp
  }

}

/**
 * The Class ProfiJSONRPC2Response which uses Jackson for JSON response generation.
 *
 * @param result The response result.
 * @param id The request ID.
 */
case class ProfiJSONRPC2Response(
  var result: java.lang.Object,
  val id: java.lang.Object
) extends JSONRPC2Response(result, id) {
  
  protected def this( error: JSONRPC2Error, id: java.lang.Object ) = this( null: java.lang.Object, id )
  
  protected case class SerializableJSONRPC2Response(
    id: String,
    result: Any,
    error: Option[SerializableJSONRPC2Error] = None,
    jsonrpc: String = "2.0" // JSON RPC version
  )
  
  protected case class SerializableJSONRPC2Error(
    code: Int,
    message: String,
    data: Any
  )

  def this(id: java.lang.Object) = this(null, id)

  override def toString(): String = {    
    val resp = new SerializableJSONRPC2Response(
      this.id.asInstanceOf[String],
      this.result,
      Option( this.getError() ).map( e => SerializableJSONRPC2Error(e.getCode(), e.getMessage(), e.getData() ))
    )
    ProfiJson.serialize(resp)
  }
  
  override def getResult(): java.lang.Object = this.result

  override def setResult(result: java.lang.Object) = {
    this.result = result
  }

}