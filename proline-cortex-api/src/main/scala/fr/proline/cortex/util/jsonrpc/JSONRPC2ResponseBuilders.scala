package fr.proline.cortex.util.jsonrpc

import com.thetransactioncompany.jsonrpc2._
import fr.profi.util.StringUtils

object BuildJSONRPC2Response {
  
  def apply( result: java.lang.Object, id: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(result, id)
  }
  
  def apply( id: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(id)
  }
  
  def forError(error: JSONRPC2Error)( requestId: java.lang.Object ): JSONRPC2Response  = {
    val resp = new JSONRPC2Response(requestId)
    resp.setError(error)
    resp
  }
  
  def forError(message: String, code: Int = -1, data: java.lang.Object = null )( requestId: java.lang.Object ): JSONRPC2Response = {
    val error = new JSONRPC2Error(code, message, data)
    new JSONRPC2Response(error, requestId)
  }
  
  def forThrowable(t: Throwable, code: Int = -1 )( requestId: java.lang.Object ): JSONRPC2Response  = {
    val messageBuilder = new StringBuilder()
    messageBuilder.append(t)

    val exMessage = t.getMessage
    if (!StringUtils.isEmpty(exMessage)) {
      messageBuilder.append(StringUtils.LINE_SEPARATOR)
      messageBuilder.append(exMessage)
    }
    
    val error = new JSONRPC2Error(code, messageBuilder.toString, t.getStackTraceString)
    new JSONRPC2Response(error, requestId)
  }
  
  def forInternalError( requestId: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(JSONRPC2Error.INTERNAL_ERROR, requestId)
  }
  
  def forInvalidParams( requestId: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(JSONRPC2Error.INVALID_PARAMS, requestId)
  }
  
  def forInvalidRequest( requestId: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(JSONRPC2Error.INVALID_REQUEST, requestId)
  }
  
  def forMethodNotFound( requestId: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
  }
  
  def forParseError( requestId: java.lang.Object ): JSONRPC2Response  = {
    new JSONRPC2Response(JSONRPC2Error.PARSE_ERROR, requestId)
  }

}

/**
 * An interface describing the methods required for JSON-RPC response creation.
 *
 */
trait IJSONResponseBuilder {
  
  /** The status of the response building process. */
  protected var _isResponseBuilt = false

  /** The request ID. */
  val requestId: java.lang.Object
  
  /**
   * Append to response.
   *
   * @param append add a partial response to the final response.
   * @return the IJSONResponseBuilder implementation.
   */
  def append( partialResponse: java.lang.Object ): IJSONResponseBuilder
  
  /** Terminates the the response building process.
   *  
   *  @return the JSONRPC2Response which has been built.
   */
  def finish(): JSONRPC2Response = {
    _isResponseBuilt = true
    this.getResponse
  }
  
  /** Getter for the _isResponseBuilt private member
   *  
   *  @return a boolean value indicating if the response has been built.
   */
  def isResponseBuilt(): Boolean = _isResponseBuilt
  
  /**
   * Gets the response.
   *
   * @return the built response
   */
  def getResponse(): JSONRPC2Response
  
  /**
   * Gets the response result.
   *
   * @param a java.lang.Object containing the response result.
   * @return the IJSONResponseBuilder implementation.
   */
  def setResponse( response: java.lang.Object ): IJSONResponseBuilder = {
    this.append( response )
    this.finish()
    this
  }
  
}


/**
 * The Class SerializedResponseBuilder is an implementation of IJSONResponseBuilder using Jackson for response generation.
 * 
 * @param requestId The JSON-RPC request ID.
 * @deprecated: use ProfiJSONRPC2Response directly
 */
/*class SerializedResponseBuilder( val requestId: java.lang.Object ) extends IJSONResponseBuilder {
  
  var _response: JSONRPC2Response = null
  
  def append( partialResponse: java.lang.Object ): IJSONResponseBuilder = {
    _response = ProfiJSONRPC2Response(partialResponse, requestId )
    this
  }
  
  def getResponse(): JSONRPC2Response = _response

}*/

/**
 * The Class StreamedResponseBuilder is an implementation of IJSONResponseBuilder
 * which appends partial responses directly to the output stream of the servlet.
 * 
 * @param requestId The JSON-RPC request ID.
 * @param os The ServletOutputStream used for response printing.
 */
// TODO: create a response builder dedicated to JMS streams
/*class StreamedResponseBuilder( val requestId: java.lang.Object, os: ServletOutputStream ) extends IJSONResponseBuilder {
  
  // Fake an empty response
  val _emptyResponse: JSONRPC2Response = new EmptyJSONRPC2Response( "", requestId )
  
  // Append JSON RPC header
  // Note: the response result will be contained in an array
  os.print("{\"id\":"+ requestId +",\"jsonrpc\":\"2.0\",\"result\":[")
  
  def append( partialResponse: java.lang.Object ): IJSONResponseBuilder = {
    os.print(partialResponse.asInstanceOf[String])
    this
  }
  
  override def finish(): JSONRPC2Response = {
    os.print("]}")
    os.flush()
    _isResponseBuilt = true
    _emptyResponse
  }
  
  /** Implementation of the getResponse method.
   *  @return a JSONRPC2Response without any result
   *  @note the response result can't be retrieved because it is sent directly to the output stream.
   *  An EmptyJSONRPC2Response is thus returned instead.
   */
  def getResponse(): JSONRPC2Response = _emptyResponse

}*/
