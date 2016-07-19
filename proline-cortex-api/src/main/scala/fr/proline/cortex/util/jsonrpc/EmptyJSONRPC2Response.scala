package fr.proline.cortex.util.jsonrpc

import com.thetransactioncompany.jsonrpc2._

/**
 * The Class EmptyJSONRPC2Response whose purpose is to contain an empty String.
 *
 * @param result The response result.
 * @param id The request ID.
 */
class EmptyJSONRPC2Response(
  var result: java.lang.Object,
  id: java.lang.Object
) extends JSONRPC2Response( result, id ) {
  
  def this( id: java.lang.Object  ) = this(null, id)
  
  override def toString(): String = ""
  
  //override def setResult( result: java.lang.Object) = this.result = ""
  
}