package fr.proline.cortex.util.jsonrpc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Response

import fr.profi.util.serialization.ProfiJson

/**
 * JerksonJSONRPC2Response uses Jerkson for JSON response generation.
 *
 * WARN Only use this class for serializtion of Proline "result" objects.
 * For standard or error response use base JSONRPC2Response class.
 *
 * @param result The response result (must be Jerkson serializable).
 * @param id The request ID (must be Jerkson serializable).
 */
/* Implementation note : result and id fields are shadowing base JSONRPC2Response fields ! Use getters */
class JerksonJSONRPC2Response(private var result: java.lang.Object,
                              id: java.lang.Object) extends JSONRPC2Response(result, id) {

  def this(id: java.lang.Object) = this(null, id)

  override def setResult(pResult: java.lang.Object) = {
    result = pResult
  }

  override def getResult(): java.lang.Object = {
    result
  }

  /* JMS ServiceRunner uses toJSONString() */
  override def toJSONString(): String = {
    ProfiJson.serialize(new Response(getResult, getID))
  }

  /* Inner class */
  case class Response(result: java.lang.Object, id: java.lang.Object, jsonrpc: String = "2.0")

}
