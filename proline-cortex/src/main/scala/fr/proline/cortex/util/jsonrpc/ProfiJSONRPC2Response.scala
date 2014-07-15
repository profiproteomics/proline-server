package fr.proline.cortex.util.jsonrpc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Response

import fr.profi.util.serialization.ProfiJson

/**
 * ProfiJSONRPC2Response uses ProfiJson for JSON response generation.
 *
 * WARN Only use this class for serializtion of Profi "result" objects.
 * For standard or error response use base JSONRPC2Response class.
 *
 * @param profiResult The Response result (must be serializable via ProfiJson).
 * @param jerksonId The JSON-RPC Request Id (must be serializable via ProfiJson).
 */
class ProfiJSONRPC2Response(private var profiResult: java.lang.Object,
                            profiId: java.lang.Object) extends JSONRPC2Response(profiResult, profiId) {

  def this(jerksonId: java.lang.Object) = this(null, jerksonId)

  override def setResult(pResult: java.lang.Object) = {
    profiResult = pResult
  }

  override def getResult(): java.lang.Object = {
    profiResult
  }

  /* JMS ServiceRunner uses toJSONString() */
  override def toJSONString(): String = {
    ProfiJson.serialize(new Response(getResult, getID))
  }

}

case class Response(result: java.lang.Object, id: java.lang.Object, jsonrpc: String = "2.0")
