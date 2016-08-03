package fr.profi.util.jsonrpc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request

import fr.profi.util.serialization.ProfiJson

object ProfiJSONRPC2Request {
  
  def apply( method: String, namedParams: java.util.Map[String,Object], requestId: Object ): ProfiJSONRPC2Request  = {
    new ProfiJSONRPC2Request(method, namedParams, requestId)
  }
  
  def apply( method: String, requestId: Object ): ProfiJSONRPC2Request  = {
    new ProfiJSONRPC2Request(method, requestId)
  }
  
}

/**
 * ProfiJSONRPC2Request uses ProfiJson for JSON response generation.
 *
 * WARN: only use this class for serialization of ProFI "result" objects.
 * For standard or error response use base JSONRPC2Response class.
 * The Response result and id must be serializable via ProfiJson.
 *
 */
class ProfiJSONRPC2Request private() extends JSONRPC2Request(null,null) {
  
  def this( method: String, namedParams: java.util.Map[String,Object], id: Object ) = {
    this()
    this.setMethod(method)
    this.setNamedParams(namedParams)
    this.setID(id)
  }
  
  def this( method: String, id: Object ) = {
    this()
    this.setMethod(method)
    this.setID(id)
  }
  
  /* Override toString instead of toJSONString to be sure that both methods return the same thing */
  override def toString(): String = {
    
    new SerializableJSONRPC2Request(
      this.getID(),
      this.getMethod(),
      this.getParams
    ).toJSONString()
    
  }

}

case class SerializableJSONRPC2Request(
  id: Any,
  method: String,
  params: Object,
  jsonrpc: String = "2.0" // JSON-RPC version
) {
  def toJSONString() = {
    ProfiJson.serialize(this)
  }
}
