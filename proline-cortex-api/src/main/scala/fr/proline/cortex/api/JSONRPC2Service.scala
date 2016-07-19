package fr.proline.cortex.api

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
//import com.thetransactioncompany.jsonrpc2.server.RequestHandler
import com.thetransactioncompany.jsonrpc2.util._
import fr.proline.cortex.util.jsonrpc._

/**
 * The Trait JSONRPC2Service.
 * All Proline Web Services should implement this trait.
 *
 */
trait JSONRPC2Service extends IRemoteServiceIdentity { //extends RequestHandler 
  
  /** The methods handled by the Web Service. */
  def handledMethods: Seq[IJSONRPC2Method]
  
  lazy val handledRequests: Array[String] = handledMethods.map { _.name } toArray
  
  /**
   * Make a NamedParamsRetriever for the provided JSONRPC2Request.
   *
   * @param req The JSON-RPC request as a JSONRPC2Request object.
   * @return An instance of a NamedParamsRetriever.
   */
  def makeNamedParamsRetriever( req: JSONRPC2Request ): NamedParamsRetriever = {    
    // TODO: check params type with getParamsType() and throws Exception if needed
    new NamedParamsRetriever( req.getNamedParams() )
  }
  
  /**
   * Make a PositionalParamsRetriever for the provided JSONRPC2Request.
   *
   * @param req The JSON-RPC request as a JSONRPC2Request object.
   * @return An instance of a PositionalParamsRetriever.
   */
  def makePositionalParamsRetriever( req: JSONRPC2Request ): PositionalParamsRetriever = {
    // TODO: check params type with getParamsType() and throws Exception if needed
    new PositionalParamsRetriever( req.getPositionalParams() )
  }

  /**
   * Make service description.
   *
   * @param target The targeted URL.
   * @param transport The transport layer. It should be one of the JSONRPC2ServiceTransport enumeration.
   * @param envelope The body type of the service message. It should be one of the JSONRPC2ServiceEnvelope enumeration.
   * @return an instance of a JSONRPC2ServiceDescription
   */
  def makeServiceDescription(
    target: String,
    transport: JSONRPC2ServiceTransport.Value = JSONRPC2ServiceTransport.POST,
    envelope: JSONRPC2ServiceEnvelope.Value = JSONRPC2ServiceEnvelope.URL
  ): JSONRPC2ServiceDescription = {
    
    new JSONRPC2ServiceDescription(
      description = this.serviceDescription,
      target = target,
      transport = transport,
      envelope = envelope,
      methods = Some( this.handledMethods.map( _.toSerializableJSONRPC2Method ) ),
      jsonpCallbackParameter = None
    )
  }

}