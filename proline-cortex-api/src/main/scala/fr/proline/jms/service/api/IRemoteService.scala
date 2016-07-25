package fr.proline.jms.service.api

import javax.jms.BytesMessage
import com.thetransactioncompany.jsonrpc2._
import fr.proline.cortex.util.jsonrpc._

trait IRemoteServiceIdentity {
  
  /** The namesapce of this service like "proline/dps/msi" */
  val serviceNamespace: String
  
  /** The name of this service like "ImportResultFiles" */
  // TODO: rename to serviceName when serviceName has been renamed to servicePath
  val serviceLabel: String
  
  /** The description of this service */
  var serviceDescription: Option[String] = None
  
  /**
   * Version of this service implementation.
   */
  val serviceVersion: String

  /**
   * True if this service implementation version is the default one. False by default.
   * <p> For a given serviceName, it must exist only one default version.
   */
  val isDefaultVersion: Boolean = false
  
  /**
   * Fully qualified service path like "proline/dps/msi/ImportResultFiles".
   */
  // TODO: rename to servicePath or serviceFullName
  lazy val serviceName = serviceNamespace + '/' + serviceLabel //+ "@" + wsVersion
  
  /** The methods handled by the Web Service. */
  def methodDefinitions: Seq[IJSONRPC2Method]

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
      methods = Some( this.methodDefinitions.map( _.toSerializableJSONRPC2Method ) ),
      jsonpCallbackParameter = None
    )
  }
}

trait IDefaultServiceVersion extends IRemoteServiceIdentity {
  val serviceVersion = RemoteServiceIdentity.defaultVersion
  override val isDefaultVersion = true
}

/** The Object WSIdentity contains static methods useful for Web Service identification. */
object RemoteServiceIdentity {
  val defaultVersion = "1.0"
}

trait IRemoteJsonRPC2Service extends IRemoteServiceIdentity {
  
  // Define the "process" method which must be implemented by child classes
  // Note: exceptions are caught by the ServiceRunner to return an error properly
  def process(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response

}

trait IRemoteProcessingService extends IRemoteJsonRPC2Service {
  
  // List of parameters used by the processing service
  val serviceParams: Seq[IJSONRPC2MethodParameter]
  val serviceResult: JSONRPC2MethodResult
  
  protected val PROCESS_METHOD_NAME = "process"
  
  // Specify the method definitions of the handled requests
  lazy val methodDefinitions = List(
    JSONRPC2Method(
      name = PROCESS_METHOD_NAME,
      description = "Runs the service using specified parameters",
      parameters = serviceParams.map(_.toSerializableJSONRPC2Parameter()),
      returns = JSONRPC2MethodResult(scala.reflect.runtime.universe.typeOf[Int])
    )
  )
  
  def process(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require(jsonRequest != null, "Req is null")
    BuildJSONRPC2Response.forMethodNotFound(jsonRequest.getID)
  }
  
}

trait IRemoteBytesMsgService extends IRemoteServiceIdentity {

  // Exceptions are caught by the ServiceRunner to return Error
  def process(bytesMessage: BytesMessage, jmsMessageContext: Map[String, Any] = Map()): JSONRPC2Response = {
    require(bytesMessage != null, "JMS BytesMessage is null")
    BuildJSONRPC2Response.forMethodNotFound(bytesMessage.getJMSMessageID)
  }
}

trait ISingleThreadedService extends IRemoteServiceIdentity {
  
  /**
   * Unique name identifying a Thread which could be share between multiple services  
   */
  val singleThreadIdent : String 

}
