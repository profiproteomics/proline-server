package fr.proline.jms.service.api

import com.thetransactioncompany.jsonrpc2.{JSONRPC2Request, JSONRPC2Response}
import fr.profi.util.jsonrpc._
import fr.profi.util.jsonrpc.JSONRPC2ServiceTransport.enumToString
import javax.jms.{BytesMessage, Message, Session}
 

trait IRemoteServiceIdentity {
  
  /** The namespace of this service like "proline/dps/msi" */
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
   * True if this service need to be linked to execution node. False by default.
   * This occurs when the service is related to resources.
   */
  val isNodeSpecific: Boolean = false
  
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
  
  val PROCESS_METHOD_NAME = "process"
}

/**
 * Interface for services
 *  - consuming of TextMessage
 *  - returning an object type
 */
trait IRemoteJsonRPC2Service extends IRemoteServiceIdentity {
  
  // Define the "runSevice" method which must be implemented by child classes
  /**
   * Implementing class should implement this runService method if the returned object in
   * a JSONRPC2Response Object which will be integrated to replied Message
   * Note: exceptions are caught by the ServiceRunner to return an error properly
   * 
   */
  def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response
  
}


/**
 * Interface for services
 *  - consuming of TextMessage
 *  - returning a complete jms Message Object which will be replied as it is
 */
trait IRemoteCompleteJsonRPC2Service extends IRemoteServiceIdentity {

  /**
   * Implementing class should implement this runService method :  the returned object is
   * a complete jms Message Object which will be replied as it is
   * Note: exceptions are caught by the ServiceRunner to return an error properly
   * 
   */
  def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any],session: Session): Message
  
 
}


trait IRemoteBytesMsgService extends IRemoteServiceIdentity {

  // Exceptions are caught by the ServiceRunner to return Error
  def runService(bytesMessage: BytesMessage, jmsMessageContext: Map[String, Any] = Map()): JSONRPC2Response = {
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
