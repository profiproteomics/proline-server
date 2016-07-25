package fr.proline.cortex.util.jsonrpc

import scala.reflect.runtime.universe._
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.annotation.JsonProperty
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.lang.EnhancedEnum
import fr.proline.cortex.util.reflect._
import fr.proline.cortex.util.json._

/** JSONRPC2 Service Description
 * 
 *  Fore more information see:
 *  https://github.com/dojo/docs/blob/master/dojox/rpc/smd.rst
 *  http://stackoverflow.com/questions/2636316/json-rpc-and-json-rpc-service-discovery-specifications
 *
 * @constructor creates a new Service Description
 * @param target This should indicate what URL (or IP address in the case of TCP/IP transport) to use for the method call requests.
 * @param id This is a unique identifier for the service. When possible, the id should represent the URL from which service descriptor can be accessed (relative URLs are acceptable). This property SHOULD be included.
 * @param description This a description of the service. This property SHOULD be included.
 * @param contentType This is the expected content type of the content returned by a service. Any valid MIME type is acceptable. This property defaults to "application/json".
 * @param transport The transport property defines the transport mechanism to be used to deliver service calls to servers.
 * @param envelope An envelope defines how a service message string is created from the provided parameters.
 * @param parameters This indicates what parameters may be supplied for the service calls.
 * @param additionalParameters If additionalParameters is false, only parameters specified on a method will be allowed.
 * @param services This should be an Object value where each property in the Object represents one of the available services. The property name represents the name of the service, and the value is the service description. This property MUST be included.
 * @param methods This should be an Array of the available methods.
 * @param jsonpCallbackParameter This indicates the parameter named to be used to indicate the callback prepended on the JSON response. This defaults to "callback".
 * @param SMDVersion This is a string that indicates the version level of the SMD being used. This specification is at version level "2.0". This property SHOULD be included.
*/
case class JSONRPC2ServiceDescription (
  var target: String,
  var id: Option[String] = None,
  var description: Option[String] = None,
  var contentType: String = "application/json",
  var transport: String = JSONRPC2ServiceTransport.POST.toString(),
  var envelope: String = JSONRPC2ServiceEnvelope.URL.toString(),
  var parameters: Option[Seq[IJSONRPC2MethodParameter]] = None,
  var additionalParameters: Option[Boolean] = None,
  var services: Option[Array[JSONRPC2ServiceDescription]] = None,
  var methods: Option[Seq[IJSONRPC2Method]] = None,
  var jsonpCallbackParameter: Option[String] = Some("callback"),
  var SMDVersion: Option[String] = Some("2.0")
)

/** JSONRPC2 Method Description
 *  Fore more information see: https://github.com/dojo/docs/blob/master/dojox/rpc/smd.rst
 *
 * @constructor creates a new Method Description
 * @param name This indicates the name of the method call. This can be used by libraries for referring to methods by name. This should be unique for each method and therefore should not be inherited from the root level.
 * @param parameters This indicates what parameters may be supplied for the service calls.
 * @param additionalParameters If additionalParameters is false, only parameters specified on a method will be allowed.
 * @param returns This indicates the expected type of value returned from the method call.
*/
case class JSONRPC2Method (
  val name: String,
  val description: String,
  var parameters: Seq[IJSONRPC2MethodParameter],
  var returns: JSONRPC2MethodResult,
  var additionalParameters: Boolean = false
) extends IJSONRPC2Method

trait JSONRPC2DefaultMethod extends IJSONRPC2Method {
  def name: String
  def description: String
  def parameters: Seq[IJSONRPC2MethodParameter]
  def returns: JSONRPC2MethodResult
  def additionalParameters: Boolean = false
}

trait IJSONRPC2Method {
  def name: String
  def description: String
  def parameters: Seq[IJSONRPC2MethodParameter]
  def returns: JSONRPC2MethodResult
  def additionalParameters: Boolean
  
  def toSerializableJSONRPC2Method() = {
    
    this match {
      case serializableMethod: JSONRPC2Method => serializableMethod
      case _ => {
        val serializableParameters = if( this.parameters == null ) null
        else this.parameters.map( _.toSerializableJSONRPC2Parameter() )
        
        JSONRPC2Method(
          name = name,
          description = description,
          parameters = serializableParameters,
          returns = returns,
          additionalParameters = additionalParameters
        )
      }
    }    

  }
}

case class JSONRPC2MethodParameter(
  val name: String,
  val description: String,
  @transient val scalaType: Type,
  var optional: Boolean = false,
  var default: Option[String] = None  
) extends IJSONRPC2MethodParameter

trait JSONRPC2DefaultMethodParameter extends IJSONRPC2MethodParameter {
  def name: String
  def description: String
  @transient def scalaType: Type
  //var `type`: String = JSONType.Any
  var optional: Boolean = false
  var default: Option[String] = None
  
  implicit def param2string( param: this.type ): String = this.name
}

trait IJSONRPC2MethodParameter extends JSONRPC2Schema {
  def name: String
  def description: String
  //def `type`: String
  @transient def scalaType: Type
  def optional: Boolean
  def default: Option[String]
  
  def toSerializableJSONRPC2Parameter() = {
    this match {
      case serializableParameter: JSONRPC2MethodParameter => serializableParameter
      case _ => JSONRPC2MethodParameter(
        name = name,
        description = description,
        scalaType = scalaType,
        optional = optional,
        default = default 
      )
    }
  }
}

case class JSONRPC2MethodResult(
  @transient val scalaType: Type,
  var description: String = null
) extends JSONRPC2Schema {
  //var `type`: String = JSONType.Any
  var optional = false
  def default = None
}

trait JSONRPC2Schema extends JsonSchema with LazyLogging {
  
  // Interface
  def scalaType: Type
  
  @JsonProperty var items: Option[JsonSchemaRef] = None // only for JSONType.Array type
  @JsonProperty var properties: Option[Map[String,JsonSchemaRef]] = None // only for JSONType.Object type
  
  lazy val `type`: String = {
    
    var jsonType = JSONType.Any.toString
    try {
      require( scalaType != null, "scalaType is null" )
      
      val scalaSchema = Schema.schemaOf(scalaType)
      val jsonSchemaRef = scalaSchema.toJsonSchema( collection.mutable.HashMap() )
      
      jsonSchemaRef match {
        case jsonSchema: JsonSchema => {
          // Update JSON type of current object
          jsonType = jsonSchema.`type`
          
          if ( jsonSchema.`type` == JSONType.Array.toString ) {
            this.items = jsonSchema.items
          } else if ( jsonSchema.`type` == JSONType.Object.toString ) {
            this.properties = jsonSchema.properties
          }
        }
        case _ => throw new Exception("jsonSchemaRef must be a JsonSchema instance")
      }
      
    } catch { case e: Exception => { this.logger.error("can't build JSON schema",e) } }
    
    jsonType
  }
  
  def getScalaTypeAsString(): Option[String] = {
    try {
      Some(Schema.schemaOf(scalaType).toString)
    } catch {
      case e: Exception => None
    }
  }
}

/*
  "POST" - The service call should be sent to the server using the HTTP POST method with the service message in the post body.
  "GET" - The service call should be sent to the server using the HTTP GET method with the service message in URL query string.
  "REST" - The service call can be sent to the server using standard REST HTTP methods (GET, PUT, DELETE, and POST) with parameters in the URL query string. All clients do not necessarily have authorization to use all the available HTTP methods for the provided service. A PUT and POST method require a content body for the service.
  "JSONP" - The service call should be sent to the server using the HTTP GET method with parameters in the URL using standard parameter encoding and the response should be prepended with a call using the JSONP protocol. The callback parameter name should be defined with the jsonpCallbackParameter property.
  "TCP/IP" - The service call should take place directly over TCP/IP.
*/
object JSONRPC2ServiceTransport extends EnhancedEnum {
  
  val POST = Value("POST")
  val GET = Value("GET")
  val REST  = Value("REST")
  val JSONP  = Value("JSONP")
  val TCP_IP = Value("TCP/IP")
} 

/*
"URL" - The response should be value returned from the method call. For example: name=value would be included the query string or request body. URL encoded parameters allow multiple parameters with the same name, so it is recommended that lists/arrays should correspond to multiple parameters with the same name.
"PATH" - The parameters should be included in the path of the URL. For example: name/value would be appended the request path.
"JSON" - The parameters will be transferred as a JSON string. For example: {"name":"value"} would be included in the query string or request body.
"JSON-RPC-1.0" - The JSON-RPC 1.0 protocol should be used. For example: {"id":1,"method":"foo","params":["value"] } would be included in the request body.
"JSON-RPC-1.1" - The JSON-RPC 1.1 protocol is deprecated and should not be used.
"JSON-RPC-2.0" - The JSON-RPC 2.0 protocol should be used.
 */
object JSONRPC2ServiceEnvelope extends EnhancedEnum {
  
  val URL = Value("URL")
  val PATH = Value("PATH")
  val JSON  = Value("JSON")
  val JSONRPC1  = Value("JSON-RPC-1.0")
  val JSONRPC1_1 = Value("JSON-RPC-1.1")
  val JSONRPC2 = Value("JSON-RPC-2.0")
  
}
