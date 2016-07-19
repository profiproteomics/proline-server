package fr.proline.cortex.util.json

import com.fasterxml.jackson.annotation.JsonProperty

// TODO: implement the v4 draft (required spec has changed)
// This may remove this need of having un un-implemented setOptional method in JsonSchemaReference case class
// see http://stackoverflow.com/questions/16204871/what-is-the-difference-between-required-vs-optional-in-json-schema?rq=1
trait JsonSchemaRef {
  val ref: Option[String] = None
  def setOptional( optional: Boolean )
}

trait JsonSchema extends JsonSchemaRef {
  def `type`: String
  var optional: Boolean
  def default: Option[String]
  def description: String
  def items: Option[JsonSchemaRef] // only for JSONType.Array type
  def properties: Option[Map[String,JsonSchemaRef]] // only for JSONType.Object type
  
  def setOptional( optional: Boolean ) { this.optional = optional }
} 

// TODO: create a JsonSchemaBuilder and replace var optional by def optional
case class JsonValueSchema(
  `type`: String,
  var optional: Boolean = false,
  var default: Option[String] = None,
  var description: String = null
) extends JsonSchema {
  val items = None
  val properties = None
}

case class JsonArraySchema(
  var optional: Boolean = false,
  items: Option[JsonSchemaRef],
  description: String = null
) extends JsonSchema {
  val `type`: String = JSONType.Array
  val default = None
  val properties = None
}

case class JsonObjectSchema(
  var optional: Boolean = false,
  properties: Option[Map[String,JsonSchemaRef]],
  description: String = null
) extends JsonSchema {
  val `type`: String = JSONType.Object
  val default = None
  val items = None
}

case class JsonSchemaReference( @JsonProperty(value = "$ref") override val ref: Option[String]) extends JsonSchemaRef {
  def setOptional( optional: Boolean ) {  }
}


