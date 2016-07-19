package fr.proline.cortex.util.reflect

import scala.collection.mutable.HashMap
import scala.reflect.runtime.universe._
import fr.proline.cortex.util.json._
import fr.proline.cortex.util.jsonrpc._

object SchemaConversionRules {
  var camelCase = false
}

// Inspired from:
// - http://stackoverflow.com/questions/14322819/is-there-a-tutorial-on-scala-2-10s-reflection-api-yet
// - http://www.encodedknowledge.com/2013/01/scala-2-10-reflection-experiments/
// - https://github.com/mirkonasato/seriala/blob/master/seriala-core/src/main/scala/io/encoded/seriala/Schema.scala
// - http://dcsobral.blogspot.fr/2012/07/json-serialization-with-reflection-in.html
// - http://dcsobral.blogspot.ch/2012/08/json-serialization-with-reflection-in.html
// - https://bitbucket.org/jaroslav/scala-macro-serialization/src/95da34f53b13db4b477552d3efc23a079ee4866e/msgpack.macro.scala?at=default
// - http://fr.slideshare.net/prasinous/scaladays-2013-final
sealed abstract class Schema(val name: String, val jsonType: JSONType.Value ) {
  
  var description: String = null
  override def toString = name //+ valueSchema.map( "["+_.toString+"]" ).getOrElse("")
  
  def toJsonSchema( jsonSchemaBySchema: HashMap[Schema,JsonSchemaRef] ): JsonSchemaRef
}

trait JsonSchemaConversion {
  
  def name: String
  def description: String
  def jsonType: JSONType.Value
  
  def toJsonSchema(jsonSchemaBySchema: HashMap[Schema,JsonSchemaRef]): JsonSchemaRef = {
    jsonType match {
      case JSONType.Any => JsonObjectSchema( properties = None, description = this.description )
      case JSONType.Object => JsonObjectSchema( properties = None, description = this.description )
      case JSONType.Boolean => JsonValueSchema( jsonType.toString, description = this.description )
      case JSONType.Integer => JsonValueSchema( jsonType.toString, description = this.description )
      case JSONType.Number => JsonValueSchema( jsonType.toString, description = this.description )
      case JSONType.String => JsonValueSchema( jsonType.toString, description = this.description )
      case _ => throw new Exception(s"unsupported JSON type ${jsonType} for Scala type ${name}")
    }
  }
}

sealed trait SchemaContainer extends JsonSchemaConversion {
  
  def valueSchema: Schema
  
  override def toString = name + "["+valueSchema.toString+"]"
  
  override def toJsonSchema(jsonSchemaBySchema: HashMap[Schema,JsonSchemaRef]): JsonSchemaRef = {
    jsonType match {
      case JSONType.Array => {
        JsonArraySchema( items = Some(valueSchema.toJsonSchema(jsonSchemaBySchema)) )
      }
      case JSONType.Object if name == "Option" => {
        
        // Set description of the value schema
        valueSchema.description = this.description
        
        val jsonSchema = valueSchema.toJsonSchema(jsonSchemaBySchema)
        jsonSchema.setOptional(true)
        jsonSchema
      }
      case _ => throw new Exception(s"unsupported JSON type ${jsonType} for Scala type ${name}")
    }
  }
  
}

sealed trait KVSchemaContainer extends SchemaContainer {
  def keySchema: Schema
  
  override def toString = s"${name}[${keySchema.toString},${valueSchema.toString}]"
  
  override def toJsonSchema(jsonSchemaBySchema: HashMap[Schema,JsonSchemaRef]): JsonSchema = {
    jsonType match {
      case JSONType.Object if name == "Map" => { // TODO: waht about HashMap ???
        JsonObjectSchema(
          properties = Some( Map(
            "keys" -> keySchema.toJsonSchema(jsonSchemaBySchema),
            "values" -> valueSchema.toJsonSchema(jsonSchemaBySchema)
          ))
        )
      }
      case _ => throw new Exception(s"unsupported JSON type ${jsonType} for Scala type ${name}")
    }
  }
}

case class AnySchema() extends Schema("Any",JSONType.Any) with JsonSchemaConversion
case class AnyRefSchema() extends Schema("AnyRef",JSONType.Object) with JsonSchemaConversion
case class AnyValSchema() extends Schema("AnyVal",JSONType.Any) with JsonSchemaConversion
case class BooleanSchema() extends Schema("Boolean",JSONType.Boolean) with JsonSchemaConversion
case class IntSchema() extends Schema("Int",JSONType.Integer) with JsonSchemaConversion
case class LongSchema() extends Schema("Long",JSONType.Integer) with JsonSchemaConversion
case class FloatSchema() extends Schema("Float",JSONType.Number) with JsonSchemaConversion
case class DoubleSchema() extends Schema("Double",JSONType.Number) with JsonSchemaConversion
case class StringSchema() extends Schema("String",JSONType.String) with JsonSchemaConversion

case class ArraySchema(valueSchema: Schema) extends Schema("Array",JSONType.Array) with SchemaContainer
case class OptionSchema(valueSchema: Schema) extends Schema("Option",JSONType.Object) with SchemaContainer
case class ListSchema(valueSchema: Schema) extends Schema("List",JSONType.Array) with SchemaContainer
case class MapSchema(keySchema: Schema, valueSchema: Schema) extends Schema("Map",JSONType.Object) with KVSchemaContainer
case class SeqSchema(valueSchema: Schema) extends Schema("Seq",JSONType.Array) with SchemaContainer

case class ObjectSchema(
  override val name: String,
  scalaTpe: Type,
  fieldBuilder: ObjectSchema => List[(String, Schema)]
) extends Schema(name,JSONType.Object) {

  val fields = fieldBuilder(this)
  
  override def toString = name + "(" + this._getFieldsAsString() + ")"
  
  private def _getFieldsAsString(): String = fields.map( f => f._1 + ": " + _stringifySchema(f._2) ).mkString(",")

  private def _stringifySchema(schema: Schema) = schema match {
    case x: ObjectSchema => x.name + "(" + x._getFieldsAsString() + ")"
    case x => x.toString
  }
  
  override def toJsonSchema(jsonSchemaBySchema: HashMap[Schema,JsonSchemaRef]): JsonSchemaRef = {
    // Cyclic schema detection
    if( jsonSchemaBySchema.contains(this) ) {
      val refName = if( SchemaConversionRules.camelCase ) this.name else snakizeFieldName(this.name)
      return JsonSchemaReference( ref = Some(refName) )
    }
    
    jsonSchemaBySchema += this -> null
    
    val props = Map.newBuilder[String,JsonSchemaRef]
    for( field <- fields ) {
      val propName = if( SchemaConversionRules.camelCase ) field._1 else snakizeFieldName(field._1)
      val schema = field._2

      props += propName -> schema.toJsonSchema(jsonSchemaBySchema)
    }
    
    val newJsonSchema = JsonObjectSchema( properties = Some(props.result) )
    jsonSchemaBySchema(this) = newJsonSchema
    
    newJsonSchema
  }
  
  private def snakizeFieldName( n: String ): String = {
    n.split("(?=\\p{Upper})").map(_.toLowerCase()).mkString("_")
  }
  
}

object Schema {

  val BooleanType = typeOf[Boolean]
  val IntType = typeOf[Int]
  val LongType = typeOf[Long]
  val FloatType = typeOf[Float]
  val DoubleType = typeOf[Double]
  val StringType = typeOf[String]
  val OptionType = typeOf[Option[_]]
  val ArrayType = typeOf[Array[_]]
  val ListType = typeOf[List[_]]
  val MapType = typeOf[Map[_, _]]
  val SeqType = typeOf[Seq[_]]

  def schemaOf( tpe: Type ) = buildSchema(tpe, Map())
  def schemaOf[T]()(implicit typeTag: TypeTag[T]) = buildSchema(typeTag.tpe, Map())

  private def buildSchema(scalaType: Type, knownObjects: Map[Type, Schema]): Schema = scalaType match {
    case t if t <:< BooleanType => BooleanSchema()
    case t if t <:< IntType => IntSchema()
    case t if t <:< LongType => LongSchema()
    case t if t <:< FloatType => FloatSchema()
    case t if t <:< DoubleType => DoubleSchema()
    case t if t <:< StringType => StringSchema()
    case t if t <:< ArrayType => new ArraySchema(buildSchema(typeArgs(t)(0), knownObjects))
    case t if t <:< OptionType => new OptionSchema(buildSchema(typeArgs(t)(0), knownObjects))
    case t if t <:< ListType => new ListSchema(buildSchema(typeArgs(t)(0), knownObjects))
    case t if t <:< MapType => {
      new MapSchema(
        buildSchema(typeArgs(t)(0), knownObjects),
        buildSchema(typeArgs(t)(1), knownObjects)
      )
    }
    case t if t <:< SeqType => new SeqSchema(buildSchema(typeArgs(t)(0), knownObjects))
    // TODO: check if we can check this at the type level instead of using the typeSymbol fullName
    case t if t.typeSymbol.fullName == "scala.Any" => AnySchema()
    case t if t.typeSymbol.fullName == "scala.AnyRef" => AnyRefSchema()
    case t if t.typeSymbol.fullName == "scala.AnyVal" => AnyValSchema()
    case t if knownObjects.contains(t) => knownObjects(t)
    case t => {
      
      // Initialize fields later to handle circular dependencies
      new ObjectSchema(
        t.typeSymbol.name.decoded,
        scalaType,
        thisSchema => fieldsOf(t, knownObjects + (t -> thisSchema))
      )

    }
    
  }

  private def typeArgs(scalaType: Type): List[Type] = scalaType match {
    // see http://stackoverflow.com/questions/12842729/finding-type-parameters-via-reflection-in-scala-2-10
    case t: TypeRefApi => t.args
  }
  
  private def fieldsOf(scalaType: Type, knownObjects: Map[Type, Schema]): List[(String, Schema)] = {

    // Try to reflect constructor members
    val ctor = scalaType.member(nme.CONSTRUCTOR)
    
    val constructorFields = if( ctor != NoSymbol ) {
      try {
        ctor.asMethod.paramss.head.map { p =>
          
          val fieldName = p.name.decoded
          val fieldSchema = buildSchema(p.typeSignature, knownObjects)
          
          annotateField(p, fieldSchema)
          
          (fieldName, fieldSchema)
        }
      } catch {
        case e: Exception => null
      }

    } else null
    
    if( constructorFields != null ) constructorFields
    else {
      
      // Reflect members
      // Source: http://stackoverflow.com/questions/21465661/scala-reflection-to-access-all-public-fields-at-runtime
      // TODO: Do not include transient fields
      // See: http://stackoverflow.com/questions/24125409/scala-macros-checking-if-a-java-field-is-marked-as-transient
      scalaType.members
        .collect({case x if x.isTerm => x.asTerm})
        .withFilter(f => f.isPublic && f.isGetter )
        .map { field =>
        
        val fieldName = field.name.decoded
        val fieldSchema = buildSchema(field.typeSignature.typeSymbol.asClass.toType, knownObjects)
        
        annotateField( field, fieldSchema )
        
        (fieldName, fieldSchema)
      } toList
      
    }

  }
  
  // Add description to the fieldSchema
  private def annotateField( field: Symbol, fieldSchema: Schema ) {
    field.annotations.find(_.tpe =:= typeOf[FieldDescription]).map { fieldAnnotation =>
      val desc = fieldAnnotation.javaArgs.head._2.toString
      fieldSchema.description = desc.substring(1, desc.length-1).replaceAll("\\\\","")
    }
  }

}