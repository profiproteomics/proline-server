package fr.proline.cortex.util.json

import fr.profi.util.lang.EnhancedEnum

object JSONType extends EnhancedEnum {
  
  val String = Value("string")
  val Number = Value("number")
  val Integer = Value("integer")
  val Boolean  = Value("boolean")
  val True  = Value("true")
  val False  = Value("false")
  val Object  = Value("object")
  val Array = Value("array")
  val Null = Value("null")
  val Any = Value("any")

}