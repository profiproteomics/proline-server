package fr.profi.util.exception

object SerializableStackTraceElement {
  
  private val PartialTraceExtractor = """(.+)\.(.+)\(.+""".r
  private val FullTraceExtractor = """(.+)\.(.+)\((.+):(\d+)\)""".r
  
  def apply(stackTraceElem: StackTraceElement): SerializableStackTraceElement = {
    SerializableStackTraceElement(stackTraceElem.getClassName, stackTraceElem.getMethodName, stackTraceElem.getFileName, stackTraceElem.getLineNumber)
  }
  
  def apply(stackTraceElemAsStr: String): SerializableStackTraceElement = {
    
    if (stackTraceElemAsStr.endsWith("(Unknown Source)")) {
      val PartialTraceExtractor(className, methodName) = stackTraceElemAsStr
      SerializableStackTraceElement(className, methodName,null, -1)
    } else {
      val FullTraceExtractor(className, methodName, sourceFile, lineNum) = stackTraceElemAsStr
      SerializableStackTraceElement(className, methodName,sourceFile, lineNum.toInt)
    }

  }
  
}

case class SerializableStackTraceElement(
  className: String,
  methodName: String,
  fileName: String,
  lineNumber: Int
) {
  
  def isNativeMethod(): Boolean = {
    lineNumber == -2
  }
  
  def toStackTraceElement(): StackTraceElement = {
    new StackTraceElement(className,methodName,fileName,lineNumber)
  }
  
  def toRawString(): String = {
    val suffix = if (isNativeMethod) "(Native Method)" 
    else if (fileName != null) {
      if (lineNumber >= 0) s"($fileName:$lineNumber)"
      else s"($fileName)"
    } else "(Unknown Source)"
    
    s"$className.$methodName $suffix"
  }
}