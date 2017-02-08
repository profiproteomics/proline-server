package fr.profi.util.exception

object ExceptionUtils {
  
  def wrapThrowable(newMessage: String, throwable: Throwable, appendCause: Boolean = false): Exception = {
    
    val message = if (appendCause == false) {
      newMessage
    } else {
      val cause = Option(throwable.getMessage).orElse(Option(throwable.getCause.getMessage)).orNull
      if (cause != null) s"$newMessage because $cause" else newMessage
    }
    
    new Exception(message, throwable)
  }
  
}