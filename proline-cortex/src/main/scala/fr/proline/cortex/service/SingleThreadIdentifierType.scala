package fr.proline.cortex.service

object SingleThreadIdentifierType extends Enumeration {
  
  val IMPORT_SINGLETHREAD_IDENT  = Value("ImportThread")
  val QUANTIFY_SINGLETHREAD_IDENT  = Value("quantifyThread")
  val QUANTIFYSC_SINGLETHREAD_IDENT  = Value("quantifySCThread")
  val SHORT_SERVICES_SINGLETHREAD_IDENT  = Value("shortServiceThread")
  
}
