package fr.proline.jms

import java.util.HashMap
import java.util.concurrent.Future

object ServiceManager {
  
  
  private val msgId2FuturesMap : HashMap[String, Future[_]]  = new HashMap[String, Future[_]]()
  private val runnable2FutureMap : HashMap[Runnable, Future[_]]  = new HashMap[Runnable, Future[_]]()
  
  def addRunnale2FuturEntry(r: Runnable, f: Future[_]) = {
    runnable2FutureMap.put(r,f)
  }
  
  def getRunnableFuture(r: Runnable) : Future[_] = {
    runnable2FutureMap.get(r)
  }
  
   def addMsg2FutureEntry(key: String, f: Future[_]) = {
       msgId2FuturesMap.put(key, f)
   }
   
   def removeMsg2Future(key: String){
     msgId2FuturesMap.remove(key)
   }
   
   def getFutureForMessage(key: String) : Future[_] = {
     msgId2FuturesMap.get(key)
   }
   
}