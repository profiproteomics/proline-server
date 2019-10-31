package fr.proline.cortex.caller

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

// TODO: move to fr.proline.cortex.caller in Proline-Cortex-API
trait IServiceCall[+T] {
  def request: Future[Any]
  def result: Future[T]
  
  def getRequestId()(implicit ec: ExecutionContext): Future[String]
  
  def map[S](f: T => S)(implicit ec: ExecutionContext): IServiceCall[S]
  def mapResult[S](f: T => S)(implicit ec: ExecutionContext): Future[S] = result.map { f }
  def newResult[S](newResult: Future[S]): IServiceCall[S]
}

