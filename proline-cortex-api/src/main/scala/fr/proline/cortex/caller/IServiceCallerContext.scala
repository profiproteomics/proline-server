package fr.proline.cortex.caller

import scala.concurrent.ExecutionContext

// TODO: move to fr.proline.cortex.caller.contex in Proline-Cortex-API
trait IServiceCallerContext {
  def clientName: Option[String]
  def close(): Unit
}