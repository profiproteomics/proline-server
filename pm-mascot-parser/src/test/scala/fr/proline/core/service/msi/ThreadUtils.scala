package fr.proline.core.service.msi

import scala.collection.JavaConversions.mapAsScalaMap

import org.junit.Ignore

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.util.StringUtils.LINE_SEPARATOR
import javax.swing.SwingUtilities

@Ignore
object ThreadUtils extends Logging {

  def traceAllThreads() {
    val stackTrace = Thread.getAllStackTraces

    val builder = new StringBuilder("Presents Threads :")
    builder.append(LINE_SEPARATOR)

    for (entry <- stackTrace) {
      val thr = entry._1
      appendThread(builder, thr)
      builder.append(LINE_SEPARATOR)

      val stackTrace = entry._2

      if ((stackTrace == null) || stackTrace.isEmpty) {
        builder.append("    Unknown StackTrace")
        builder.append(LINE_SEPARATOR)
      } else {

        for (trace <- stackTrace) {
          builder.append("    ").append(trace)
          builder.append(LINE_SEPARATOR)
        }

      }

      builder.append(LINE_SEPARATOR)
    }

    logger.info(builder.toString)
  }

  private def appendThread(sb: StringBuilder, thr: Thread) {
    sb.append("Thread #").append(thr.getId)
    sb.append(" [").append(thr.getName).append(']')

    if (Thread.currentThread == thr) {

      if (SwingUtilities.isEventDispatchThread) {
        sb.append(" (EDT *)")
      } else {
        sb.append(" (*)")
      }

    }

    if (thr.isDaemon) {
      sb.append(", daemon")
    }

    sb.append(", priority: ").append(thr.getPriority)
    sb.append(", ").append(thr.getState)

    sb.append(", exceptionHandler: ").append(thr.getUncaughtExceptionHandler)

    sb.append(", Group : ")

    var group: ThreadGroup = thr.getThreadGroup

    var first: Boolean = true

    while (group != null) {

      if (first) {
        first = false
      } else {
        sb.append(" -> ")
      }

      sb.append('[').append(group.getName)
      sb.append("], max priority: ").append(group.getMaxPriority)

      group = group.getParent
    } // End loop on thread groups

  }

}
