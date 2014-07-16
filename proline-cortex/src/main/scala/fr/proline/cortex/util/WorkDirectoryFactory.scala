package fr.proline.cortex.util

import java.io.File
import java.nio.file.Files
import java.util.Timer
import java.util.TimerTask
import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.util.ThreadLogger

object WorkDirectoryFactory extends Logging {

  /**
   * Creates a temporary ProlineWork directory. Old files ( > 24 hours) are purged every hour.
   */
  lazy val prolineWorkDirectory = createProlineWorkDirectory()

  private def createProlineWorkDirectory(): File = {

    try {
      val workPath = Files.createTempDirectory("ProlineWork_")
      val workDirectory = workPath.toFile()

      addShutdownHook(workDirectory)

      addPurgeTimer(workDirectory)

      logger.debug("Temp directory [" + workDirectory.getAbsolutePath + "] created")

      workDirectory
    } catch {

      case ex: Exception => {
        /* Log and re-throw */
        val message = "Unable to create ProlineWork temp directory"
        logger.error(message, ex)

        throw new RuntimeException(message, ex)
      }

    }

  }

  private def addShutdownHook(tempDirectory: File) {
    assert((tempDirectory != null) && tempDirectory.isDirectory, "addShutdownHook() invalid tempDirectory")

    val target = new Runnable() {

      def run() {
        val absolutePathname = tempDirectory.getAbsolutePath
        val files = tempDirectory.listFiles()

        if (files == null) {
          logger.warn("Cannot list [" + absolutePathname + ']')
        } else {

          if (files.isEmpty) {
            val result = tempDirectory.delete()

            if (result) {
              logger.info("Empty directory [" + absolutePathname + "] DELETED")
            } else {
              logger.error("Cannot delete [" + absolutePathname + ']')
            }

          } // End if (tempDirectory is empty)

        } // End if (files is not null)

      }

    }

    val hook = new Thread(target, "Thread-Shutdown-DeleteTempDir")
    hook.setPriority(Thread.NORM_PRIORITY)
    hook.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName()))

    Runtime.getRuntime.addShutdownHook(hook)
  }

  private def addPurgeTimer(tempDirectory: File) {
    val timer = new Timer("Timer-PurgeTempDir")

    timer.scheduleAtFixedRate(new TempDirectoryPurgeTask(tempDirectory), TimeUnit.MINUTES.toMillis(15), TimeUnit.HOURS.toMillis(1))
  }

}

class TempDirectoryPurgeTask(tempDirectory: File) extends TimerTask with Logging {

  val PURGE_DELAY = TimeUnit.HOURS.toMillis(24)

  /* Constructor checks */
  assert((tempDirectory != null) && tempDirectory.isDirectory, "TempDirectoryPurgeTask() invalid tempDirectory")

  def run() {
    val currentThread = Thread.currentThread

    if (!(currentThread.getUncaughtExceptionHandler.isInstanceOf[ThreadLogger])) {
      currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName()))
    }

    val now = System.currentTimeMillis

    val files = tempDirectory.listFiles()

    if (files == null) {
      logger.warn("Cannot list [" + tempDirectory.getAbsoluteFile + ']')
    } else {

      for (file <- files) {

        if (file.lastModified + PURGE_DELAY < now) {
          val fileAbsolutePathname = file.getAbsolutePath

          val result = file.delete()

          if (result) {
            logger.info("Old file [" + fileAbsolutePathname + "] DELETED")
          } else {
            logger.error("Cannot delete [" + fileAbsolutePathname + ']')
          }

        } // End if (file is old)

      } // End loop for each file

    } // End if (files is not null)

  }

}
