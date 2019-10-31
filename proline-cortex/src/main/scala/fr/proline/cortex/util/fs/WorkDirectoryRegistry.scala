package fr.proline.cortex.util.fs

import java.io.File
import java.io.IOException

import scala.collection.mutable.HashSet

object WorkDirectoryRegistry {

  private val _registeredDirSet = HashSet.empty[File]
  this.registerWorkDirectory(WorkDirectoryFactory.prolineWorkDirectory)

  def isDirectoryRegistered(dir: File) {
    require(dir.isDirectory, "invalid directory")
    _registeredDirSet.contains(dir)
  }

  /**
   * Returns false if the directory doesn't exist or
   * is not located in one of the registered working directories.
   */
  def isManagedDirectory(dir: java.io.File): Boolean = {
    require(dir.isDirectory, "invalid directory")
    this.isManagedResource(dir)
  }

  /**
   * Returns false if the file doesn't exist or
   * is not located in one of the registered working directories.
   */
  def isManagedFile(file: java.io.File): Boolean = {
    require(file.isFile, "invalid file")
    this.isManagedResource(file)
  }

  /**
   * Returns false if the resource doesn't exist or
   * is not located in one of the registered working directories.
   */
  def isManagedResource(resource: java.io.File): Boolean = {
    if (!resource.exists) return false
    val resourcePath = resource.getCanonicalPath

    for (dir <- _registeredDirSet) {
      val dirPath = dir.getCanonicalPath
      if (resourcePath.startsWith(dirPath)) return true
    }

    false
  }

  def registerWorkDirectory(workDir: File) {
    require(workDir.isDirectory, "invalid directory")
    if (!workDir.exists) {
      throw new IOException("directory [" + workDir + "] does not exist")
    }

    this.synchronized {
      _registeredDirSet += workDir
    }
  }

  def unregisterWorkDirectory(workDir: File) {
    require(workDir.isDirectory, "invalid directory")

    this.synchronized {
      _registeredDirSet -= workDir
    }
  }

}