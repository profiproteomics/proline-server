package fr.proline.cortex.util.fs

import java.nio.file.FileSystems
import java.nio.file.Files

import scala.collection.JavaConversions.asScalaSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.FileUtils
import fr.profi.util.StringUtils
import fr.proline.admin.service.db.SetupProline
import fr.proline.cortex.api.fs._

/**
 * Singleton object to handle Proline-Core (Server side) MountPoints.
 * This version is NOT dynamic : configuration file is parsed only once when Singleton instance is initialized (ProlineWeb-Core startup).
 *
 * Use of public methods is thread-safe : inner Maps are are @GuardedBy("m_registryLock").
 *
 * Note : this implementation use {{{java.nio.file.Path}}} instead of legacy {{{java.io.File}}} API.
 */
object MountPointRegistry extends LazyLogging {

  /* Constants (alway use Constants instead of raw Strings) */
  val MOUNT_POINTS_KEY = "mount_points"

  /* Directory types */
  val RESULT_FILES_DIRECTORY = "result_files"

  val RAW_FILES_DIRECTORY = "raw_files"

  val MZDB_FILES_DIRECTORY = "mzdb_files"

  /* (Singleton) Instance variables */
  private val m_registryLock = new Object()

  /* All mutable fields (Maps) are @GuardedBy("m_registryLock") */
  private val m_mountPointsPerType = mutable.Map.empty[String, ArrayBuffer[MountPoint]]

  private val m_mountPointsPerLabel = mutable.Map.empty[String, ArrayBuffer[MountPoint]]

  /* Singleton constructor */
  init()

  /* Public methods */
  /**
   * Return hard-coded Proline directory types (result_files, raw_files, mzdb_files).
   */
  def retrieveAllDirectoryTypes(): Array[String] = {
    return Array(RESULT_FILES_DIRECTORY, RAW_FILES_DIRECTORY, MZDB_FILES_DIRECTORY)
  }

  /**
   * Return all MountPoints for all Proline directory types and all labels.
   *  @param withAbsolutePath If {{{true}}}, the real absolute path of directories of the server is returned.
   */
  def retrieveAllMountPoints(withAbsolutePath: Boolean): Array[MountPoint] = {
    val result = ArrayBuffer.empty[MountPoint]

    m_registryLock.synchronized {

      for (entry <- m_mountPointsPerType; mp <- entry._2) {

        if (withAbsolutePath) {
          result += (mp.copy()) // Alway return a clone
        } else {
          result += (mp.copy(path = ""))
        }

      }

    } // End of synchronized block on m_registryLock

    result.toArray
  }

  /**
   * Return all MountPoints for given Proline directory type.
   *  @param withAbsolutePath If {{{true}}}, the real absolute path of directories of the server is returned.
   */
  def retrieveMountPointsByType(directoryType: String, withAbsolutePath: Boolean): Array[MountPoint] = {
    require(!StringUtils.isEmpty(directoryType), "Invalid directoryType")

    val result = ArrayBuffer.empty[MountPoint]

    m_registryLock.synchronized {
      val optionalListPerType = m_mountPointsPerType.get(directoryType)

      if (optionalListPerType.isDefined) {
        val listPerType = optionalListPerType.get

        if ((listPerType != null) && !listPerType.isEmpty) {

          for (mp <- listPerType) {

            if (withAbsolutePath) {
              result += (mp.copy()) // Alway return a clone
            } else {
              result += (mp.copy(path = ""))
            }

          } // End loop for each mp

        }

      }

    } // End of synchronized block on m_registryLock

    result.toArray
  }

  /**
   * Return all MountPoints for given {{{label}}}.
   *  @param withAbsolutePath If {{{true}}}, the real absolute path of directories of the server is returned.
   */
  def retrieveMountPointsByLabel(label: String, withAbsolutePath: Boolean): Array[MountPoint] = {
    require(!StringUtils.isEmpty(label), "Invalid label")

    val result = ArrayBuffer.empty[MountPoint]

    m_registryLock.synchronized {
      val optionalListPerLabel = m_mountPointsPerLabel.get(label)

      if (optionalListPerLabel.isDefined) {
        val listPerLabel = optionalListPerLabel.get

        if ((listPerLabel != null) && !listPerLabel.isEmpty) {

          for (mp <- listPerLabel) {

            if (withAbsolutePath) {
              result += (mp.copy()) // Alway return a clone
            } else {
              result += (mp.copy(path = ""))
            }

          } // End loop for each mp

        }

      }

    } // End of synchronized block on m_registryLock

    result.toArray
  }

  /**
   * Tries to retrieve a {{{MountPoint}}} for the given local path.   *
   * Note : only work with canonicalizable local path.
   */
  def retrieveMountPointByPath(originLocalPathname: String, directoryTypeHint: Option[String] = None): Option[MountPoint] = {
    require(!StringUtils.isEmpty(originLocalPathname), "Invalid originLocalPathname")

    var foundMointPoint: Option[MountPoint] = None

    var maxPathLength: Int = -1

    try {
      val rawLocalPath = FileSystems.getDefault.getPath(originLocalPathname)

      val canonicalLocalPath = rawLocalPath.toAbsolutePath.normalize // Canonicalize path with Path API
      val canonicalLocalPathname = canonicalLocalPath.toString

      val mountPoints = retrieveAllMountPoints(true)

      if (mountPoints != null) {

        for (mp <- mountPoints) {

          if (canonicalLocalPathname.startsWith(mp.path)) {

            if (directoryTypeHint.isDefined) {
              /* Try to use directoryTypeHint */

              if (directoryTypeHint.get.equals(mp.directoryType)) {
                val pathLength = mp.path.length

                if (pathLength > maxPathLength) {
                  maxPathLength = pathLength

                  foundMointPoint = Some(mp.copy()) // Alway return a clone
                }

              } // End if (directoryTypeHint == mp.directoryType)

            } else {
              val pathLength = mp.path.length

              if (pathLength > maxPathLength) {
                maxPathLength = pathLength

                foundMointPoint = Some(mp.copy()) // Alway return a clone
              }

            } // End if (directoryTypeHint is not defined)

          } // End if (canonicalLocalPathname startWith mp.path)

        } // End loop for each mp

      } // End if (mountPoints is not null)

    } catch {

      case ex: Exception => {
        /* Log and re-throw */
        val message = "Cannot build path \"" + originLocalPathname + '\"'
        logger.error(message, ex)

        throw new RuntimeException(message, ex)
      }

    }

    foundMointPoint
  }

  /**
   * Tries to replace the first part of given path by a MountPoint absolute path if the given {{{originPath}}} starts with a known label.
   * Given {{{originPath}}} must me splittable by {{{FileUtils.splitFilePath()}}} .
   * If no label is found : returns given {{{originPath}}} unchanged.
   * @param directoryTypeHint Directory type used if multiple MountPoint are found for a label.
   */
  def replacePossibleLabel(originPathname: String, directoryTypeHint: Option[String] = None): LocalPath = {
    require(!StringUtils.isEmpty(originPathname), "Invalid originPathname")
    require((directoryTypeHint != null), "DirectoryTypeHint Option is null")

    var localPathname: String = originPathname
    var foundMountPoint: Option[MountPoint] = None

    val pathParts = FileUtils.splitFilePath(originPathname)
    if (pathParts.length > 0) {

      val possibleLabel = pathParts(0)
      if (!StringUtils.isEmpty(possibleLabel)) {

        val mountPoints = retrieveMountPointsByLabel(possibleLabel, true)
        if (mountPoints != null) {

          if (mountPoints.length == 1) {
            val mp = mountPoints(0)

            localPathname = buildLocalFilePath(mp.path, pathParts)
            logger.debug("Replace \"" + originPathname + "\" by \"" + localPathname + '\"')

            foundMountPoint = Some(mp.copy()) // Alway return a clone
          } else if ((mountPoints.length > 1) && (directoryTypeHint.isDefined)) {
            /* If there are multiple mountPoints for same label, try to use directoryTypeHint */

            var found: Boolean = false

            for (mp <- mountPoints) {

              if (directoryTypeHint.get.equals(mp.directoryType)) {
                found = true

                localPathname = buildLocalFilePath(mp.path, pathParts)
                logger.debug("Replace \"" + originPathname + "\" by \"" + localPathname + '\"')

                foundMountPoint = Some(mp.copy()) // Alway return a clone
              } // End if (directoryTypeHint == mp.directoryType)

            } // End loop for each mp

            if (!found) {
              logger.warn("Cannot found MountPoint for type [" + directoryTypeHint + "] and label [" + possibleLabel + ']')
            }

          }

        } // End if (mountPoints is not null)

      } // End if (possibleLabel is valid)

    }

    new LocalPath(localPathname, foundMountPoint)
  }

  /* Private methods */
  private def init() {

    /* Load all Mountpoint from "application.conf" file */

    try {
      val mountPointsConfig = SetupProline.getConfigParams.getConfig(MOUNT_POINTS_KEY)

      if (mountPointsConfig.hasPath(RESULT_FILES_DIRECTORY)) {
        val resultFileMountPoints = mountPointsConfig.getConfig(RESULT_FILES_DIRECTORY)

        if (resultFileMountPoints.isEmpty) {
          logger.warn("No MountPoints defined for type [" + RESULT_FILES_DIRECTORY + ']')
        } else {
          parseConfig(RESULT_FILES_DIRECTORY, resultFileMountPoints)
        }

      } else {
        logger.warn("No MountPoints entry for type [" + RESULT_FILES_DIRECTORY + ']')
      }

      if (mountPointsConfig.hasPath(RAW_FILES_DIRECTORY)) {
        val rawFileMountPoints = mountPointsConfig.getConfig(RAW_FILES_DIRECTORY)

        if (rawFileMountPoints.isEmpty) {
          logger.warn("No MountPoints defined for type [" + RAW_FILES_DIRECTORY + ']')
        } else {
          parseConfig(RAW_FILES_DIRECTORY, rawFileMountPoints)
        }

      } else {
        logger.warn("No MountPoints entry for type [" + RAW_FILES_DIRECTORY + ']')
      }

      if (mountPointsConfig.hasPath(MZDB_FILES_DIRECTORY)) {
        val mzdbFileMountPoints = mountPointsConfig.getConfig(MZDB_FILES_DIRECTORY)

        if (mzdbFileMountPoints.isEmpty) {
          logger.warn("No MountPoints defined for type [" + MZDB_FILES_DIRECTORY + ']')
        } else {
          parseConfig(MZDB_FILES_DIRECTORY, mzdbFileMountPoints)
        }

      } else {
        logger.warn("No MountPoints entry for type [" + MZDB_FILES_DIRECTORY + ']')
      }

    } catch {
      case t: Throwable => logger.error("Error initializing MountPointRegistry from \"application.conf\" file", t)
    }

    /* Logg all found MountPoints with full path at Info level */
    val messageBuilder = new StringBuilder()
    messageBuilder.append("Found MountPoints :")

    val allMountPoints = retrieveAllMountPoints(true)

    for (mp <- allMountPoints) {
      messageBuilder.append(StringUtils.LINE_SEPARATOR)
      messageBuilder.append("MountPoint type [").append(mp.directoryType)
      messageBuilder.append("]  label [").append(mp.label)
      messageBuilder.append("]  path \"").append(mp.path).append('\"')
    }

    logger.info(messageBuilder.toString)
  }

  private def parseConfig(directoryType: String, config: Config) {
    assert(!StringUtils.isEmpty(directoryType), "Invalid directoryType")
    assert((config != null), "Config is null")

    val defaultFileSystem = FileSystems.getDefault

    val entries = config.entrySet

    for (entry <- entries) {
      val label = entry.getKey

      if (StringUtils.isEmpty(label)) {

        if (label == null) {
          logger.error("Label for type [" + directoryType + "] is NULL")
        } else {
          logger.error("Invalid label for type [" + directoryType + "] : [" + label + ']')
        }

      } else {
        val rawPath = config.getString(label)

        if (rawPath == null) {
          logger.error("Path for type [" + directoryType + "] label [" + label + "] is NULL")
        } else {

          try {
            val path = defaultFileSystem.getPath(rawPath)

            if (Files.exists(path)) {
              val canonicalPath = path.toAbsolutePath.normalize // Canonicalize path with Path API
              val canonicalPathname = canonicalPath.toString // Store canonical path as string

              val mountPoint = MountPoint(directoryType, label, canonicalPathname)

              addMountPoint(mountPoint)
            } else {
              logger.error("Invalid or non-existent path for type [" + directoryType + "] label [" + label + "] : \"" + path + '\"')
            }

          } catch {
            case ex: Exception => logger.error("Error handling directory type ["
              + directoryType + "] label [" + label + "] path \"" + rawPath + '\"', ex)
          }

        } // End if (rawPath is not null)

      } // End if (label is valid)

    } // End loop for each entry

  }

  private def addMountPoint(mountPoint: MountPoint) {
    assert((mountPoint != null), "MountPoint is null")

    m_registryLock.synchronized {

      /* Add to m_mountPointsPerType Map first (to check if multiple label for same type) */

      var listPerType: ArrayBuffer[MountPoint] = null

      val optionalListPerType = m_mountPointsPerType.get(mountPoint.directoryType)

      if (optionalListPerType.isDefined) {
        listPerType = optionalListPerType.get

        if (!listPerType.isEmpty) {

          for (mp <- listPerType) {

            /* Check if we have already a MountPoint with same type AND same label */
            if (mp.label.equals(mountPoint.label)) {
              val message = "A MountPoint of type [" + mountPoint.directoryType + "] ALREADY exists with label [" + mountPoint.label + ']'

              logger.error(message)

              throw new RuntimeException(message)
            }

          } // End loop for each existing mp

        }

      } else {
        listPerType = ArrayBuffer.empty[MountPoint]
        m_mountPointsPerType.put(mountPoint.directoryType, listPerType)
      }

      listPerType += mountPoint

      /* Add to m_mountPointsPerLabel Map */

      var listPerLabel: ArrayBuffer[MountPoint] = null

      val optionalListPerLabel = m_mountPointsPerLabel.get(mountPoint.label)

      if (optionalListPerLabel.isDefined) {
        listPerLabel = optionalListPerLabel.get
      } else {
        listPerLabel = ArrayBuffer.empty[MountPoint]
        m_mountPointsPerLabel.put(mountPoint.label, listPerLabel)
      }

      listPerLabel += mountPoint

      logger.debug(mountPoint + "  added")
    } // End of synchronized block on m_registryLock

  }

  private def buildLocalFilePath(absolutePath: String, pathParts: Array[String]): String = {
    val tempParts = pathParts.clone()

    tempParts(0) = absolutePath

    val defaultFileSystem = FileSystems.getDefault

    val rawLocalPathname = tempParts.mkString(defaultFileSystem.getSeparator)

    try {
      val rawLocalPath = defaultFileSystem.getPath(rawLocalPathname)

      val canonicalLocalPath = rawLocalPath.toAbsolutePath.normalize // Canonicalize path with Path API
      canonicalLocalPath.toString // Return canonical path as string
    } catch {

      case ex: Exception => {
        /* Log and re-throw */
        val message = "Cannot build path \"" + rawLocalPathname + '\"'
        logger.error(message, ex)

        throw new RuntimeException(message, ex)
      }

    }

  }

}

class LocalPath(val localPathname: String, val mountPoint: Option[MountPoint]) {

  /* Constructor checks */
  require(!StringUtils.isEmpty(localPathname), "Invalid localPathname")

  require((mountPoint != null), "MountPoint Option is null")

}
