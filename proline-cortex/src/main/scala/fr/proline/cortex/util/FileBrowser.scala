package fr.proline.cortex.util

import java.io.File
import java.nio.file.FileSystems
import java.nio.file.FileVisitOption
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.TimeUnit

import scala.util.matching.Regex

import com.typesafe.scalalogging.LazyLogging

trait FileOrDirAttrs {
  val path: String // file or dir path
  val name: String
  val isDir: Boolean

  def isFile = !isDir
}

case class FileAttrs(
  path: String, // file path
  name: String,
  size: Long,
  lastmodified: Int, // mod time since epoch
  extension: String) extends FileOrDirAttrs {
  val isDir = false
}

case class DirAttrs(
  path: String, // dir path
  name: String,
  lastmodified: Int // mod time since epoch
  //subdirs: Seq[DirProps] = Seq(),
  //files: Seq[FileProps] = Seq()
  ) extends FileOrDirAttrs {
  val isDir = true
}

/**
 * @author David Bouyssie
 *
 */
object FileBrowser extends LazyLogging {

  def getDirectories(localPathname: String, mountPoint: Option[MountPoint], rule: String, maxDepth: Int = 1): Seq[DirAttrs] = {
    require((localPathname != null), "LocalPathname is null")

    var dirSeqBuilder = Seq.newBuilder[DirAttrs]

    onEachFileOrDirAttrs(
      localPathname,
      mountPoint,
      rule,
      onlyDirs = true,
      maxDepth = maxDepth,
      callback = { dirAttrs =>
        dirSeqBuilder += dirAttrs.asInstanceOf[DirAttrs]
      }

    )

    dirSeqBuilder.result
  }

  def getDirectoryContent(localPathname: String, mountPoint: Option[MountPoint], rule: String): Seq[FileOrDirAttrs] = {
    require((localPathname != null), "LocalPathname is null")

    var fileOrDirSeqBuilder = Seq.newBuilder[FileOrDirAttrs]

    onEachFileOrDirAttrs(
      localPathname,
      mountPoint,
      rule,
      onlyDirs = false,
      callback = { attrs =>
        fileOrDirSeqBuilder += attrs
      }

    )

    fileOrDirSeqBuilder.result
  }

  protected def onEachFileOrDirAttrs(
    localPathname: String,
    mountPoint: Option[MountPoint],
    rule: String,
    onlyDirs: Boolean = false,
    maxDepth: Int = 1,
    callback: FileOrDirAttrs => Unit): Unit = {

    require((localPathname != null), "LocalPathname is null")
    require((mountPoint != null), "MountPoint Option is null")

    require((maxDepth >= 1), "MaxDepth can't be lower than 1")

    logger.debug("LocalPathname \"" + localPathname + "\" | mountPoint " + mountPoint + " | maxDepth: " + maxDepth)

    // Create root dir
    val rootDir = new File(localPathname)
    val rootDirPath = rootDir.toPath

    // Convert rule into a regex
    val regexAsStr = _ruleToRegexAsStr(rule)

    if (!rootDir.isDirectory) {
      logger.debug(s"Unable to open root directory root=${localPathname} !")

    } else {

      //logger.debug( s"Browsing directories from root=${root}" )

      //if( regexAsStr.isDefined ) {
      //  logger.debug( s"filtering directories with regex=${regexAsStr.get}" )
      //}

      class FileAndDirVisitor extends SimpleFileVisitor[Path] {

        override def visitFile(currentPath: Path, attrs: BasicFileAttributes): FileVisitResult = {

          if (onlyDirs && !attrs.isDirectory) {
            return FileVisitResult.CONTINUE
          } else {

            // Retrieve the file name
            val fileName = currentPath.getFileName.toString

            // Apply some filters before sending attributes to the callback
            if ((fileName != ".") && (fileName != "..") &&
              (regexAsStr.isEmpty || fileName.matches(regexAsStr.get))) {

              val lastModifiedTimeInSecs = attrs.lastModifiedTime.to(TimeUnit.SECONDS).toInt

              /* DO NOT return real absolute path to client (try to replace by label) */
              val canonicalPath = currentPath.toAbsolutePath.normalize // Canonicalize path with Path API
              val canonicalPathname = canonicalPath.toString

              var labelPathname: String = canonicalPathname

              if (mountPoint.isDefined) {
                val mp = mountPoint.get

                if (canonicalPathname.startsWith(mp.path)) {
                  /* Replace leftPart by mp.label */

                  val rightPart = canonicalPathname.substring(mp.path.length)

                  val separator = FileSystems.getDefault.getSeparator

                  if (rightPart.startsWith(separator)) {
                    labelPathname = mp.label + rightPart
                    logger.trace("Replace \"" + canonicalPathname + "\" by \"" + labelPathname + '\"')
                  } else {
                    labelPathname = mp.label + separator + rightPart
                    logger.trace("Replace \"" + canonicalPathname + "\" by \"" + labelPathname + '\"')
                  }

                } // End if (canonicalPathname startsWith mp.path)

              }

              val fileOrDirAttrs = if (attrs.isDirectory) {

                new DirAttrs(
                  path = labelPathname,
                  name = fileName,
                  lastmodified = lastModifiedTimeInSecs
                )

              } else {

                new FileAttrs(
                  path = labelPathname,
                  name = fileName,
                  size = attrs.size,
                  lastmodified = lastModifiedTimeInSecs,
                  extension = getFileExtension(fileName)
                )

              }

              callback(fileOrDirAttrs)
            } // End if (file is handled)

            FileVisitResult.CONTINUE
          }

        } // End if (file is not dir)

      } // End inner class definition

      // Instantiate the file visitor
      val visitor = new FileAndDirVisitor

      // Walk the file tree
      val visitOptions = new java.util.HashSet[FileVisitOption]()

      Files.walkFileTree(rootDir.toPath, visitOptions, maxDepth, visitor)
    }

  }

  protected def getFileExtension(fileName: String): String = {
    require(fileName != null, "fileName is null")

    val extIndex = fileName.lastIndexOf(".")

    if (extIndex == -1) ""
    else fileName.substring(extIndex + 1)
  }

  // Convert rule as string into a regex
  private def _ruleToRegex(rule: String): Option[Regex] = {
    this._ruleToRegexAsStr(rule).map(_.r)
  }

  private def _ruleToRegexAsStr(rule: String): Option[String] = {

    if (rule != null && rule.length > 0) {
      if (rule == '*') return None

      Some(rule.replace(".", """\.""").replace("*", """\*"""))
    } else None

  }

}
