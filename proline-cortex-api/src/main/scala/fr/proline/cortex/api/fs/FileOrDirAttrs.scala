package fr.proline.cortex.api.fs

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
  extension: String
) extends FileOrDirAttrs {
  val isDir = false
}

case class DirAttrs(
  path: String, // dir path
  name: String,
  lastmodified: Int // mod time since epoch
  //subdirs: collection.Seq[DirProps] = Seq(),
  //files: collection.Seq[FileProps] = Seq()
) extends FileOrDirAttrs {
  val isDir = true
}
