package fr.proline.cortex.api.fs

import fr.profi.util.StringUtils

case class MountPoint(
  val directoryType: String,
  val label: String,
  val path: String
) {

  /* Constructor checks */
  require(!StringUtils.isEmpty(directoryType), "Invalid directoryType")

  require(!StringUtils.isEmpty(label), "Invalid label")

}
