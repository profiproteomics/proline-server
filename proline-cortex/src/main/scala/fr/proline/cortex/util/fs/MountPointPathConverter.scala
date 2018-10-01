package fr.proline.cortex.util.fs

import fr.proline.core.om.provider.IProlinePathConverter
import fr.proline.core.om.provider.ProlineManagedDirectoryType

object MountPointPathConverter extends IProlinePathConverter {

  def prolinePathToAbsolutePath(prolineResourcePath: String, dirType: ProlineManagedDirectoryType.Value): String = {    
    MountPointRegistry.replacePossibleLabel( prolineResourcePath, Some( dirType.toString.toLowerCase() ) ).localPathname
  }


}