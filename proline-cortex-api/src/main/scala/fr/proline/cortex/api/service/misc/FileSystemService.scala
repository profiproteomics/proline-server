package fr.proline.cortex.api.service.misc

import scala.reflect.runtime.universe.typeOf

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.util.FileAttrs
import fr.proline.cortex.util.MountPoint
import fr.proline.cortex.util.jsonrpc.JSONRPC2DefaultMethod
import fr.proline.cortex.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.cortex.util.jsonrpc.JSONRPC2MethodResult

trait IFileSystemService extends IMiscService {
  
  /* JMS Service identification */
  val serviceName = "FileSystem"
  this.serviceDescription = Some("Provides a remote API to the FileSystem")
  
}

object FileSystemService extends IFileSystemService with IDefaultServiceVersion with Logging {
  
  /* List the handled methods */
  val handledMethods = List(RetrieveAllDirectoryTypes, RetrieveAllMountPoints, RetrieveMountPointsByType, RetrieveMountPointsByLabel, RetrieveDirectoryContent)
  
  /* Constants */
  val LABEL_PARAM_NAME = "label"
  val LABEL_PATH_PARAM_NAME = "label_path"
  val DIRECTORY_TYPE_PARAM_NAME = "dir_type"
  
  object RetrieveAllDirectoryTypes extends JSONRPC2DefaultMethod {    
    val name = "retrieve_all_directory_types"
    val description = "Returns the list of all directory types."
    val parameters = null
    val returns = JSONRPC2MethodResult(typeOf[Array[String]])
  }
  
  object RetrieveAllMountPoints extends JSONRPC2DefaultMethod {    
    val name = "retrieve_all_mount_points"
    val description = "Returns the list of defined mount points."
    val parameters = null
    val returns = JSONRPC2MethodResult(typeOf[Array[MountPoint]])
  }
  
  object RetrieveMountPointsByType extends JSONRPC2DefaultMethod {
    
    val name = "retrieve_mount_points_by_type"
    val description = "Returns the list of mount points corresponding to a given directory type."
    val parameters = List(DIR_TYPE_PARAM)
    val returns = JSONRPC2MethodResult(typeOf[Array[MountPoint]])
      
    // Method parameters definitions
    object DIR_TYPE_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = DIRECTORY_TYPE_PARAM_NAME
      val description = null
      val scalaType = typeOf[String]
    }
    
  }
  
  object RetrieveMountPointsByLabel extends JSONRPC2DefaultMethod {
    
    val name = "retrieve_mount_points_by_label"
    val description = "Returns the list of mount points corresponding to a given label."
    val parameters = List(LABEL_PARAM)
    val returns = JSONRPC2MethodResult(typeOf[Array[MountPoint]])
      
    // Method parameters definitions
    object LABEL_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = LABEL_PARAM_NAME
      val description = null
      val scalaType = typeOf[String]
    }
    
  }
  
  object RetrieveDirectoryContent extends JSONRPC2DefaultMethod {
    
    val name = "retrieve_directory_content"
    val description = "Returns the list of files/directories found in a given managed directory."
    val parameters = List(
      LABEL_PATH_PARAM,
      DIRECTORY_TYPE_PARAM,
      INCLUDE_FILES_PARAM,
      INCLUDE_DIRS_PARAM,
      EXTENSION_FILTER_PARAM
    )
    val returns = JSONRPC2MethodResult(typeOf[Array[FileAttrs]])
      
    // Method parameters definitions
    object LABEL_PATH_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = LABEL_PATH_PARAM_NAME
      val description = "The aliased path of the directory to browse. It has to be prefixed by a registered label."
      val scalaType = typeOf[String]
    }
    
    object DIRECTORY_TYPE_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = DIRECTORY_TYPE_PARAM_NAME
      val description = "The mount point type of the targeted directory."
      val scalaType = typeOf[String]
      optional = true
    }    
    object INCLUDE_FILES_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "include_files"
      val description = "If true, files will be added to the result."
      val scalaType = typeOf[Boolean]
    }    
    object INCLUDE_DIRS_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "include_dirs"
      val description = "If true, directories will be added to the result."
      val scalaType = typeOf[Boolean]
    }    
    object EXTENSION_FILTER_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "extension_filter"
      val description = "A string used to filter files in function of their extension."
      val scalaType = typeOf[String]
      optional = true
    }
    
  }

}
