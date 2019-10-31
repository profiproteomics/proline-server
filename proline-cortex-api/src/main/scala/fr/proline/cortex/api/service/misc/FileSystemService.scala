package fr.proline.cortex.api.service.misc

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import FileSystemServiceConstants.DIRECTORY_TYPE_PARAM_NAME
import FileSystemServiceConstants.LABEL_PARAM_NAME
import FileSystemServiceConstants.LABEL_PATH_PARAM_NAME
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult
import fr.proline.cortex.api.fs.FileAttrs
import fr.proline.cortex.api.fs.MountPoint
import fr.proline.jms.service.api.IDefaultServiceVersion

object FileSystemServiceConstants {
  val LABEL_PARAM_NAME = "label"
  val LABEL_PATH_PARAM_NAME = "label_path"
  val DIRECTORY_TYPE_PARAM_NAME = "dir_type"
}

object FileSystemService extends IFileSystemService

trait IFileSystemService extends IMiscService with IDefaultServiceVersion {
  
  
  /* JMS Service identification */
  val serviceLabel = "FileSystem"
  this.serviceDescription = Some("Provides a remote API to the FileSystem.")
  
  /* List the handled methods */
  val methodDefinitions = List(
    RETRIEVE_ALL_DIRECTORY_TYPES_METHOD,
    RETRIEVE_ALL_MOUNT_POINTS_METHOD,
    RETRIEVE_MOUNT_POINTS_BY_TYPE_METHOD,
    RETRIEVE_MOUNT_POINTS_BY_LABEL_METHOD,
    RETRIEVE_DIRECTORY_CONTENT_METHOD
  )
  
  object RETRIEVE_ALL_DIRECTORY_TYPES_METHOD extends JSONRPC2DefaultMethod {    
    val name = "retrieve_all_directory_types"
    val description = "Return hard-coded Proline directory types (result_files, raw_files, mzdb_files)."
    val parameters = null
    val returns = JSONRPC2MethodResult(typeOf[Array[String]])
  }
  
  object RETRIEVE_ALL_MOUNT_POINTS_METHOD extends JSONRPC2DefaultMethod {    
    val name = "retrieve_all_mount_points"
    val description = "Returns the list of defined mount points for all defined directory types and labels."
    val parameters = null
    val returns = JSONRPC2MethodResult(typeOf[Array[MountPoint]])
  }
  
  object RETRIEVE_MOUNT_POINTS_BY_TYPE_METHOD extends JSONRPC2DefaultMethod {
    
    val name = "retrieve_mount_points_by_type"
    val description = "Returns the list of mount points corresponding to a given directory type."
    val parameters = List(DIR_TYPE_PARAM)
    val returns = JSONRPC2MethodResult(typeOf[Array[MountPoint]])
      
    // Method parameters definitions
    object DIR_TYPE_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = DIRECTORY_TYPE_PARAM_NAME
      val description = "Valid directory types are: result_files, raw_files, mzdb_files."
      val scalaType = typeOf[String]
    }
    
  }
  
  object RETRIEVE_MOUNT_POINTS_BY_LABEL_METHOD extends JSONRPC2DefaultMethod {
    
    val name = "retrieve_mount_points_by_label"
    val description = "Returns the list of mount points corresponding to a given label."
    val parameters = List(LABEL_PARAM)
    val returns = JSONRPC2MethodResult(typeOf[Array[MountPoint]])
      
    // Method parameters definitions
    object LABEL_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = LABEL_PARAM_NAME
      val description = "The label that will be used to search for corresponding mount points."
      val scalaType = typeOf[String]
    }
    
  }
  
  object RETRIEVE_DIRECTORY_CONTENT_METHOD extends JSONRPC2DefaultMethod {
    
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
