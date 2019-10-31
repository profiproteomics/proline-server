package fr.proline.cortex.service.misc

import java.io.File
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.profi.util.jsonrpc.ProfiJSONRPC2Response
import fr.proline.cortex.api.fs.MountPoint
import fr.proline.cortex.api.service.misc.FileSystemServiceConstants.DIRECTORY_TYPE_PARAM_NAME
import fr.proline.cortex.api.service.misc.FileSystemServiceConstants.LABEL_PARAM_NAME
import fr.proline.cortex.api.service.misc.FileSystemServiceConstants.LABEL_PATH_PARAM_NAME
import fr.proline.cortex.api.service.misc.IFileSystemService
import fr.proline.cortex.util.fs.FileBrowser
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.cortex.util.fs.WorkDirectoryRegistry
import fr.proline.jms.service.api.IRemoteJsonRPC2Service
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.cortex.service.SingleThreadIdentifierType

/**
 * Define JMS Service to get information on Proline Server FileSystem.
 *
 *
 * retrieve_all_directory_types :  Return hard-coded Proline directory types (result_files, raw_files, mzdb_files)
 * retrieve_all_mount_points : Return all MountPoints for all Proline directory types and all labels
 * retrieve_mount_points_by_type :  Return all MountPoints for given Proline directory type.
 *   Input Param : dir_type : directory types (result_files, raw_files, mzdb_files) to get Mountpoint for
 * retrieve_mount_points_by_label: Return all MountPoints for given label
 *   Input Param : label : label to get Mountpoint for
 * retrieve_directory_content : Returns the list of files/directories found in a given managed directory.
 *   Input Param :
 *       label_path : The aliased path of the directory to browse. It has to be prefixed by a registered label
 *       dir_type : The mount point type of the targeted directory
 *       include_files : specify if file should be listed
 *       include_dirs : specify if directory should be listed
 *       extension_filter : specify a file extension to use as filter
 *
 */
class FileSystem extends IFileSystemService with IRemoteJsonRPC2Service with ISingleThreadedService with LazyLogging {
  
    /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.SHORT_SERVICES_SINGLETHREAD_IDENT.toString


  /* Define the concrete process method */
  def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require(jsonRequest != null, "jsonRequest is null")

    val requestId = jsonRequest.getID
    val methodName = jsonRequest.getMethod

    /* Method dispatch */
    methodName match {

      case RETRIEVE_ALL_DIRECTORY_TYPES_METHOD.name => {
        val directoryTypes = MountPointRegistry.retrieveAllDirectoryTypes()

        new ProfiJSONRPC2Response(directoryTypes, requestId)
      }

      case RETRIEVE_ALL_MOUNT_POINTS_METHOD.name => {
        // Do not return real absolute path to client
        val mountPoints = MountPointRegistry.retrieveAllMountPoints(false)

        new ProfiJSONRPC2Response(mountPoints, requestId)
      }

      case RETRIEVE_MOUNT_POINTS_BY_TYPE_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        require(paramsRetriever != null, "no parameter specified")
      
        val directoryType = paramsRetriever.getString(DIRECTORY_TYPE_PARAM_NAME)
        require(!StringUtils.isEmpty(directoryType), "Invalid \"" + DIRECTORY_TYPE_PARAM_NAME + "\" parameter")

        // Do not return real absolute path to client
        val mountPoints = MountPointRegistry.retrieveMountPointsByType(directoryType, withAbsolutePath = false)

        new ProfiJSONRPC2Response(mountPoints, requestId)
      }

      case RETRIEVE_MOUNT_POINTS_BY_LABEL_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        require(paramsRetriever != null, "no parameter specified")
      
        val label = paramsRetriever.getString(LABEL_PARAM_NAME)
        require(StringUtils.isNotEmpty(label), "Invalid \"" + LABEL_PARAM_NAME + "\" parameter")

        // Do not return real absolute path to client
        val mountPoints = MountPointRegistry.retrieveMountPointsByLabel(label, withAbsolutePath = false)

        new ProfiJSONRPC2Response(mountPoints, requestId)
      }

      case RETRIEVE_DIRECTORY_CONTENT_METHOD.name => {
        retrieveDirectoryContent(jsonRequest)
      }

      // Method name not supported
      case _ => new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
    }

  }
    
  private def retrieveDirectoryContent(req: JSONRPC2Request): JSONRPC2Response = {
    
    import RETRIEVE_DIRECTORY_CONTENT_METHOD._
    
    val requestId = req.getID
          
    val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)
    require(paramsRetriever != null, "no parameter specified")
      
    val labelOrPath = paramsRetriever.getString(LABEL_PATH_PARAM_NAME)
    require(StringUtils.isNotEmpty(labelOrPath), s"Invalid '$LABEL_PATH_PARAM_NAME' parameter")

    val directoryType = paramsRetriever.getOptString(DIRECTORY_TYPE_PARAM_NAME, "")

    val directoryTypeHint = if (StringUtils.isEmpty(directoryType)) {
      None
    } else {
      Some(directoryType)
    }

    val includeFiles = paramsRetriever.getBoolean(INCLUDE_FILES_PARAM)
    val includeDirs = paramsRetriever.getBoolean(INCLUDE_DIRS_PARAM)
    val extensionFilter = paramsRetriever.getOptString(EXTENSION_FILTER_PARAM, "")

    val localPath = MountPointRegistry.replacePossibleLabel(labelOrPath, directoryTypeHint)
    logger.debug("looking for content of directory: " + localPath.localPathname) 

    val dir = new File(localPath.localPathname)

    var foundMountPoint: Option[MountPoint] = localPath.mountPoint

    if (foundMountPoint.isEmpty) {
      foundMountPoint = MountPointRegistry.retrieveMountPointByPath(localPath.localPathname, directoryTypeHint)
    }

    // Send the same error message for the following cases (for security reasons) :
    // non-existing directory, file, unmanaged resource
    val errorMessage = s"directory $labelOrPath cannot be found"

    // Check that the directory exists and is not a file
    // TODO: return a specific exception
    if (!dir.isDirectory) {
      logger.error(s"Directory ${localPath.localPathname} cannot be found")
      require(dir.isDirectory, errorMessage)
    }

    // Check that this directory is managed by Proline
    // TODO: return a specific exception
    require(WorkDirectoryRegistry.isManagedDirectory(dir), errorMessage)

    // Get the directory content
    val fileOrDirs = FileBrowser.getDirectoryContent(dir.getAbsolutePath, foundMountPoint, extensionFilter)

    val filteredFileOrDirs = fileOrDirs.filter { entry =>
      if (entry.isFile) includeFiles
      else includeDirs
    }

    new ProfiJSONRPC2Response(filteredFileOrDirs, requestId)
  }

}
