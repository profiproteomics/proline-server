package fr.proline.cortex.service.misc

import java.io.File

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.util.StringUtils
import fr.proline.cortex.service.IRemoteService
import fr.proline.cortex.util.FileBrowser
import fr.proline.cortex.util.MountPoint
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.cortex.util.WorkDirectoryRegistry
import fr.proline.cortex.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.util.jsonrpc.ProfiJSONRPC2Response

class FileSystem extends IRemoteService with Logging {

  /* Constants */
  val LABEL_PARAM_NAME = "label"
  val DIRECTORY_TYPE_PARAM_NAME = "dir_type"

  val LABEL_PATH_PARAM_NAME = "label_path"

  /* JMS Service identification */
  val serviceName = "proline/misc/FileSystem"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  /* Define the concrete service method */
  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {
    require((req != null), "Req is null")

    val requestId = req.getID
    val methodName = req.getMethod

    /* Method dispatch */
    methodName match {

      case "retrieve_all_directory_types" => {
        val directoryTypes = MountPointRegistry.retrieveAllDirectoryTypes()

        return new ProfiJSONRPC2Response(directoryTypes, requestId)
      }

      case "retrieve_all_mount_points" => {
        // Do not return real absolute path to client
        val mountPoints = MountPointRegistry.retrieveAllMountPoints(false)

        return new ProfiJSONRPC2Response(mountPoints, requestId)
      }

      case "retrieve_mount_points_by_type" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)

        val directoryType = paramsRetriever.getString(DIRECTORY_TYPE_PARAM_NAME)
        require(!StringUtils.isEmpty(directoryType), "Invalid \"" + DIRECTORY_TYPE_PARAM_NAME + "\" parameter")

        // Do not return real absolute path to client
        val mountPoints = MountPointRegistry.retrieveMountPointsByType(directoryType, false)

        return new ProfiJSONRPC2Response(mountPoints, requestId)
      }

      case "retrieve_mount_points_by_label" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)

        val label = paramsRetriever.getString(LABEL_PARAM_NAME)
        require(!StringUtils.isEmpty(label), "Invalid \"" + LABEL_PARAM_NAME + "\" parameter")

        // Do not return real absolute path to client
        val mountPoints = MountPointRegistry.retrieveMountPointsByType(label, false)

        return new ProfiJSONRPC2Response(mountPoints, requestId)
      }

      case "retrieve_directory_content" => {
        return retrieveDirectoryContent(req)
      }

      // Method name not supported
      case _ => return new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
    }

  }

  private def retrieveDirectoryContent(req: JSONRPC2Request): JSONRPC2Response = {
    val requestId = req.getID
    val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)

    val labelOrPath = paramsRetriever.getString(LABEL_PATH_PARAM_NAME)
    require(!StringUtils.isEmpty(labelOrPath), "Invalid \"" + LABEL_PATH_PARAM_NAME + "\" parameter")

    val directoryType = paramsRetriever.getOptString(DIRECTORY_TYPE_PARAM_NAME, "")

    val directoryTypeHint = if (StringUtils.isEmpty(directoryType)) {
      None
    } else {
      Some(directoryType)
    }

    val includeFiles = paramsRetriever.getBoolean("include_files")
    val includeDirs = paramsRetriever.getBoolean("include_dirs")
    val extensionFilter = paramsRetriever.getOptString("extension_filter", "")

    val localPath = MountPointRegistry.replacePossibleLabel(labelOrPath, directoryTypeHint)

    val dir = new File(localPath.localPathname)

    var foundMountPoint: Option[MountPoint] = localPath.mountPoint

    if (foundMountPoint.isEmpty) {
      foundMountPoint = MountPointRegistry.retrieveMountPointByPath(localPath.localPathname, directoryTypeHint)
    }

    // Send the same error message for the following cases (for security reasons) :
    // non-existing directory, file, unmanaged resource
    val errorMessage = s"directory ${labelOrPath} cannot be found"

    // Check that the directory exists and is not a file
    // TODO: return a specific exception
    require(dir.isDirectory, errorMessage)

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
