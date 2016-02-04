package fr.proline.module.exporter.commons.context

import java.io.File
import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.api.template.IFileExtensionEnumeration
import fr.proline.module.exporter.api.view.IDataView

class DirectoryContext(val fileExtension: IFileExtensionEnumeration#Value) extends IViewSetCreationContext {
  
  var viewSetLocation: Option[File] = None
  
  def getViewSetLocation(viewDir: File, viewSetName: String): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    viewDir.getAbsoluteFile
  }
  
  def getViewLocation(viewDir: File, view: IDataView): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    // Create a unique name for this view
    val fileBaseName = view.viewName + '_' + new java.util.Date().getTime() + '.' + fileExtension
    new File(viewDir, fileBaseName)
  }

  // Nothing to open here
  protected def openOutputResources(viewDir: File, viewSetName: String) {    
    val location = getViewSetLocation(viewDir, viewSetName)

    // Set view set location
    viewSetLocation = Some(location)
  }
  
  // Nothing to close here
  protected def closeOutputResources() {
    
  }
  
}