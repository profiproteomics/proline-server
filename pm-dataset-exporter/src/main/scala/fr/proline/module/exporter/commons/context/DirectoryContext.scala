package fr.proline.module.exporter.commons.context

import java.io.File
import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.api.template.IFileExtensionEnumeration
import fr.proline.module.exporter.api.view.IDataView

class DirectoryContext(val fileExtension: IFileExtensionEnumeration#Value) extends IViewSetCreationContext {
  
  var viewSetLocation: Option[File] = None
  
  def getViewSetLocation(viewDir: File, viewSetName: String): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    // Create a name for this view set
    new File( viewDir.getAbsolutePath + '/' + viewSetName )
  }
  
  def getViewLocation(viewDir: File, view: IDataView): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    // Create a name for this view
    new File(viewDir, view.viewName + '.' + fileExtension)
  }

  // Nothing to open here
  protected def openOutputResources(viewDir: File, viewSetName: String) {
    val location = getViewSetLocation(viewDir, viewSetName)
    
    // Create view set directory if doesn't exist
    location.mkdir()

    // Set view set location
    viewSetLocation = Some(location)
  }
  
  // Nothing to close here
  protected def closeOutputResources() {
    
  }
  
}