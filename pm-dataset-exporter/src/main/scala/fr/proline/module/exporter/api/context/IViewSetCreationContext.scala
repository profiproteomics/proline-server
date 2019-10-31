package fr.proline.module.exporter.api.context

import java.io.File

import fr.proline.module.exporter.api.template.IFileExtensionEnumeration

trait IViewSetCreationContext {
  
  def fileExtension: IFileExtensionEnumeration#Value
  
  def viewSetLocation: Option[File]
  def getViewSetLocation(viewDir: File, viewSetName: String): File
  
  def formatViewSet[T](viewDir: File, viewSetName: String)(exporterFn: () => T): T = {
    this.openOutputResources(viewDir, viewSetName)
    
    val result = exporterFn()
    
    this.closeOutputResources()
    
    result
  }
  
  /*private var viewSetLocation: Option[File] = None  
  def getViewSetLocation(): Option[File] = viewSetLocation
  def setViewSetLocation(viewDir: File, viewSetName: String) = {
    viewSetLocation = Some(_getViewSetLocation(viewDir,viewSetName))
  }*/
  
  /*def getViewLocation(viewDir: File, view: IDataView): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    // Create a unique name for this view
    val fileBaseName = view.viewName + '_' + new java.util.Date().getTime() + '.' + fileExtension
    new File(viewDir, fileBaseName)
  }
  
  def getViewSetLocation(viewDir: File, viewSetName: String): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    viewDir.getAbsoluteFile
  }*/
  
  /** Open resources associated to this context **/
  protected def openOutputResources(viewDir: File, viewSetName: String)
  
  /** Close resources associated to this context **/
  protected def closeOutputResources()
}