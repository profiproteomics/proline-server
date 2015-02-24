package fr.proline.module.exporter.api.formatter

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.template.IViewTemplate
import java.util.UUID

trait IViewFormatter {

  val template: IViewTemplate
  val fileExtension: String = template.fileExtension

  def getViewLocation(viewDir: File, view: IDataView): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    // Create a unique name for this view
    val fileBaseName = view.viewName + '_' + new java.util.Date().getTime() + '.' + fileExtension
    new File(viewDir, fileBaseName)
  }

  def getViewSetLocation(viewDir: File, viewSetName: String): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    viewDir.getAbsoluteFile
  }

  def formatView(view: IDataView, os: OutputStream)

  def formatView(view: IDataView, location: File): File = {

    if (location.isFile) { // isFile => exist && is a normal File
      _formatView(view, location)
      location
    } else {
      val file = getViewLocation(location, view)
      _formatView(view, file)
      file
    }

  }

  protected def _formatView(view: IDataView, outputFile: File) {
    val fop = new FileOutputStream(outputFile)
    formatView(view, fop)
    fop.close()
  }

  /*def formatView( view: IDatasetView, outputDir: File, fileName: String ) {
    require( outputDir.isDirectory == true, "outputDir must be a directory" )
    
    val outputFile = new File( outputDir.getAbsolutePath() + "/" + fileName )
    val fop = new FileOutputStream(outputFile)
    this.formatView( view, fop )
    fop.close()
  }*/

}
