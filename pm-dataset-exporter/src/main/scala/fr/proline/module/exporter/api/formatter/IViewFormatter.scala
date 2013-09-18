package fr.proline.module.exporter.api.formatter

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.template.IViewTemplate

trait IViewFormatter {
  
  val template: IViewTemplate
  val fileExtension: String = template.fileExtension
  
  def getViewLocation( viewDir: File, view: IDatasetView ): File = {
    require( viewDir.isDirectory, "viewDir must be a directory" )
    new File( viewDir.getAbsolutePath() + "/" + view.viewName + "." + fileExtension )
  }
  def getViewSetLocation( viewDir: File, viewSetName: String ): File = {
    new File( viewDir.getAbsolutePath() )
  }
  
  def formatView( view: IDatasetView, os: OutputStream )
  
  def formatView( view: IDatasetView, location: File ) {
    
    if( location.isFile() || location.exists() == false ) {
      _formatView( view, location )
    } else {
      val file = this.getViewLocation( location, view )
      _formatView( view, file )
    }

  }
  
  protected def _formatView( view: IDatasetView, outputFile: File ) {
    val fop = new FileOutputStream(outputFile)
    this.formatView( view, fop )
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