package fr.proline.module.exporter.commons

import java.io.File
import java.io.OutputStream

import fr.proline.module.exporter.api.IDatasetExporter
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.commons.formatter.BuildViewFormatter

trait XDatasetExporter extends IDatasetExporter {
  
  val formatter = BuildViewFormatter( template )

  def exportViewToStream( outputStream: OutputStream ) {
    formatter.formatView(dataView, outputStream)  
  }
  
  def exportViewToLocation( location: File ): File = {
    formatter.formatView(dataView, location )
  }
  
  /*def exportViewToDirectory( outputDir: File ) {
    formatter.formatView(datasetView, outputDir )
  }*/
  
}