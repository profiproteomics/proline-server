package fr.proline.module.exporter.api

import java.io.File
import java.io.OutputStream
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.formatter.IViewFormatter

trait IDatasetExporter {

  val datasetView: IDatasetView
  val template: IViewTemplate
  val formatter: IViewFormatter
  
  def exportViewToStream( outputStream: OutputStream )
  
  /**
   * Returns the file containing the exported view
   */
  def exportViewToLocation( location: File ): File
  
}