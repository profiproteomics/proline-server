package fr.proline.module.exporter.api

import java.io.File
import java.io.OutputStream
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.formatter.IViewFormatter
import fr.proline.module.exporter.commons.config.ExportConfig

// TODO: rename into DataExporter
trait IDatasetExporter {

  val dataView: IDataView
  val template: IViewTemplate
  val formatter: IViewFormatter
  val exportConfig : ExportConfig
  
  def exportViewToStream( outputStream: OutputStream )
  
  /**
   * Returns the file containing the exported view
   */
  def exportViewToLocation( location: File ): File
  
}