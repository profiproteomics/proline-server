package fr.proline.module.exporter.api.view

import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.api.IDatasetExporter
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.commons.config.ExportConfig

trait IViewSet {
  
  var viewSetName: String
  val templatedViews: Seq[ViewWithTemplate]
  val exportConfig : ExportConfig
  
  def exporters: Seq[IDatasetExporter]
}