package fr.proline.module.exporter.dataset.view


import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.dataset.DatasetExporter
import fr.proline.module.exporter.commons.config.ExportConfig

class DatasetViewSet (
  var viewSetName: String,
  val templatedViews: Seq[ViewWithTemplate], 
  val exportConfig : ExportConfig
) extends IViewSet {

  lazy val exporters = templatedViews.map { templatedView =>
    new DatasetExporter( templatedView.dataView, templatedView.template, exportConfig )
  }
  
}