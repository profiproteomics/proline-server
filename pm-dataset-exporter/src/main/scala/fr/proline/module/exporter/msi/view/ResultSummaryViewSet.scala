package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.msi.ResultSummaryExporter
import fr.proline.module.exporter.commons.config.ExportConfig

class ResultSummaryViewSet(
  var viewSetName: String,
  val templatedViews: Seq[ViewWithTemplate], 
  val exportConfig : ExportConfig
) extends IViewSet {

  lazy val exporters = templatedViews.map { templatedView =>
    new ResultSummaryExporter( templatedView.dataView, templatedView.template, exportConfig )
  }
  
}