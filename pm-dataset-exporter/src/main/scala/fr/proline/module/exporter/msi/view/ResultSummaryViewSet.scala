package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.msi.ResultSummaryExporter

class ResultSummaryViewSet(
  var viewSetName: String,
  val templatedViews: Seq[ViewWithTemplate]
) extends IViewSet {

  lazy val exporters = templatedViews.map { templatedView =>
    new ResultSummaryExporter( templatedView.datasetView, templatedView.template )
  }
  
}