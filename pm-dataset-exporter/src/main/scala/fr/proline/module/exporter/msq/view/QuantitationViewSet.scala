package fr.proline.module.exporter.msq.view

import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.msq.QuantitationExporter

class QuantitationViewSet(
  var viewSetName: String,
  val templatedViews: Seq[ViewWithTemplate]
) extends IViewSet {

  lazy val exporters = templatedViews.map { templatedView =>
    new QuantitationExporter( templatedView.dataView, templatedView.template )
  }
  
}