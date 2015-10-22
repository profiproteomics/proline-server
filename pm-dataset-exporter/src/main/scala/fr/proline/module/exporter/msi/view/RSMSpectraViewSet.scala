package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.commons.config.ExportConfig
import  fr.proline.module.exporter.msi.RSMSpectraExporter

/**
 * @author VD225637
 */
class RSMSpectraViewSet(var viewSetName: String,
  val templatedViews: Seq[ViewWithTemplate],
  val exportConfig: ExportConfig) extends IViewSet {

  lazy val exporters = templatedViews.map { templatedView =>
    new RSMSpectraExporter(templatedView.dataView, templatedView.template, exportConfig)
  }
}