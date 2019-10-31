package fr.proline.module.exporter.commons.view

import fr.proline.module.exporter.api.template.IViewSetTemplate
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.commons.config.ExportConfig

class ViewSet(
  var viewSetName: String,
  val viewSetTemplate: IViewSetTemplate,
  val templatedViews: Seq[ViewWithTemplate], 
  val exportConfig: ExportConfig
) extends IViewSet