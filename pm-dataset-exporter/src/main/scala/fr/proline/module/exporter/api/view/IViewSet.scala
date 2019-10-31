package fr.proline.module.exporter.api.view

import fr.proline.module.exporter.api.template.IViewSetTemplate
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.commons.config.ExportConfig

trait IViewSet {
  
  var viewSetName: String
  val viewSetTemplate: IViewSetTemplate
  val templatedViews: Seq[ViewWithTemplate]
  val exportConfig: ExportConfig
  
}