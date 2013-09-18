package fr.proline.module.exporter.api.template

import fr.proline.module.exporter.api.view.IViewTypeEnumeration

case class ViewTypeWithTemplate(
  viewType: IViewTypeEnumeration#Value,
  template: IViewTemplate,
  viewName: Option[String] = None
)