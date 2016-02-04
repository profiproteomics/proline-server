package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.commons.formatter.ViewFormatterType

trait ITextTemplate extends IViewTemplate {
  val selectedFields: Option[Seq[IViewFieldEnumeration#Value]]
  val sepChar: String
  
  final val formatterType = ViewFormatterType.TEXT
}

