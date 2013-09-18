package fr.proline.module.exporter.api.template

import fr.proline.module.exporter.api.formatter.IFormatterTypeEnumeration

trait IViewTemplate {
  
  val formatterType: IFormatterTypeEnumeration#Value
  val fileExtension: String

}