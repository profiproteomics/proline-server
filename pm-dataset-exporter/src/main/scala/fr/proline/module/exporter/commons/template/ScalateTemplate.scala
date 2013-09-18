package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.commons.formatter.ViewFormatterTypes

class ScalateTemplate( val URI: String, val fileExtension: String ) extends IViewTemplate {
  
  final val formatterType = ViewFormatterTypes.SCALATE

}