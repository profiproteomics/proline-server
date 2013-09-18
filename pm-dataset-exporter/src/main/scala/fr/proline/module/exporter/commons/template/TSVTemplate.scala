package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.view.IViewFieldEnumeration

class TSVTemplate(
  val selectedFields: Option[Seq[IViewFieldEnumeration#Value]] = None,
  val fileExtension: String = "tsv"
) extends ITextTemplate {
  val sepChar: String = "\t"
}