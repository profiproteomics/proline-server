package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.api.template.IFileExtensionEnumeration

class TSVTemplate(
  val selectedFields: Option[Seq[IViewFieldEnumeration#Value]] = None,
  val fileExtension: IFileExtensionEnumeration#Value = FileExtensionEnum.TSV
) extends ITextTemplate {
  val sepChar: String = "\t"
}