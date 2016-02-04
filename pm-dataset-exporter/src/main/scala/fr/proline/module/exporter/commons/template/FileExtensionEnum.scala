package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.template.IFileExtensionEnumeration

object FileExtensionEnum extends IFileExtensionEnumeration {
  val TSV = Value("tsv")
  val XLSX = Value("xlsx")
}