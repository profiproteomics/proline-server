package fr.proline.module.exporter.api.formatter

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream

import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.api.template.IFileExtensionEnumeration
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView

trait IViewFormatter {

  val template: IViewTemplate
  val outputType: IFormatterTypeEnumeration#Value
  val fileExtension: IFileExtensionEnumeration#Value = template.fileExtension
  val viewSetCreationContext: IViewSetCreationContext

  def formatView(view: IDataView): File

}
