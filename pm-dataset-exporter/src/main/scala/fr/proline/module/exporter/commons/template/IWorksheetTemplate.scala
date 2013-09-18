package fr.proline.module.exporter.commons.template

import java.io.File

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.commons.formatter.ViewFormatterTypes

trait IWorksheetTemplate extends IViewTemplate {
  
  final val formatterType = ViewFormatterTypes.SPREADSHEET
  
  def newWorkbook( workbookLocation: File )
  def newWorksheet( view: IDatasetView, workbookLocation: File )
  
}

