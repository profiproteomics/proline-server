package fr.proline.module.exporter.commons.formatter

import java.io.File

import fr.proline.module.exporter.api.formatter.IViewFormatter
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.context.WorkbookContext
import fr.proline.module.exporter.commons.template.IWorksheetTemplate

class SpreadsheetFormatter(
  val template: IWorksheetTemplate,
  val viewSetCreationContext: WorkbookContext
) extends IViewFormatter {
  
  val outputType = ViewFormatterType.SPREADSHEET

  override def formatView(view: IDataView): File = {

    // Append a new worksheet to the workbook for this view
    template.newWorksheet(view, viewSetCreationContext)

    viewSetCreationContext.viewSetLocation.get
  }

}