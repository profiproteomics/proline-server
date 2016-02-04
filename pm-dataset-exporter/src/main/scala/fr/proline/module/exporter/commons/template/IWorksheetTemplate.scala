package fr.proline.module.exporter.commons.template

import org.apache.poi.ss.usermodel.Cell

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.context.WorkbookContext
import fr.proline.module.exporter.commons.formatter.ViewFormatterType

trait IWorksheetTemplate extends IViewTemplate {
  
  final val formatterType = ViewFormatterType.SPREADSHEET
  
  def newWorksheet(view: IDataView, workbookCreationContext: WorkbookContext)
  
  protected def setCellValue(cell: Cell, record: Map[String, Any], field: String): Int = {
    
    val value = record.get(field).flatMap( Option(_) ).getOrElse("")

    // TODO: manage Date, timestamp...
    var colSize = 12 // default column size
    value match {
      case f: Float   => cell.setCellValue(f)
      case d: Double   => cell.setCellValue(d)
      case num: Number => cell.setCellValue(num.doubleValue)
      case s: String => {
        if (!s.isEmpty()){ // blank cell if isEmpty
        	cell.setCellValue(s)
        }
        colSize = s.length()
      }
      case a: Any => {
        cell.setCellValue(a.toString)
        colSize = a.toString.length()
      }
    }
    
    colSize
  }
}

