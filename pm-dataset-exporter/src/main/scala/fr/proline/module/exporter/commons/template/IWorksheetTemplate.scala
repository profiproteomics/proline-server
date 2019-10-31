package fr.proline.module.exporter.commons.template

import org.apache.poi.ss.usermodel.Cell

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.context.WorkbookContext
import fr.proline.module.exporter.commons.formatter.ViewFormatterType

trait IWorksheetTemplate extends IViewTemplate {
  
  final val formatterType = ViewFormatterType.SPREADSHEET
  final val maxCellSize = 32767
  final val maxRowsNbr = 1048576
  final val maxColsNbr = 16384


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
        colSize = s.length()
        if (!s.isEmpty){ // blank cell if isEmpty
          val cellVal = if(colSize>=maxCellSize) { s.substring(0,maxCellSize-3).concat("...")} else s
        	cell.setCellValue(cellVal)
        }

      }
      case a: Any => {
        cell.setCellValue(a.toString)
        colSize = a.toString.length()
      }
    }
    
    colSize
  }
}

