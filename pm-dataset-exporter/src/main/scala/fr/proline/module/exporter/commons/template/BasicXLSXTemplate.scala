package fr.proline.module.exporter.commons.template

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import org.apache.poi.ss.usermodel.WorkbookFactory
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.view.IViewFieldEnumeration

class BasicXLSXTemplate(
  val selectedFields: Option[Seq[IViewFieldEnumeration#Value]] = None
) extends IWorksheetTemplate {
  
  val fileExtension: String = "xlsx"
  
  def newWorkbook( workbookLocation: File ) {    
    val fileOut = new FileOutputStream(workbookLocation)
    val wb = new XSSFWorkbook()
    wb.write(fileOut)
    fileOut.close()
  }
  
  def newWorksheet( view: IDatasetView, workbookLocation: File ) {
    
    val selectedFieldsOrFields: Seq[String] = {
      if( selectedFields.isDefined ) selectedFields.get.map(_.toString)
      else view.fields.values.toSeq.map(_.toString)
    }
    
    // Open workbook
    val wb = WorkbookFactory.create( new FileInputStream(workbookLocation) )
    
    // Create worksheet
    val sheet = wb.createSheet(view.viewName)
    
    // Add header to the worksheet
    var firstRow = sheet.createRow(0)
    var colIdx = 0
    for (header <- selectedFieldsOrFields) {
      firstRow.createCell(colIdx).setCellValue(header)
      colIdx += 1
    }

    
    // Iterate over records to append them to the worksheet
    var rowIdx = 1
    view.onEachRecord( record => {
      
      var row = sheet.createRow(rowIdx)
      
      var colIdx = 0
      for( field <- selectedFieldsOrFields ) {   
        val cell =  row.createCell(colIdx)
        val value = Option(record(field)).getOrElse("")
        
        // TODO: manage Date, timestamp...
        value match {
          case d: Double => cell.setCellValue(d)
          case num: Number => cell.setCellValue(num.doubleValue)
          case s: String => cell.setCellValue(s)
          case a: Any => cell.setCellValue(a.toString)
        }
      
        colIdx += 1
      }

      rowIdx += 1
    })
    
    // Write result to the file
    val fileOut = new FileOutputStream(workbookLocation)
    wb.write(fileOut)
    
    // Close the file    
    fileOut.close()    
  }

}