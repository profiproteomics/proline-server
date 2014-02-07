package fr.proline.module.exporter.commons.template

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import org.apache.poi.ss.usermodel.WorkbookFactory
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import org.apache.poi.ss.usermodel.Font
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.ss.usermodel.CellStyle

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
    
    
    // Retrieve Default Cell Style
    var defaultCellStyle = wb.getCellStyleAt(0);
    val defaultFontIndex = defaultCellStyle.getFontIndex();
    val defaultfont = wb.getFontAt(defaultFontIndex);
    
    // Create a Cell Style for Headers
    val headerCellStyle = wb.createCellStyle();
    headerCellStyle.cloneStyleFrom(defaultCellStyle);
    val fontIndex = headerCellStyle.getFontIndex();
    val headerFont = wb.createFont();
    headerFont.setFontHeightInPoints(defaultfont.getFontHeightInPoints());
    headerFont.setFontName(defaultfont.getFontName());
    headerFont.setBoldweight(Font.BOLDWEIGHT_BOLD);
    headerCellStyle.setFont(headerFont);
    headerCellStyle.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex());
    headerCellStyle.setFillPattern(CellStyle.SOLID_FOREGROUND);
    
    // Create a Cell Style for Data
    /*val dataCellStyleRow1 = wb.createCellStyle();
    dataCellStyleRow1.cloneStyleFrom(defaultCellStyle);
    val dataCellStyleRow2 = wb.createCellStyle();
    dataCellStyleRow2.cloneStyleFrom(dataCellStyleRow1);
    dataCellStyleRow2.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex());
    dataCellStyleRow2.setFillPattern(CellStyle.SOLID_FOREGROUND);*/
    
    
    // Add header to the worksheet
    var firstRow = sheet.createRow(0)
    var colIdx = 0
    for (header <- selectedFieldsOrFields) {
      
      var cell = firstRow.createCell(colIdx)
      cell.setCellValue(header)
      cell.setCellStyle(headerCellStyle);

      colIdx += 1
    }

    // create an array to control width of column
    var colSizeArray:Array[Int] = new Array[Int](colIdx)
    
    // Iterate over records to append them to the worksheet
    var rowIdx = 1
    view.onEachRecord( record => {
      
      var row = sheet.createRow(rowIdx)
      
      var colIdx = 0
      for( field <- selectedFieldsOrFields ) {   
        val cell =  row.createCell(colIdx)

        /*if (rowIdx % 2 == 0) {
          cell.setCellStyle(dataCellStyleRow1);
        } else {
          cell.setCellStyle(dataCellStyleRow2);
        }*/
        
        val value = Option(record(field)).getOrElse("")
        
        // TODO: manage Date, timestamp...
        value match {
          case d: Double => cell.setCellValue(d)
          case num: Number => cell.setCellValue(num.doubleValue)
          case s: String => {
            cell.setCellValue(s)
            var l = s.length()
            if (l>colSizeArray(colIdx)) {
              colSizeArray(colIdx) = l;
            }
          }
          case a: Any => cell.setCellValue(a.toString)
        }
      
        colIdx += 1
      }

      rowIdx += 1
    })
    
    // Auto size columns except when columns would be too large
    val MAX_COLUMN_WIDTH_IN_CHARS = 30
    for (j <- 0 to colIdx-1) {
      if (colSizeArray(j)<MAX_COLUMN_WIDTH_IN_CHARS) {
    	sheet.autoSizeColumn(j);
      } else {
        sheet.setColumnWidth(j, MAX_COLUMN_WIDTH_IN_CHARS*256)  // the unit is 1/256 char 
      }
    }
    
    
    // Write result to the file
    val fileOut = new FileOutputStream(workbookLocation)
    wb.write(fileOut)
    
    // Close the file    
    fileOut.close()    
  }

}