package fr.proline.module.exporter.commons.template

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import org.apache.poi.ss.usermodel.WorkbookFactory
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import org.apache.poi.hssf.usermodel.HSSFCellStyle
import org.apache.poi.hssf.usermodel.HSSFFont
import org.apache.poi.ss.usermodel.Font
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.ss.usermodel.CellStyle
import scala.collection.mutable.Map


class InfoXLSXTemplate(
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
    val headerCellStyleRow1 = wb.createCellStyle();
    headerCellStyleRow1.cloneStyleFrom(defaultCellStyle);
    val fontIndex = headerCellStyleRow1.getFontIndex();
    val headerFont = wb.createFont();
    headerFont.setFontHeightInPoints(defaultfont.getFontHeightInPoints());
    headerFont.setFontName(defaultfont.getFontName());
    headerFont.setBoldweight(Font.BOLDWEIGHT_BOLD);
    headerCellStyleRow1.setFont(headerFont);
    
    // Create an alternate cell style for header of the different rows
    val headerCellStyleRow2 = wb.createCellStyle();
    headerCellStyleRow2.cloneStyleFrom(headerCellStyleRow1);
    headerCellStyleRow2.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex());
    headerCellStyleRow2.setFillPattern(CellStyle.SOLID_FOREGROUND);

    
    // Create a Cell Style for Data
    val dataCellStyleRow1 = wb.createCellStyle();
    dataCellStyleRow1.cloneStyleFrom(defaultCellStyle);
    dataCellStyleRow1.setAlignment(CellStyle.ALIGN_LEFT);

    val dataCellStyleRow2 = wb.createCellStyle();
    dataCellStyleRow2.cloneStyleFrom(dataCellStyleRow1);
    dataCellStyleRow2.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex());
    dataCellStyleRow2.setFillPattern(CellStyle.SOLID_FOREGROUND);

    
    // create an array to control width of columns
    val colSizeMap = scala.collection.mutable.Map[Int,Int]()

    
    // Add header to the worksheet as the first column
    var colIdx = 0;
    var rowIdx = 0
    for (header <- selectedFieldsOrFields) {
      var row = sheet.createRow(rowIdx)
      var cell = row.createCell(colIdx)
      cell.setCellValue(header)
      if (rowIdx % 2 == 0) {
    	  cell.setCellStyle(headerCellStyleRow1);
      } else {
    	  cell.setCellStyle(headerCellStyleRow2);
      }
      
      var l = header.length()
      val size = colSizeMap.get(colIdx)
      if (size.isDefined) {
        if (l>colSizeMap(colIdx)) {
          colSizeMap(colIdx) = l
        }
      } else {
        colSizeMap(colIdx) = l
      }
      
      rowIdx += 1
    }

    
    // Iterate over records to append them to the worksheet
    // Each new record is a new column
    rowIdx = 0
    colIdx = 1;
    view.onEachRecord( record => {

      for (field <- selectedFieldsOrFields) {

        var row = sheet.getRow(rowIdx)
        val cell = row.createCell(colIdx)
        if (rowIdx % 2 == 0) {
          cell.setCellStyle(dataCellStyleRow1);
        } else {
          cell.setCellStyle(dataCellStyleRow2);
        }
        
        val value = record.get(field).flatMap( Option(_) ).getOrElse("")

        // TODO: manage Date, timestamp...
        var l = 12; // default column size
        value match {
          case d: Double => cell.setCellValue(d)
          case num: Number => cell.setCellValue(num.doubleValue)
          case s: String => {
            cell.setCellValue(s)
            l = s.length()
          }
          case a: Any => {
            cell.setCellValue(a.toString)
            l = a.toString.length()
          }
        }

        val size = colSizeMap.get(colIdx)
        if (size.isDefined) {
          if (l > colSizeMap(colIdx)) {
            colSizeMap(colIdx) = l
          }
        } else {
          colSizeMap(colIdx) = l
        }
        
        
        rowIdx += 1
      }
      colIdx +=1

     
    })
    
    // size the columns
    for ((col, nbCharacters) <- colSizeMap) {
      sheet.setColumnWidth(col, nbCharacters*256+256)  // the unit is 1/256 char 
    }
    

    
    // Write result to the file
    val fileOut = new FileOutputStream(workbookLocation)
    wb.write(fileOut)
    
    // Close the file    
    fileOut.close()    
  }

}