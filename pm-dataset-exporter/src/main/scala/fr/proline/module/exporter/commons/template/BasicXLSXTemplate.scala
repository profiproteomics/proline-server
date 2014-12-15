package fr.proline.module.exporter.commons.template

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import org.apache.poi.ss.usermodel.WorkbookFactory
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import org.apache.poi.ss.usermodel.Font
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.ss.usermodel.CellStyle
import com.typesafe.scalalogging.slf4j.Logging

class BasicXLSXTemplate(
  val selectedFields: Option[Seq[String]] = None
) extends IWorksheetTemplate with Logging {

  val fileExtension: String = "xlsx"

  def newWorkbook(workbookLocation: File) {
    val fileOut = new FileOutputStream(workbookLocation)
    val wb = new XSSFWorkbook()
    wb.write(fileOut)
    fileOut.close()
  }

  def newWorksheet(view: IDataView, workbookLocation: File) {

    val selectedFieldsOrFields: Seq[String] = {
      if (selectedFields.isDefined) selectedFields.get
      else view.getFieldsNames
    }

    // Open workbook
    val wb = WorkbookFactory.create(new FileInputStream(workbookLocation))

    // Create worksheet
    val sheet = wb.createSheet(view.viewName)

    // Retrieve Default Cell Style
    var defaultCellStyle = wb.getCellStyleAt(0)
    val defaultFontIndex = defaultCellStyle.getFontIndex()
    val defaultfont = wb.getFontAt(defaultFontIndex)

    // Create a Cell Style for Headers
    val headerCellStyle = wb.createCellStyle()
    headerCellStyle.cloneStyleFrom(defaultCellStyle)
    val fontIndex = headerCellStyle.getFontIndex()
    val headerFont = wb.createFont()
    headerFont.setFontHeightInPoints(defaultfont.getFontHeightInPoints())
    headerFont.setFontName(defaultfont.getFontName())
    headerFont.setBoldweight(Font.BOLDWEIGHT_BOLD)
    headerCellStyle.setFont(headerFont)
    headerCellStyle.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex())
    headerCellStyle.setFillPattern(CellStyle.SOLID_FOREGROUND)

    // create an array to control width of column
    var colSizeArray: Array[Int] = new Array[Int](selectedFieldsOrFields.length)

    // Add header to the worksheet
    var firstRow = sheet.createRow(0)
    var colIdx = 0
    for (header <- selectedFieldsOrFields) {

      var cell = firstRow.createCell(colIdx)
      cell.setCellValue(header)
      cell.setCellStyle(headerCellStyle)

      var l = header.length()
      if (l > colSizeArray(colIdx)) {
        colSizeArray(colIdx) = l
      }

      colIdx += 1
    }

    // Iterate over records to append them to the worksheet
    var rowIdx = 1
    logger.info("Iterates over records ")
    view.onEachRecord(record => {

      var row = sheet.createRow(rowIdx)
      logger.info("createRow: "+rowIdx)

      var colIdx = 0
      for (field <- selectedFieldsOrFields) {
        val cell = row.createCell(colIdx)

        val value = record.get(field).flatMap( Option(_) ).getOrElse("")

        // TODO: manage Date, timestamp...
        var l = 12; // default column size
        value match {
          case d: Double   => cell.setCellValue(d)
          case num: Number => cell.setCellValue(num.doubleValue)
          case s: String => {
            if (!s.isEmpty()){ // blank cell if isEmpty
            	cell.setCellValue(s)
            }
            l = s.length()
          }
          case a: Any => {
            cell.setCellValue(a.toString)
            l = a.toString.length()
          }
        }
        if (l > colSizeArray(colIdx)) {
          colSizeArray(colIdx) = l
        }

        colIdx += 1
      }

      rowIdx += 1
    })

    logger.info("end of iteration ")
    // size columns (they can not exceed MAX_COLUMN_WIDTH_IN_CHARS characters)
    val MAX_COLUMN_WIDTH_IN_CHARS = 30
    for (j <- 0 to colIdx - 1) {
      if (colSizeArray(j) > MAX_COLUMN_WIDTH_IN_CHARS) {
        colSizeArray(j) = MAX_COLUMN_WIDTH_IN_CHARS
      }
      sheet.setColumnWidth(j, colSizeArray(j) * 256 + 256) // the unit is 1/256 char 
    }

    // Write result to the file
    val fileOut = new FileOutputStream(workbookLocation)
    wb.write(fileOut)

    // Close the file
    fileOut.close()
  }

}