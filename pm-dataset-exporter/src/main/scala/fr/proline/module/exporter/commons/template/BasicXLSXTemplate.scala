package fr.proline.module.exporter.commons.template

import com.typesafe.scalalogging.LazyLogging
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.context.WorkbookContext
import org.apache.poi.ss.usermodel.FillPatternType
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.xssf.streaming.SXSSFSheet

class BasicXLSXTemplate(
  val selectedFields: Option[Seq[String]] = None
) extends IWorksheetTemplate with LazyLogging {

  val fileExtension = FileExtensionEnum.XLSX
  
  def newWorksheet(view: IDataView, workbookCreationContext: WorkbookContext) {
    
    val selectedFieldsOrFields: Seq[String] = {
      if (selectedFields.isDefined) selectedFields.get
      else view.getFieldsNames
    }
    logger.debug("Export View with "+selectedFieldsOrFields.length+" columns ")

    // Retrieve workbook
    val wb = workbookCreationContext.workbook.get

    // Create worksheet
    val sheet = wb.createSheet(view.viewName)
    sheet match {
      case sheetAsStream: SXSSFSheet => {
        sheetAsStream.setRandomAccessWindowSize(10)
      }
    }

    // Retrieve Default Cell Style
    val defaultCellStyle = wb.getCellStyleAt(0)
    val defaultFontIndex = defaultCellStyle.getFontIndex()
    val defaultfont = wb.getFontAt(defaultFontIndex)

    // Create a Cell Style for Headers
    val headerCellStyle = wb.createCellStyle()
    headerCellStyle.cloneStyleFrom(defaultCellStyle)
    val headerFont = wb.createFont()
    headerFont.setFontHeightInPoints(defaultfont.getFontHeightInPoints())
    headerFont.setFontName(defaultfont.getFontName())
    headerFont.setBold(true)
    headerCellStyle.setFont(headerFont)
    headerCellStyle.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex())
    headerCellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND)

    // create an array to control width of column
    val colSizeArray: Array[Int] = new Array[Int](selectedFieldsOrFields.length)
    
    // Add header to the worksheet
    val firstRow = sheet.createRow(0)
    var colIdx = 0
    for (header <- selectedFieldsOrFields) {

      val cell = firstRow.createCell(colIdx)
      cell.setCellValue(header)
      cell.setCellStyle(headerCellStyle)

      val l = header.length()
      if (l > colSizeArray(colIdx)) {
        colSizeArray(colIdx) = l
      }

      colIdx += 1
    }
    
    // Iterate over records to append them to the worksheet
    var rowIdx = 1
    view.formatView { record =>

      val row = sheet.createRow(rowIdx)

      var colIdx = 0
      for (field <- selectedFieldsOrFields) {
        val cell = row.createCell(colIdx)

        val colSize = this.setCellValue(cell, record, field)
        
        if (colSize > colSizeArray(colIdx)) {
          colSizeArray(colIdx) = colSize
        }

        colIdx += 1
      }

      rowIdx += 1
      if(rowIdx >= maxRowsNbr)
        throw new RuntimeException("Too much rows in "+view.viewName+". Can't export to xlsx format !")
    }
    logger.debug("Export View with "+rowIdx+" rows")

    // size columns (they can not exceed MAX_COLUMN_WIDTH_IN_CHARS characters)
    val MAX_COLUMN_WIDTH_IN_CHARS = 30
    for (j <- 0 until colIdx) {
      
      val colSize = if (colSizeArray(j) > MAX_COLUMN_WIDTH_IN_CHARS) MAX_COLUMN_WIDTH_IN_CHARS
      else colSizeArray(j)
      
      sheet.setColumnWidth(j, colSize * 256 + 256) // the unit is 1/256 char 
    }

  }

}