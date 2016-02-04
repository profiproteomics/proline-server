package fr.proline.module.exporter.commons.template

import org.apache.poi.ss.usermodel.CellStyle
import org.apache.poi.ss.usermodel.Font
import org.apache.poi.ss.usermodel.IndexedColors

import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.context.WorkbookContext

// TODO: rename to VerticalXLSXTemplate
class InfoXLSXTemplate(
  val selectedFields: Option[Seq[String]] = None
) extends IWorksheetTemplate {
  
  val fileExtension = FileExtensionEnum.XLSX

  def newWorksheet(view: IDataView, workbookCreationContext: WorkbookContext) {
    
    val selectedFieldsOrFields: Seq[String] = {
      if( selectedFields.isDefined ) selectedFields.get
      else view.getFieldsNames
    }
    
    // Retrieve workbook
    val wb = workbookCreationContext.workbook.get
    
    // Create worksheet
    val sheet = wb.createSheet(view.viewName)
    
    // Retrieve Default Cell Style
    val defaultCellStyle = wb.getCellStyleAt(0)
    val defaultFontIndex = defaultCellStyle.getFontIndex()
    val defaultfont = wb.getFontAt(defaultFontIndex)
    
    // Create a Cell Style for Headers
    val headerCellStyleRow1 = wb.createCellStyle()
    headerCellStyleRow1.cloneStyleFrom(defaultCellStyle)
    val fontIndex = headerCellStyleRow1.getFontIndex()
    val headerFont = wb.createFont()
    headerFont.setFontHeightInPoints(defaultfont.getFontHeightInPoints())
    headerFont.setFontName(defaultfont.getFontName())
    headerFont.setBoldweight(Font.BOLDWEIGHT_BOLD)
    headerCellStyleRow1.setFont(headerFont)
    
    // Create an alternate cell style for header of the different rows
    val headerCellStyleRow2 = wb.createCellStyle()
    headerCellStyleRow2.cloneStyleFrom(headerCellStyleRow1)
    headerCellStyleRow2.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex())
    headerCellStyleRow2.setFillPattern(CellStyle.SOLID_FOREGROUND)
    
    // Create a Cell Style for Data
    val dataCellStyleRow1 = wb.createCellStyle()
    dataCellStyleRow1.cloneStyleFrom(defaultCellStyle)
    dataCellStyleRow1.setAlignment(CellStyle.ALIGN_LEFT)

    val dataCellStyleRow2 = wb.createCellStyle()
    dataCellStyleRow2.cloneStyleFrom(dataCellStyleRow1)
    dataCellStyleRow2.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex())
    dataCellStyleRow2.setFillPattern(CellStyle.SOLID_FOREGROUND)
    
    // create an array to control width of columns
    val colSizeMap = scala.collection.mutable.LongMap[Int]()
    
    // Add header to the worksheet as the first column
    var( colIdx, rowIdx ) = ( 0, 0 )
    
    for (header <- selectedFieldsOrFields) {
      val row = sheet.createRow(rowIdx)
      val cell = row.createCell(colIdx)
      cell.setCellValue(header)
      if (rowIdx % 2 == 0) {
    	  cell.setCellStyle(headerCellStyleRow1)
      } else {
    	  cell.setCellStyle(headerCellStyleRow2)
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
    
    view.onEachRecord( record => {
      
      colIdx += 1
      rowIdx = 0

      for (field <- selectedFieldsOrFields) {

        val row = sheet.getRow(rowIdx)
        val cell = row.createCell(colIdx)
        
        if (rowIdx % 2 == 0) {
          cell.setCellStyle(dataCellStyleRow1)
        } else {
          cell.setCellStyle(dataCellStyleRow2)
        }
        
        val colSize = this.setCellValue(cell, record, field)

        val sizeOpt = colSizeMap.get(colIdx)
        if (sizeOpt.isDefined) {
          if (colSize > sizeOpt.get) {
            colSizeMap(colIdx) = colSize
          }
        } else {
          colSizeMap(colIdx) = colSize
        }
        
        rowIdx += 1
      }
     
    })
    
    // size the columns
    for ((col, nbCharacters) <- colSizeMap) {
      sheet.setColumnWidth(col.toInt, (nbCharacters*256+256).min(255*256))  // the unit is 1/256 char, highest value is 255*256 
    }

  }

}