package fr.proline.module.exporter.commons.context

import java.io.File
import java.io.FileOutputStream

import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.api.template.IFileExtensionEnumeration
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.xssf.streaming.SXSSFWorkbook

class WorkbookContext(val fileExtension: IFileExtensionEnumeration#Value) extends IViewSetCreationContext {
  
  var viewSetLocation: Option[File] = None
  var workbook: Option[Workbook] = None  
  
  def getViewSetLocation(viewDir: File, viewSetName: String): File = {
    require((viewDir != null) && viewDir.isDirectory, "viewDir must be a directory")

    val fileBaseName = viewSetName + '.' + fileExtension
    new File(viewDir, fileBaseName)
  }

  protected def openOutputResources(viewDir: File, viewSetName: String): Unit = {
    
    // Determine output location
    val location = getViewSetLocation(viewDir, viewSetName)
    
    // Set view set location
    viewSetLocation = Some(location)
    
    // Open workbook
    val wb = new SXSSFWorkbook()
    wb.setCompressTempFiles(false)
    workbook = Some(wb)
  }
  
  protected def closeOutputResources(): Unit = {
    
    if(viewSetLocation.isDefined && workbook.isDefined) {
      // Write result to the file
      val fileOut = new FileOutputStream(viewSetLocation.get)
      
      workbook.get.write(fileOut)
  
      // Close the file
      fileOut.close()
    }

  }
}