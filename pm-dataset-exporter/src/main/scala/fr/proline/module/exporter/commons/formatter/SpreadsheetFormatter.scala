package fr.proline.module.exporter.commons.formatter

import java.io.File
import java.io.OutputStream
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.api.formatter.IViewFormatter
import fr.proline.module.exporter.commons.template.IWorksheetTemplate

class SpreadsheetFormatter(
  val template: IWorksheetTemplate) extends IViewFormatter {

  override def getViewSetLocation(viewDir: File, viewSetName: String): File = {
    require(((viewDir != null) && viewDir.isDirectory), "viewDir must be a directory")

    val fileBaseName = viewSetName + '.' + fileExtension
    new File(viewDir, fileBaseName)
  }

  def formatView(view: IDatasetView, os: OutputStream) {
    throw new Exception("not yet implemented")
  }

  // TODO: use Apache POI or equivalent to export some views
  override def formatView(view: IDatasetView, location: File): File = {
    require(location.isDirectory == false, "location must be a file")

    // Create the workbook if it doesn't exist
    if (location.exists() == false) {
      template.newWorkbook(location)
    }

    // Append a new worksheet to the workbook for this view
    template.newWorksheet(view, location)

    location
  }

}