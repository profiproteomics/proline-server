package fr.proline.module.exporter.commons

import java.io.File
import fr.proline.module.exporter.api.view.IViewSet

object ViewSetExporter {

  /**
   * Returns the list of exported files.
   */
  def exportViewSetToDirectory(viewSet: IViewSet, outputDir: File): Seq[File] = {

    // Create the directory of it doesn't exists
    if (!outputDir.exists) {
      outputDir.mkdir()
    }

    // Iterate over each view exporter
    viewSet.exporters.map { exporter =>
      val viewSetLocation = exporter.formatter.getViewSetLocation(outputDir, viewSet.viewSetName)
      exporter.exportViewToLocation(viewSetLocation)
    } distinct

  }

  /*def exportViewsToArchive( viewSetName: String, exporters: Seq[XDatasetExporter], archiveFile: File ) {

    
  }*/
}
