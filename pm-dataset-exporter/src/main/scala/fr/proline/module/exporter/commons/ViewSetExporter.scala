package fr.proline.module.exporter.commons

import java.io.File
import fr.proline.module.exporter.api.view.IViewSet

object ViewSetExporter {

  def exportViewSetToDirectory( viewSet: IViewSet, outputDir: File ) {
    
    // Create the directory of it doesn't exists
    if( outputDir.exists() == false ) outputDir.mkdir()
    
    // Iterate over each view exporter
    viewSet.exporters.foreach { exporter =>
      val viewSetLocation = exporter.formatter.getViewSetLocation(outputDir, viewSet.viewSetName )
      exporter.exportViewToLocation( viewSetLocation )
    }
  }

  /*def exportViewsToArchive( viewSetName: String, exporters: Seq[XDatasetExporter], archiveFile: File ) {

    
  }*/
}