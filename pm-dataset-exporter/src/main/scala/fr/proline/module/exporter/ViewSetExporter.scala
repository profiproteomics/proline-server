package fr.proline.module.exporter

import java.io.File
import fr.proline.module.exporter.api.view.IViewSet
import fr.proline.module.exporter.commons.formatter.BuildViewFormatter

object ViewSetExporter {

  /**
   * Returns the list of exported files.
   */
  def exportViewSetToDirectory(viewSet: IViewSet, outputDir: File): Seq[File] = {

    // Create the directory of it doesn't exists
    if (!outputDir.exists) {
      outputDir.mkdir()
    }
    
    val viewSetCreationContext = viewSet.viewSetTemplate.viewSetCreationContext
    
    viewSetCreationContext.formatViewSet(outputDir, viewSet.viewSetName) { () =>
      
      // Iterate over each view templated view
      viewSet.templatedViews.map { templatedView =>
        val formatter = BuildViewFormatter(templatedView.template, viewSetCreationContext)      
        formatter.formatView(templatedView.dataView)
      }.distinct
      
    }


  }

  /*def exportViewsToArchive( viewSetName: String, exporters: Seq[XDatasetExporter], archiveFile: File ) {

    
  }*/
}
