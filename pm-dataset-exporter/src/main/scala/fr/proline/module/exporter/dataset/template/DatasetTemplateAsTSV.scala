package fr.proline.module.exporter.dataset.template

import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.template.AbstractViewSetTSVTemplate

class DatasetTemplateAsTSV(val config: ExportConfig) extends AbstractViewSetTSVTemplate with IDatasetTemplate {
  
  def buildTemplate(config: ExportConfig): Array[ViewTypeWithTemplate] = {

    // Create an TSV template specific to the infos view rows
    config.sheets.map { sheet =>
      sheetToTemplatedViewType(sheet, tsvTemplate)
    }
    
  }

}