package fr.proline.module.exporter.dataset.template

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.DatasetViewType._
import fr.proline.module.exporter.commons.template.AbstractViewSetTSVTemplate
import fr.proline.module.exporter.commons.template.TSVTemplate

class DatasetTemplateAsTSV(val config: ExportConfig) extends AbstractViewSetTSVTemplate with IDatasetTemplate {
  
  def buildTemplate(config: ExportConfig): Array[ViewTypeWithTemplate] = {

    // Create an TSV template specific to the infos view rows
    config.sheets.map { sheet =>
      sheetToTemplatedViewType(sheet, tsvTemplate)
    }
    
  }

}