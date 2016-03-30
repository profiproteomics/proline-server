package fr.proline.module.exporter.dataset.template

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.DatasetViewType._
import fr.proline.module.exporter.commons.template.AbstractViewSetXLSXTemplate
import fr.proline.module.exporter.commons.template.InfoXLSXTemplate

class DatasetTemplateAsXLSX(val config: ExportConfig) extends AbstractViewSetXLSXTemplate with IDatasetTemplate {
  
  def buildTemplate(config: ExportConfig): Array[ViewTypeWithTemplate] = {

    // Create an XLSX template specific to the infos view rows
    lazy val verticalXlsxTemplate = new InfoXLSXTemplate()
   
    config.sheets.map { sheet =>
      
      // TODO: DBO => why use endsWith and not equals ?
      val template = if (sheet.presentation.endsWith(PRESENTATION_SHEET_ROWS))
        verticalXlsxTemplate
      else
        xlsxTemplate

      this.sheetToTemplatedViewType(sheet, template)
    }

  }

}