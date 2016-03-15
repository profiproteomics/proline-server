package fr.proline.module.exporter.dataset.template

import com.typesafe.scalalogging.LazyLogging

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.DatasetViewType._
import fr.proline.module.exporter.commons.template.AbstractViewSetTSVTemplate
import fr.proline.module.exporter.commons.template.TSVTemplate

class DatasetTemplateAsTSV(config: ExportConfig) extends AbstractViewSetTSVTemplate with LazyLogging {
  
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = buildTemplate(config)

  def buildTemplate(config: ExportConfig): Array[ViewTypeWithTemplate] = {

    // Create an TSV template specific to the infos view rows
    val sheets = config.sheets
    val nSheets = sheets.length
    logger.debug(s"Build template from config with $nSheets sheets")
    
    val templateList = sheets.map { sheet =>
      
      val viewType = sheet.id match {
        case SHEET_INFORMATION => MSI_SEARCH_EXTENDED
        case SHEET_IMPORT => IMPORT_AND_VALIDATION_PROPS
        case SHEET_PROTEIN_SETS => PROT_SET_TO_TYPICAL_PROT_MATCH
        case SHEET_BEST_PSM => PROT_SET_TO_BEST_PEPTIDE_MATCH
        case SHEET_PROTEIN_MATCH => PROT_SET_TO_PROT_MATCH
        case SHEET_ALL_PSM => PROT_SET_TO_ALL_PEPTIDE_MATCHES
        case SHEET_MASTER_QUANT_PEPTIDE_ION => MASTER_QUANT_PEPTIDE_ION
        case SHEET_STAT => STATISTICS
        case _ => throw new Exception("Invalid sheet id")
      }
      
      ViewTypeWithTemplate(viewType, tsvTemplate, viewName = Some(sheet.title))
    }
    
    templateList
  }

}