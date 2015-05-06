package fr.proline.module.exporter.commons.config.template

import fr.proline.module.exporter.api.template.IViewSetTemplate
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.TSVTemplate
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.commons.config.view.DatasetViewTypes

class ProlineConfigViewSetTemplateAsTSV(config:ExportConfig) extends IViewSetTemplate with Logging{
val templatedViewTypes: Seq[ViewTypeWithTemplate] = buildTemplate(config)
  
	def buildTemplate(config:ExportConfig):Array[ViewTypeWithTemplate]={
	  // Create a generic XLSX template for views columns
	  val tsvTemplate = new TSVTemplate()
  
	  // Create an TSV template specific to the infos view rows
	  val verticalTsvTemplate = new TSVTemplate()
	  val sheets :Array[ExportConfigSheet] = config.sheets
	  val nbSheets:Int = sheets.size
	  logger.debug("build template from config with "+nbSheets+ " sheets")
	  var templateList : Array[ViewTypeWithTemplate] = new Array(nbSheets)
	  for (i <- 0 to (nbSheets - 1)){
	  	var s :ExportConfigSheet = sheets(i)
	    var template:IViewTemplate = tsvTemplate
	    if (s.presentation.endsWith(ExportConfigConstant.PRESENTATION_SHEET_ROWS)){
	      template = verticalTsvTemplate
	    }
	    var viewType: IViewTypeEnumeration#Value = null 
	    s.id match{
	      case ExportConfigConstant.SHEET_INFORMATION => {
	         viewType = DatasetViewTypes.MSI_SEARCH_EXTENDED
	      }
	      case ExportConfigConstant.SHEET_IMPORT => {
	         viewType = DatasetViewTypes.IMPORT_AND_VALIDATION_PROPS
	      }
	      case ExportConfigConstant.SHEET_PROTEIN_SETS => {
	         viewType = DatasetViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH
	      }
	      case ExportConfigConstant.SHEET_BEST_PSM => {
	         viewType = DatasetViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH
	      }
	      case ExportConfigConstant.SHEET_PROTEIN_MATCH => {
	         viewType = DatasetViewTypes.PROT_SET_TO_PROT_MATCH
	      }
	      case ExportConfigConstant.SHEET_ALL_PSM => {
	         viewType = DatasetViewTypes.PROT_SET_TO_ALL_PEPTIDE_MATCHES
	      }
	      case ExportConfigConstant.SHEET_STAT => {
	         viewType = DatasetViewTypes.STATISTICS
	      }
	      case other => {
	        // should not happen
	      }
	    }
	    var t :ViewTypeWithTemplate = ViewTypeWithTemplate(viewType, template, viewName = Some(s.title))
	    templateList(i) = t
	  }
	  return templateList
	    
	}
	
}