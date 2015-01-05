package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes
import fr.proline.module.exporter.commons.template.BasicXLSXTemplate
import fr.proline.module.exporter.commons.template.InfoXLSXTemplate

object IRMaLikeViewSetTemplateAsXLSX extends IViewSetTemplate {

  // Create a generic XLSX template for views
  private val xlsxTemplate = new BasicXLSXTemplate()
  
  // Create an XLSX template specific to the infos view
  private val infoXlsxTemplate = new InfoXLSXTemplate()
  
  
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
	ViewTypeWithTemplate( ResultSummaryViewTypes.MSI_SEARCH_EXTENDED, infoXlsxTemplate, viewName = Some("search settings and infos") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS, infoXlsxTemplate, viewName = Some("import and filters") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH, xlsxTemplate, viewName = Some("protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH, xlsxTemplate, viewName = Some("best PSM from protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH, xlsxTemplate, viewName = Some("protein matches in protein set ") ),   
    ViewTypeWithTemplate( ResultSummaryViewTypes.STATISTICS, infoXlsxTemplate, viewName = Some("statistics") )
  )
}