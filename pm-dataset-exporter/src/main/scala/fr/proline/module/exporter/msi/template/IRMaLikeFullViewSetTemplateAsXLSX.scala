package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.template.AbstractViewSetXLSXTemplate
import fr.proline.module.exporter.commons.template.InfoXLSXTemplate
import fr.proline.module.exporter.msi.view.ResultSummaryViewType

object IRMaLikeFullViewSetTemplateAsXLSX extends AbstractViewSetXLSXTemplate {

  // Create an XLSX template specific to the infos view
  private val infoXlsxTemplate = new InfoXLSXTemplate()
  
  // Create an XLSX template specific to the peptides view
//  private val irmaPeptidesXlsxTemplate = new BasicXLSXTemplate(
//   selectedFields = Some( IRMaLikeTemplateFields.peptidesFields.map(_.toString) )
//  )
  
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( ResultSummaryViewType.MSI_SEARCH_EXTENDED, infoXlsxTemplate, viewName = Some("search settings and infos") ),
    ViewTypeWithTemplate( ResultSummaryViewType.IMPORT_AND_VALIDATION_PROPS, infoXlsxTemplate, viewName = Some("import and filters") ),
    ViewTypeWithTemplate( ResultSummaryViewType.PROT_SET_TO_TYPICAL_PROT_MATCH, xlsxTemplate, viewName = Some("protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewType.PROT_SET_TO_BEST_PEPTIDE_MATCH, xlsxTemplate, viewName = Some("best PSM from protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewType.PROT_SET_TO_PROT_MATCH, xlsxTemplate, viewName = Some("protein matches in protein set ") ),   
    ViewTypeWithTemplate( ResultSummaryViewType.PROT_SET_TO_ALL_PEPTIDE_MATCHES, xlsxTemplate, viewName = Some("all PSMs from protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewType.STATISTICS, infoXlsxTemplate, viewName = Some("statistics") )
  )
  
}