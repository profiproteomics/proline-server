package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes
import fr.proline.module.exporter.commons.template.BasicXLSXTemplate

object IRMaLikeViewSetTemplateAsXLSX extends IViewSetTemplate {

  private val xlsxTemplate = new BasicXLSXTemplate()
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( ResultSummaryViewTypes.MSI_SEARCH_EXTENDED, xlsxTemplate, viewName = Some("infos") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH, xlsxTemplate, viewName = Some("summary") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_BEST_PEP_MATCH, xlsxTemplate, viewName = Some("peptides") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH, xlsxTemplate, viewName = Some("protein matches") )
  )
  
}