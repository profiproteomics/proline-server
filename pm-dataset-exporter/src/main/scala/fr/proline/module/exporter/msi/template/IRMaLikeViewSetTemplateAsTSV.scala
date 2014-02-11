package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes
import fr.proline.module.exporter.commons.template.TSVTemplate

object IRMaLikeViewSetTemplateAsTSV extends IViewSetTemplate {

  private val tsvTemplate = new TSVTemplate()
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( ResultSummaryViewTypes.MSI_SEARCH_EXTENDED, tsvTemplate, viewName = Some("infos") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH, tsvTemplate, viewName = Some("protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH_IRMA, tsvTemplate, viewName = Some("peptides") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH, tsvTemplate, viewName = Some("protein matches") )
  )
  
}