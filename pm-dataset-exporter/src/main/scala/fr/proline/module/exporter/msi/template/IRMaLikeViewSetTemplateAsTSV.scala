package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.template.TSVTemplate
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes

object IRMaLikeViewSetTemplateAsTSV extends IViewSetTemplate {

  // Create a generic TSV template for views
  private val tsvTemplate = new TSVTemplate()
  
  // Create a TSV template specific to the peptides view
  private val irmaPeptidesTSVTemplate = new TSVTemplate(
    selectedFields = Some( IRMaLikeTemplateFields.peptidesFields )
  )
  
  // Create the templated view types
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( ResultSummaryViewTypes.MSI_SEARCH_EXTENDED, tsvTemplate, viewName = Some("infos") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH, tsvTemplate, viewName = Some("protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH, irmaPeptidesTSVTemplate, viewName = Some("peptides") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH, tsvTemplate, viewName = Some("protein matches") )
  )
  
}