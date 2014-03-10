package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes
import fr.proline.module.exporter.commons.template.BasicXLSXTemplate
import fr.proline.module.exporter.commons.template.InfoXLSXTemplate

object AllPSMViewSetTemplateAsXLSX extends IViewSetTemplate {

  // Create a generic XLSX template for views
  private val xlsxTemplate = new BasicXLSXTemplate()
  
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( ResultSummaryViewTypes.ALL_PEPTIDE_MATCHES, xlsxTemplate, viewName = Some("peptide_matches") )
  )
  
}