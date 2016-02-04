package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.template.AbstractViewSetXLSXTemplate
import fr.proline.module.exporter.msi.view.ResultSummaryViewType

object AllPSMViewSetTemplateAsXLSX extends AbstractViewSetXLSXTemplate {

  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate(
      ResultSummaryViewType.TYPICAL_PROT_MATCH_TO_ALL_PEP_MATCHES,
      xlsxTemplate,
      viewName = Some("peptide_matches")
    )
  )
  
}