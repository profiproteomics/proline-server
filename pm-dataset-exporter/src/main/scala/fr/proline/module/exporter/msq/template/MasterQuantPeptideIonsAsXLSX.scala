package fr.proline.module.exporter.msq.template

import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.AbstractViewSetXLSXTemplate
import fr.proline.module.exporter.msq.view.QuantitationViewTypes

object MasterQuantPeptideIonsAsXLSX extends AbstractViewSetXLSXTemplate {

  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate(
      QuantitationViewTypes.MASTER_QUANT_PEPTIDE_IONS,
      xlsxTemplate,
      viewName = Some("exportQuantPeptideIons")
    )
  )

}