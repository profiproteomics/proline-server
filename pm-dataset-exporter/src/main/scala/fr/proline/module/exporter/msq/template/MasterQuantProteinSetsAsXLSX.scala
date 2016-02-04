package fr.proline.module.exporter.msq.template

import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.AbstractViewSetXLSXTemplate
import fr.proline.module.exporter.msq.view.QuantitationViewTypes

object MasterQuantProteinSetsAsXLSX extends AbstractViewSetXLSXTemplate {

  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate(
      QuantitationViewTypes.MASTER_QUANT_PROTEIN_SETS,
      xlsxTemplate,
      viewName = Some("exportQuantProteinSetsProfile")
    )
  )

}