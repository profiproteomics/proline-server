package fr.proline.module.exporter.msq.template

import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.api.template.IViewSetTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.AbstractViewSetTSVTemplate
import fr.proline.module.exporter.commons.template.TSVTemplate
import fr.proline.module.exporter.msq.view.QuantitationViewTypes

object BasicMasterQuantProteinSetsAsTSV extends AbstractViewSetTSVTemplate {

  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate(
      QuantitationViewTypes.BASIC_MASTER_QUANT_PROTEIN_SETS,
      tsvTemplate,
      viewName = Some("exportQuantProteinSets")
    )
  )

}