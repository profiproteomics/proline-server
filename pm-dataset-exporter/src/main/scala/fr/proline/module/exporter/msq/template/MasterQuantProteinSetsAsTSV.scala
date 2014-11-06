package fr.proline.module.exporter.msq.template

import fr.proline.module.exporter.api.template.IViewSetTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.TSVTemplate
import fr.proline.module.exporter.msq.view.QuantitationViewTypes

object MasterQuantProteinSetsAsTSV extends IViewSetTemplate {
	// Create a generic TSV template for views
	private val tsvTemplate = new TSVTemplate()
  
	val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
	  ViewTypeWithTemplate(
			  QuantitationViewTypes.MASTER_QUANT_PROTEIN_SETS,
			  tsvTemplate,
			  viewName = Some("exportQuantProteinSetsProfile")
      )
	)
	
}