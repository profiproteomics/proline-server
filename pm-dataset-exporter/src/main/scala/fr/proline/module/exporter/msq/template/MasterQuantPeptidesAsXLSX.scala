package fr.proline.module.exporter.msq.template

import fr.proline.module.exporter.api.template.IViewSetTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.BasicXLSXTemplate
import fr.proline.module.exporter.msq.view.QuantitationViewTypes

object MasterQuantPeptidesAsXLSX extends IViewSetTemplate {
	// Create a generic XLSX template for views
	private val xlsxTemplate = new BasicXLSXTemplate()
  
	val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
	  ViewTypeWithTemplate(
			  QuantitationViewTypes.MASTER_QUANT_PEPTIDE,
			  xlsxTemplate,
			  viewName = Some("exportQuantPeptides")
      )
	)
	
}