package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.commons.context.WorkbookContext
import fr.proline.module.exporter.api.template.IViewSetTemplate

abstract class AbstractViewSetXLSXTemplate extends IViewSetTemplate {
  
  lazy val viewSetCreationContext: IViewSetCreationContext = new WorkbookContext(fileExtension)

  // Create a generic XLSX template for views
	protected val xlsxTemplate = new BasicXLSXTemplate()
	
}