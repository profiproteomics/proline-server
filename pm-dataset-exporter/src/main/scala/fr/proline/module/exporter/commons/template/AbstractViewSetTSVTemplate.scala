package fr.proline.module.exporter.commons.template

import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.commons.context.DirectoryContext
import fr.proline.module.exporter.api.template.IViewSetTemplate

abstract class AbstractViewSetTSVTemplate extends IViewSetTemplate {
  
  lazy val viewSetCreationContext: IViewSetCreationContext = new DirectoryContext(fileExtension)
  
  // Create a generic TSV template for views
	protected val tsvTemplate = new TSVTemplate()
  
}