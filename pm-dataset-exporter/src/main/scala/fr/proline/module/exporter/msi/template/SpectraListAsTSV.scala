package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.api.template.IViewSetTemplate
import fr.proline.module.exporter.commons.template.TSVTemplate
import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.msi.view.RSMSpectraViewTypes


/**
 * @author VD225637
 */
object SpectraListAsTSV extends IViewSetTemplate {
    
  private val tsvTemplate = new TSVTemplate()
  
    // Create the templated view types
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( RSMSpectraViewTypes.SPECTRA_LIST, tsvTemplate, viewName = Some("spectraList") )
  )
  
}