package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.api.template.ViewTypeWithTemplate
import fr.proline.module.exporter.commons.template.AbstractViewSetTSVTemplate
import fr.proline.module.exporter.msi.view.RSMSpectraViewType

/**
 * @author VD225637
 */
object SpectraListAsTSV extends AbstractViewSetTSVTemplate {
    
    // Create the templated view types
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( RSMSpectraViewType.SPECTRA_LIST, tsvTemplate, viewName = Some("spectraList") )
  )
  
}