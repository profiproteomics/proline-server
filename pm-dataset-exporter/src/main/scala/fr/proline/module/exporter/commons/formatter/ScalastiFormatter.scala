package fr.proline.module.exporter.commons.formatter

import java.io.OutputStream
import fr.proline.module.exporter.commons.template.ScalastiTemplate
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.formatter.IViewFormatter

/**
 * @author David Bouyssie
 *
 */
class ScalastiFormatter(
  val template: ScalastiTemplate
) extends IViewFormatter {
  
  def formatView( view: IDatasetView, os: OutputStream ) {
    
  }

}