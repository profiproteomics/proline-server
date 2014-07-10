package fr.proline.module.exporter.commons.formatter

import java.io.OutputStream
import fr.proline.module.exporter.commons.template.ScalastiTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.formatter.IViewFormatter

/**
 * @author David Bouyssie
 *
 */
class ScalastiFormatter(
  val template: ScalastiTemplate
) extends IViewFormatter {
  
  def formatView( view: IDataView, os: OutputStream ) {
    
  }

}