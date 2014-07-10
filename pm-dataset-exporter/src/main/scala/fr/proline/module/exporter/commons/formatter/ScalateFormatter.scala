package fr.proline.module.exporter.commons.formatter

import java.io.OutputStream
import java.io.PrintWriter
import org.fusesource.scalate._
import fr.proline.module.exporter.commons.template.ScalateTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.formatter.IViewFormatter

/**
 * @author David Bouyssie
 *
 */
class ScalateFormatter(
  val template: ScalateTemplate
) extends IViewFormatter {

  def formatView( view: IDataView, os: OutputStream ) {
    
    val engine = new TemplateEngine()
    
    // TODO: try to provide an iterator
    val output = engine.layout(
      template.URI,
      new PrintWriter( os ),
      Map(
        "headers" -> view.getFieldsNames,
        "records" -> view.getAllRecords
      )
    )
    
    //val output = engine.layout(template.URI, Map("records" -> view.getAllRecords ) )r
    //println( output )

  }

}