package fr.proline.module.exporter.commons.formatter

import java.io.OutputStream
import java.io.PrintWriter
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.commons.template.ITextTemplate
import fr.proline.module.exporter.api.formatter.IViewFormatter

class TextFormatter(
  val template: ITextTemplate
) extends IViewFormatter {
  
  // Defines some values
  //protected val locale = java.util.Locale.ENGLISH 
  
  protected class ViewToTextHelper(
    val view: IDatasetView
  ) {

    val selectedFields = template.selectedFields
    val sepChar = template.sepChar
    
    protected lazy val selectedFieldsOrFields: Seq[String] = {
      if( selectedFields.isDefined ) selectedFields.get.map(_.toString)
      else view.fields.values.toSeq.map(_.toString)
    }
    
    def mkRowHeader(): String = {
      selectedFieldsOrFields.mkString(sepChar)
    }
    
    def mkRow( record: Map[String,Any] ): String = {
      selectedFieldsOrFields.map( field => record.get(field).flatMap( Option(_) ).getOrElse("").toString() ).mkString(sepChar)
    }
  }
  
  def formatView( view: IDatasetView, os: OutputStream ) {
    
    val writer = new PrintWriter( os )
    val helper = new ViewToTextHelper( view )
    
    // Print the header
    writer.println( helper.mkRowHeader() )
    writer.flush()
    
    // Print the rows
    view.onEachRecord( record => {
      writer.println( helper.mkRow(record) )
      writer.flush()
    })

  }
  


}