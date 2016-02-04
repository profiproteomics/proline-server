package fr.proline.module.exporter.commons.formatter

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter

import fr.proline.module.exporter.api.formatter.IViewFormatter
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.context.DirectoryContext
import fr.proline.module.exporter.commons.template.ITextTemplate

class TextFormatter(
  val template: ITextTemplate,
  val viewSetCreationContext: DirectoryContext
) extends IViewFormatter {
  
  val outputType = ViewFormatterType.TEXT
  
  // Defines some values
  //protected val locale = java.util.Locale.ENGLISH 
  
  protected class ViewToTextHelper(
    val view: IDataView
  ) {

    val selectedFields = template.selectedFields
    val sepChar = template.sepChar
    
    protected lazy val selectedFieldsOrFields: Seq[String] = {
      if( selectedFields.isDefined ) selectedFields.get.map(_.toString)
      else view.getFieldsNames
    }
    
    def mkRowHeader(): String = {
      selectedFieldsOrFields.mkString(sepChar)
    }
    
    def mkRow( record: Map[String,Any] ): String = {
      selectedFieldsOrFields.map( field => record.get(field).flatMap( Option(_) ).getOrElse("").toString() ).mkString(sepChar)
    }
  }
  
  def formatView(view: IDataView): File = {

    /*val outputFile = if (location.isFile) { // isFile => exist && is a normal File
      location
    } else {
      viewSetCreationContext.getViewLocation(location, view)
    }*/
    val outputDir = viewSetCreationContext.viewSetLocation.get    
    val outputFile = viewSetCreationContext.getViewLocation(outputDir, view)
    
    _formatView(view, outputFile)
    
    outputFile
  }

  protected def _formatView(view: IDataView, outputFile: File): Unit = {
    
    val fos = new FileOutputStream(outputFile)
    val writer = new PrintWriter( fos )
    val helper = new ViewToTextHelper( view )
    
    // Print the header
    writer.println( helper.mkRowHeader() )
    writer.flush()
    
    // Print the rows
    view.onEachRecord( record => {
      writer.println( helper.mkRow(record) )
      writer.flush()
    })
    
    fos.close()
  }

}