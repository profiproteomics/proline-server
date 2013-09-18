package fr.proline.module.exporter.commons.template

import java.io.File

import org.clapper.scalasti.StringTemplate
import org.clapper.scalasti.StringTemplateGroup

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.commons.formatter.ViewFormatterTypes

object BuildScalastiTemplate {
  
  var groupNumber = 0
  
  def apply( templateDir: File, templateName: String, fileExtension: String ): ScalastiTemplate = {
    groupNumber += 1
    
    val group = new StringTemplateGroup("group #"+groupNumber, templateDir )
    
    new ScalastiTemplate( group.template(templateName), fileExtension )
  }
  
  def apply( resourcePath: String, fileExtension: String ): ScalastiTemplate = {
    groupNumber += 1
    
    val group = new StringTemplateGroup("group #"+groupNumber )
    
    new ScalastiTemplate( group.template(resourcePath), fileExtension )
  }
  
}

class ScalastiTemplate( val stringTemplate: StringTemplate, val fileExtension: String ) extends IViewTemplate {
  
  final val formatterType = ViewFormatterTypes.SCALASTI
  
  

}