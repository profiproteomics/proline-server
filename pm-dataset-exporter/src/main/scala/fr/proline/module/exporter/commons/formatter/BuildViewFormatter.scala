package fr.proline.module.exporter.commons.formatter

import fr.proline.module.exporter.api.formatter.IViewFormatter
import fr.proline.module.exporter.api.context.IViewSetCreationContext
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.context.DirectoryContext
import fr.proline.module.exporter.commons.context.WorkbookContext
import fr.proline.module.exporter.commons.template._

object BuildViewFormatter {
  
  def apply( template: IViewTemplate, viewSetCreationContext: IViewSetCreationContext ): IViewFormatter = {
    
    val t = template match {
//      case scalateTpl: ScalateTemplate => new ScalateFormatter( scalateTpl )
//      case scalastiTpl: ScalastiTemplate => new ScalastiFormatter( scalastiTpl )
      case sheetTpl: IWorksheetTemplate => new SpreadsheetFormatter( sheetTpl, viewSetCreationContext.asInstanceOf[WorkbookContext] )
      case textTpl: ITextTemplate => new TextFormatter( textTpl, viewSetCreationContext.asInstanceOf[DirectoryContext] )
      case _ => throw new Exception("unknown template")
    }

    t
  }
  
  
}