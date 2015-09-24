package fr.proline.module.exporter.commons.formatter

import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.api.formatter.IViewFormatter
import fr.proline.module.exporter.commons.template._

object BuildViewFormatter {
  
  def apply( template: IViewTemplate ): IViewFormatter = {
    
    val t = template match {
//      case scalateTpl: ScalateTemplate => new ScalateFormatter( scalateTpl )
//      case scalastiTpl: ScalastiTemplate => new ScalastiFormatter( scalastiTpl )
      case sheetTpl: IWorksheetTemplate => new SpreadsheetFormatter( sheetTpl )
      case textTpl: ITextTemplate => new TextFormatter( textTpl )
      case _ => throw new Exception("unknown template")
    }

    t
  }
  
  
}