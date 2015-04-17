package fr.proline.module.exporter.commons.config.view

import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.commons.config.ExportConfigSheet

/**
 * generates a ViewFieldEnumeration based on a configuration of a sheet
 */
class SheetViewFieldsConfig (sheetConfig: ExportConfigSheet) extends IViewFieldEnumeration{
   val listFields :Array[ExportConfigField] = sheetConfig.fields
   for ( f <- listFields ) {
    	 Field(f.title)
   }
   
   def addField(title: String):  String= {
    return Field(title).toString()
    
  }
}