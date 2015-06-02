package fr.proline.module.exporter.commons.config.view

import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.ExportConfigConstant

/**
 * generates a ViewFieldEnumeration based on a configuration of a sheet
 */
class SheetViewFieldsConfig(fields: Array[String]) extends IViewFieldEnumeration {
  for (f <- fields) {
    Field(f)
  }

  def addField(title: String): String = {
    return Field(title).toString()

  }
}