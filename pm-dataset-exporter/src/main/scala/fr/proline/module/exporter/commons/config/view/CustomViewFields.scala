package fr.proline.module.exporter.commons.config.view

import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.commons.config.CustomFieldConfig
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.ExportConfigConstant

/**
 * Generates a ViewFieldEnumeration based on an iterable of field titles
 */
class CustomViewFields(titles: Iterable[String]) extends IViewFieldEnumeration {
  
  for (title <- titles) {
    Field(title)
  }

  // TODO: remove me
  def addField(title: String): String = {
    Field(title).toString()
  }
  
}