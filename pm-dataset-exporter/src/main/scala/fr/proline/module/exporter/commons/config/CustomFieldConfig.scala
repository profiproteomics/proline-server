package fr.proline.module.exporter.commons.config

/**
 * Represents the configuration for a field in the customizable export.
 */
case class CustomFieldConfig(
  val id: String,
  val title: String
) {
  // Plain constructor
  def this() = this("", "")
  var defaultDisplayed: Boolean = true
}

