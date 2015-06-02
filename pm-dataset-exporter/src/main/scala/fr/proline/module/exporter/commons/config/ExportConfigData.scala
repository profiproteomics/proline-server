package fr.proline.module.exporter.commons.config

/**
 * represents which data to export
 * for now, only which protein to export (all or validated proteins)
 */
class ExportConfigData(
  var allProteinSet: Boolean,
  var bestProfile: Boolean // export  bestProfile or iterates over profiles
  ) {
  // Plain constructor
  def this() = this(true, true)
}

object ExportConfigData {

  // get all config
  def getAllConfig(): ExportConfigData = {
    var dataConfig: ExportConfigData = new ExportConfigData()
    dataConfig.allProteinSet = true
    dataConfig.bestProfile = true
    return dataConfig
  }
}
    