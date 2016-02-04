package fr.proline.module.exporter.commons.config

/**
 * represents which data to export
 * for now, only which protein to export (all or validated proteins)
 */
case class ExportConfigData(
  allProteinSet: Boolean = true,
  bestProfile: Boolean = true // export  bestProfile or iterates over profiles
)

object ExportConfigData {

  // get all config
  /*def getAllConfig(): ExportConfigData = {
    var dataConfig: ExportConfigData = new ExportConfigData()
    dataConfig.allProteinSet = true
    dataConfig.bestProfile = true
    return dataConfig
  }*/
  
  def getDefaultConfig() = ExportConfigData()
}
