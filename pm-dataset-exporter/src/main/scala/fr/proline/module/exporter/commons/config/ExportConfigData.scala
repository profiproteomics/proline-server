package fr.proline.module.exporter.commons.config


/**
 * represents which data to export
 * for now, only which protein to export (all or validated proteins)
 */
class ExportConfigData  (
    var allProteinSet :Boolean
) {
	// Plain constructor
	def this() = this(true)
}

object ExportConfigData {
  
	// get all config
	def getAllConfig() :ExportConfigData={
	  var dataConfig :ExportConfigData = new ExportConfigData()
	  dataConfig.allProteinSet = true
	  return dataConfig
	}
}
    