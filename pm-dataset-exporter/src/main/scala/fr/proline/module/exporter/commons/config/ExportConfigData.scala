package fr.proline.module.exporter.commons.config


/**
 * represents which data to export
 * for now, only which protein to export (all or validated proteins)
 */
class ExportConfigData  (
    var dataExportAllProteinSet :Boolean
) {
	// Plain constructor
	def this() = this(true)
}

object ExportConfigData {
  
	// get all config
	def getAllConfig() :ExportConfigData={
	  var dataConfig :ExportConfigData = new ExportConfigData()
	  dataConfig.dataExportAllProteinSet = true
	  return dataConfig
	}
}
    