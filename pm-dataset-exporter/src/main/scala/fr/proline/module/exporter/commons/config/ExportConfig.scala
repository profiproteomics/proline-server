package fr.proline.module.exporter.commons.config


import fr.profi.util.serialization._

/**
 * represents the export file configuration. It has global parameters and different ExportSheetConfig
 */

case class ExportConfig(
    var format :String, 
    var decimalSeparator  :Char, 
    var dateFormat : String, 
    var dataExport : ExportConfigData,
    var sheets : Array[ExportConfigSheet] // sorted by positions
)  {
	// Plain constructor
	def this() = this("xlsx", '.', "YYYYMMDD HH:mm:ss", new ExportConfigData() , Array.empty[ExportConfigSheet])
	
	
}

object ExportConfig{
  
	object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer 
  
     // build object  from  JSON 
     def fromJSON(jsonConfig:String): ExportConfig={
	   val config :ExportConfig =  CustomSerializer.deserialize[ExportConfig](jsonConfig)
	   return config
	 }
	
	
	// write object to JSON String
     def toJSON(conf: ExportConfig): String={
	   return CustomSerializer.serialize(conf)
	 }
	
     
     
  
    // get all config for identification export
	def getAllForIdentificationExport() :ExportConfig={
	  var conf :ExportConfig = new  ExportConfig()
	  conf.format = "xlsx"
	  conf.decimalSeparator = '.'
	  conf.dateFormat = "YYYY:MM:DD HH:mm:ss"
	  conf.dataExport = ExportConfigData.getAllConfig()
	  var informationSheet : ExportConfigSheet =ExportConfigSheet.getAllInformationSheet()
	  var importSheet : ExportConfigSheet =ExportConfigSheet.getAllImportSheet
	  var proteinSetSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinSetSheet
	  var bestPSMSheet : ExportConfigSheet =ExportConfigSheet.getAllBestPSMSheet
	  var proteinMatchSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinMatchSheet
	  var allPSMSheet : ExportConfigSheet = ExportConfigSheet.getAllAllPSMSheet
	  var statSheet : ExportConfigSheet =ExportConfigSheet.getAllStatSheet()
	  conf.sheets =  Array(informationSheet, importSheet, proteinSetSheet, bestPSMSheet, proteinMatchSheet, allPSMSheet, statSheet)
	  
	  return conf
	}
}
	