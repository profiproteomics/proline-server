package fr.proline.module.exporter.commons.config


import fr.profi.util.serialization._

/**
 * represents the export file configuration. It has global parameters and different ExportSheetConfig
 */

case class ExportConfig(
    var format :String, // cf FORMAT_XLSX or FORMAT_TSV
    var decimalSeparator  :Char, 
    var dateFormat : String, 
    var titleSeparator: String,
    var dataExport : ExportConfigData,
    var sheets : Array[ExportConfigSheet]  // sorted by positions
    
)  {
	// Plain constructor
	def this() = this(ExportConfigConstant.FORMAT_XLSX, ExportConfigConstant.DECIMAL_SEPARATOR_DOT, ExportConfigConstant.DATE_FORMAT_HOUR, 
	       ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE, new ExportConfigData() , Array.empty[ExportConfigSheet] )
	      
	val formatValues: Array[String] = ExportConfigConstant.FORMAT_VALUES
    val decimalSeparatorValues: Array[String] = ExportConfigConstant.DECIMAL_SPEARATOR_VALUES
    val dateFormatValues: Array[String] = ExportConfigConstant.DATE_FORMAT_VALUES 
    val titleSeparatorValues: Array[String] = ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_VALUES
    val sheetPresentationValues: Array[String] = ExportConfigConstant.PRESENTATION_SHEET_VALUES
	
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
	   var informationSheet : ExportConfigSheet =ExportConfigSheet.getAllInformationSheet()
	  var importSheet : ExportConfigSheet =ExportConfigSheet.getAllImportSheet()
	  var proteinSetSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinSetSheet(false, false)
	  var bestPSMSheet : ExportConfigSheet =ExportConfigSheet.getAllBestPSMSheet(false, false)
	  var proteinMatchSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinMatchSheet()
	  var allPSMSheet : ExportConfigSheet = ExportConfigSheet.getAllAllPSMSheet(false, false)
	  var statSheet : ExportConfigSheet =ExportConfigSheet.getAllStatSheet()
	  var sheetsList: Array[ExportConfigSheet] =  Array(informationSheet, importSheet, proteinSetSheet, bestPSMSheet, proteinMatchSheet, allPSMSheet, statSheet)
	  var conf :ExportConfig = new  ExportConfig(
	      ExportConfigConstant.FORMAT_XLSX, 
	      ExportConfigConstant.DECIMAL_SEPARATOR_DOT,  
	      ExportConfigConstant.DATE_FORMAT_HOUR, 
	       ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE,
	      ExportConfigData.getAllConfig(),
	      sheetsList
	      )
	  
	  
	  return conf
	}
	
	
	// get all config for SC export
	def getAllForSCExport() :ExportConfig={
	  var informationSheet : ExportConfigSheet =ExportConfigSheet.getAllInformationSheet()
	  var importSheet : ExportConfigSheet =ExportConfigSheet.getAllImportSheet()
	  var proteinSetSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinSetSheet(false, true)
	  var bestPSMSheet : ExportConfigSheet =ExportConfigSheet.getAllBestPSMSheet(false, true)
	  bestPSMSheet.defaultDisplayed = false
	  var proteinMatchSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinMatchSheet()
	  var allPSMSheet : ExportConfigSheet = ExportConfigSheet.getAllAllPSMSheet(false, true)
	  //allPSMSheet.defaultDisplayed = false
	  var statSheet : ExportConfigSheet =ExportConfigSheet.getAllStatSheet()
	  statSheet.defaultDisplayed = false
	  var sheetsList: Array[ExportConfigSheet] =  Array(informationSheet, importSheet, proteinSetSheet, bestPSMSheet, proteinMatchSheet, allPSMSheet, statSheet)
	  var conf :ExportConfig = new  ExportConfig(
	      ExportConfigConstant.FORMAT_XLSX, 
	      ExportConfigConstant.DECIMAL_SEPARATOR_DOT,  
	      ExportConfigConstant.DATE_FORMAT_HOUR, 
	       ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE,
	      ExportConfigData.getAllConfig(),
	      sheetsList
	      )
	  
	  
	  return conf
	}
	
	// get all config for XIC export
	def getAllForXICExport() :ExportConfig={
	   var informationSheet : ExportConfigSheet =ExportConfigSheet.getAllInformationSheet()
	   var importSheet : ExportConfigSheet =ExportConfigSheet.getAllImportSheet()
	   var proteinSetSheet : ExportConfigSheet =ExportConfigSheet.getAllProteinSetSheet(true, false)
	   var allPSMSheet : ExportConfigSheet = ExportConfigSheet.getAllAllPSMSheet(true, false)
	   var masterQuantPeptideIon : ExportConfigSheet = ExportConfigSheet.getAllMasterQuantPeptideIon()
	   var statSheet : ExportConfigSheet =ExportConfigSheet.getAllStatSheet()
	  statSheet.defaultDisplayed = false
	  var sheetsList: Array[ExportConfigSheet] =  Array(informationSheet, importSheet, proteinSetSheet, allPSMSheet, masterQuantPeptideIon, statSheet)
	  var conf :ExportConfig = new  ExportConfig(
	      ExportConfigConstant.FORMAT_XLSX, 
	      ExportConfigConstant.DECIMAL_SEPARATOR_DOT,  
	      ExportConfigConstant.DATE_FORMAT_HOUR, 
	       ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE,
	      ExportConfigData.getAllConfig(),
	      sheetsList
	      )
	  
	  
	  return conf
	}
}
