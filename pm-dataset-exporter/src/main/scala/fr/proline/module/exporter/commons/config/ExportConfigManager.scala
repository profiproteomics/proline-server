package fr.proline.module.exporter.commons.config


import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.util.serialization._
import scala.collection.mutable.ArrayBuffer

/**
 * read the configuration file (json) and build the configuration parameters
 * export the default possible configuration file
 */
object ExportConfigManager extends Logging {
   // read a config file and build the  corresponding ExportConfig
	def readConfig(jsonConfig:String) : ExportConfig = {
	  logger.debug("BuildCongif export "+jsonConfig);
	  var config: ExportConfig = ExportConfig.fromJSON(jsonConfig)
	   return config
	}
	
	// returns the json string corresponding to the given mode
	def getAllConfigurationExport(mode : String) : String = {
	  logger.debug("getAllConfigurationExport ");
	  if (mode == ExportConfigConstant.MODE_QUANT_SC){
	    return getAllConfigurationForSCExport()
	  }else if (mode == ExportConfigConstant.MODE_QUANT_XIC){
	    return getAllConfigurationForXICExport()
	  }
	  return getAllConfigurationForIdentificationExport()
	  
	}
	
	// returns a json string with all  configuration for identification export
	def getAllConfigurationForIdentificationExport() : String = {
	  logger.debug("getAllConfigurationForIdentificationExport ");
	  var config: ExportConfig = ExportConfig.getAllForIdentificationExport()
	  var jsonStr =ExportConfig.toJSON(config)
	  logger.debug("... result: "+jsonStr);
	  return jsonStr  
	}
	
	// returns a json string with all configuration for SC export
	def getAllConfigurationForSCExport() : String = {
	  logger.debug("getAllConfigurationForSCExport ");
	  var config: ExportConfig = ExportConfig.getAllForSCExport()
	  var jsonStr =ExportConfig.toJSON(config)
	  logger.debug("... result: "+jsonStr);
	  return jsonStr  
	}
	
	// returns a json string with all configuration for XIC export
	def getAllConfigurationForXICExport() : String = {
	  logger.debug("getAllConfigurationForXICExport ");
	  var config: ExportConfig = ExportConfig.getAllForXICExport()
	  var jsonStr =ExportConfig.toJSON(config)
	  logger.debug("... result: "+jsonStr);
	  return jsonStr  
	}
	
	// for a given json with all configuration for a dataset, returns the corresponding default json string  
	def getDefaultConfiguration(mode : String): String = {
	  logger.debug("getDefaultConfiguration ");
	  var allConfig : String = getAllConfigurationExport(mode)
	  var defaultConfig:String = allConfig
	  // reserialize : 
	  val allConfigObj = ExportConfigManager.readConfig(allConfig)
	  // build the default config from this object
	  var confObj: ExportConfig = new ExportConfig()
	  confObj.format = allConfigObj.format
	  confObj.decimalSeparator = allConfigObj.decimalSeparator
	  confObj.dateFormat = allConfigObj.dateFormat
	  var exportConfigData : ExportConfigData = new ExportConfigData()
	  exportConfigData.allProteinSet = allConfigObj.dataExport.allProteinSet
	  exportConfigData.bestProfile = allConfigObj.dataExport.bestProfile
	  confObj.dataExport = exportConfigData
	  var sheets: ArrayBuffer[ExportConfigSheet] = new ArrayBuffer()
	  for(s <- allConfigObj.sheets){
	    if (s.defaultDisplayed){
	      var sheet: ExportConfigSheet = new ExportConfigSheet()
	      sheet.id = s.id
	      sheet.title = s.title
	      sheet.presentation = s.presentation
	      var fields: ArrayBuffer[ExportConfigField] = new ArrayBuffer()
	      for (f <- s.fields){
	        if (f.defaultDisplayed){
	          var field: ExportConfigField = new ExportConfigField()
	          field.id = f.id
	          field.title = f.title
	          fields += field
	        }
	      }
	      sheet.fields = fields.toArray
	      sheets += sheet
	    }
	  }
	  confObj.sheets = sheets.toArray
	  // to JSON
	  defaultConfig = ExportConfig.toJSON(confObj)
	  return defaultConfig
	}
	
	
}