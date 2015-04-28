package fr.proline.module.exporter.commons.config


import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.util.serialization._

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
	  }else if (mode == ExportConfigConstant.MODE_QUANT_SC){
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
	
	// returns a json strin with all configuration for SC export
	def getAllConfigurationForSCExport() : String = {
	  logger.debug("getAllConfigurationForSCExport ");
	  var config: ExportConfig = ExportConfig.getAllForSCExport()
	  var jsonStr =ExportConfig.toJSON(config)
	  logger.debug("... result: "+jsonStr);
	  return jsonStr  
	}
	
	// returns a json strin with all configuration for XIC export
	def getAllConfigurationForXICExport() : String = {
	  logger.debug("getAllConfigurationForXICExport ");
	  var config: ExportConfig = ExportConfig.getAllForXICExport()
	  var jsonStr =ExportConfig.toJSON(config)
	  logger.debug("... result: "+jsonStr);
	  return jsonStr  
	}
	
	
}