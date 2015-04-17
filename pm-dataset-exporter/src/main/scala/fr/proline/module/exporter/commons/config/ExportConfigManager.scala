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
	
	// returns a json string with all  configuration for identification export
	def getAllConfigurationForIdentificationExport() : String = {
	  logger.debug("getAllConfigurationForIdentificationExport ");
	  var config: ExportConfig = ExportConfig.getAllForIdentificationExport()
	  var jsonStr =ExportConfig.toJSON(config)
	  logger.debug("... result: "+jsonStr);
	  return jsonStr  
	}
}