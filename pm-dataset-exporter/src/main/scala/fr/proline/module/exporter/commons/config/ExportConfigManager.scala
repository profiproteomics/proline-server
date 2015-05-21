package fr.proline.module.exporter.commons.config


import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.util.serialization._
import scala.collection.mutable.ArrayBuffer
import java.text.DecimalFormat

/**
 * read the configuration file (json) and build the configuration parameters
 * export the default possible configuration file
 */
object ExportConfigManager extends Logging {
   // read a config file and build the  corresponding ExportConfig
	def readConfig(jsonConfig:String) : ExportConfig = {
	  //logger.debug("BuildCongif export "+jsonConfig);
	  logger.debug("BuildCongif export ");
	  var config: ExportConfig = ExportConfig.fromJSON(jsonConfig)
	   return config
	}
	
	// check that all titles in a sheet are different -- returns true if ok
	def checkTitle(config : ExportConfig): Boolean = {
	  for(s <- config.sheets){
	    var i:Int = 0
	    for (f <- s.fields){
	      if (f.title == null || f.title.trim().equals("")){
	        logger.info("The sheet "+s.id+" contains an empty title" )
	        return false
	      }
	      if (s.isContainTitle(f.title, i)){
	        logger.info("The sheet "+s.id+" contains already the field "+f.title )
	        return false
	      }
	      i = i+1
	    }
	  }
	  return true
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
	
	// format the given value depending of the given DecimalFormat
	def format(decimalFormat: DecimalFormat, value: Any): Any = {
	  if (value == null || value.equals("") || (value.isInstanceOf[Double]  && value.asInstanceOf[Double].isNaN()) || (value.isInstanceOf[Float]  && value.asInstanceOf[Float].isNaN())){
	    return ""
	  }else if (!value.isInstanceOf[Double] && !value.isInstanceOf[Float] && !value.isInstanceOf[Int]){
	    return value
	  }
	  else{
	    if (decimalFormat.getDecimalFormatSymbols().getDecimalSeparator() == ExportConfigConstant.DECIMAL_SEPARATOR_DOT){
	      return decimalFormat.format(value).toDouble
	    }else {
	      return decimalFormat.format(value)
	    }
	  }
	  
	}
	
	
}