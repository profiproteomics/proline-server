package fr.proline.module.exporter.commons.config

import com.typesafe.scalalogging.LazyLogging
import ExportConfigConstant._

/**
 * read the configuration file (json) and build the configuration parameters
 * export the default possible configuration file
 */
object ExportConfigManager extends LazyLogging {
  
  // read a config file and build the  corresponding ExportConfig
  def readConfig(jsonConfig: String): ExportConfig = {
    //logger.debug("BuildCongif export "+jsonConfig);
    logger.debug("BuildCongif export ")
    
    ExportConfig.fromJSON(jsonConfig)
  }

  // check that all titles in a sheet are different -- returns true if ok
  /*def checkTitle2(config: ExportConfig): Boolean = {
    for (s <- config.sheets) {
      var i: Int = 0
      for (f <- s.fields) {
        if (f.title == null || f.title.trim().equals("")) {
          logger.info("The sheet " + s.id + " contains an empty title")
          return false
        }
        if (s.containsTitle(f.title, i)) {
          logger.info("The sheet " + s.id + " contains already the field " + f.title)
          return false
        }
        i = i + 1
      }
    }
    return true
  }*/

  // returns the json string corresponding to the given mode
  def getAllConfigurationExport(mode: String): String = {
    logger.debug("getAllConfigurationExport ")
    
    if (mode == MODE_QUANT_SC)
      getAllConfigurationForSCExport()
    else if (mode == MODE_QUANT_XIC)
      getAllConfigurationForXICExport()
    else
      getAllConfigurationForIdentificationExport()
  }

  // returns a json string with all  configuration for identification export
  def getAllConfigurationForIdentificationExport(): String = {
    logger.debug("getAllConfigurationForIdentificationExport ")
    ExportConfig.toJSON(ExportConfig.getAllForIdentificationExport())
  }

  // returns a json string with all configuration for SC export
  def getAllConfigurationForSCExport(): String = {
    logger.debug("getAllConfigurationForSCExport ")
    ExportConfig.toJSON(ExportConfig.getAllForSCExport())
  }

  // returns a json string with all configuration for XIC export
  def getAllConfigurationForXICExport(): String = {
    logger.debug("getAllConfigurationForXICExport ")
    ExportConfig.toJSON(ExportConfig.getAllForXICExport())
  }

  // for a given json with all configuration for a dataset, returns the corresponding default json string  
  def getDefaultConfiguration(mode: String): String = {
    logger.debug("getDefaultConfiguration ");
    
    val allConfig: String = getAllConfigurationExport(mode)
    
    // reserialize : 
    // TODO: DBO => why reserialize ???
    val allConfigObj = ExportConfigManager.readConfig(allConfig)
    
    // build the default config from this object
    val exportConfigData = ExportConfigData(
      allProteinSet = allConfigObj.dataExport.allProteinSet,
      bestProfile = allConfigObj.dataExport.bestProfile
    )
    val displayedSheetsAndFields = allConfigObj.sheets.withFilter(_.defaultDisplayed).map(_.copyWithDisplayedFields())
    
    val confObj = ExportConfig(
      format = allConfigObj.format,
      decimalSeparator = allConfigObj.decimalSeparator,
      dateFormat = allConfigObj.dateFormat,
      dataExport = exportConfigData,
      sheets = displayedSheetsAndFields
    )
      
    // to JSON
    ExportConfig.toJSON(confObj)
  }


}