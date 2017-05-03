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
    logger.debug("Parsing jsonConfig...")
    
    ExportConfig.fromJSON(jsonConfig)
  }

  // returns the json string corresponding to the given mode
  def getFullExportConfigAsJson(mode: String): String = {
    logger.debug("getFullExportConfigAsJson")
    
    ExportConfig.toJSON(this.getFullExportConfig(mode))
  }
  
  // returns the ExportConfig corresponding to the given mode
  def getFullExportConfig(mode: String): ExportConfig = {
    logger.debug("getFullExportConfig")
    
    if (mode == MODE_QUANT_SC)
      ExportConfig.getSCExportFullConfig()
    else if (mode == MODE_QUANT_XIC)
      ExportConfig.getXicExportFullConfig()
    else if (mode == MODE_QUANT_TAGGING)
      ExportConfig.getIsobaricTaggingExportFullConfig()
    else
      ExportConfig.getIdentificationFullExportConfig()
  }

  // for a given json with all configuration for a dataset, returns the corresponding default json string  
  def getDefaultExportConfig(mode: String): ExportConfig = {
    logger.debug("getDefaultExportConfig")
    
    val fullConfig = getFullExportConfig(mode)
    
    // build the default config from this object
    val exportConfigData = ExportConfigData(
      allProteinSet = fullConfig.dataExport.allProteinSet,
      bestProfile = fullConfig.dataExport.bestProfile
    )
    val defaultSheetsAndFields = fullConfig.sheets.withFilter(_.defaultDisplayed).map(_.copyWithDefaultFields())

    val confObj = ExportConfig(
      format = fullConfig.format,
      decimalSeparator = fullConfig.decimalSeparator,
      dateFormat = fullConfig.dateFormat,
      dataExport = exportConfigData,
      sheets = defaultSheetsAndFields
    )

    confObj
  }

  def getDefaultExportConfigAsJson(mode: String): String = {
    logger.debug("getDefaultExportConfigAsJson")

    val defaultConfig = getDefaultExportConfig(mode)

    // to JSON
    ExportConfig.toJSON(defaultConfig)
  }

}