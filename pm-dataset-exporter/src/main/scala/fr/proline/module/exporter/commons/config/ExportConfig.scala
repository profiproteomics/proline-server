package fr.proline.module.exporter.commons.config

import fr.profi.util.serialization._

/**
 * Represents the export file configuration.
 * It has global parameters and different ExportSheetConfig
 */
case class ExportConfig(
  val format: String = ExportConfigConstant.FORMAT_XLSX, // cf FORMAT_XLSX or FORMAT_TSV
  val decimalSeparator: Char = ExportConfigConstant.DECIMAL_SEPARATOR_DOT,
  val dateFormat: String = ExportConfigConstant.DATE_FORMAT_HOUR,
  val titleSeparator: String = ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE,
  val dataExport: ExportConfigData = ExportConfigData.getDefaultConfig(),
  val sheets: Array[ExportConfigSheet] = Array() // sorted by positions
  ) {

  val formatValues: Array[String] = ExportConfigConstant.FORMAT_VALUES
  val decimalSeparatorValues: Array[String] = ExportConfigConstant.DECIMAL_SPEARATOR_VALUES
  val dateFormatValues: Array[String] = ExportConfigConstant.DATE_FORMAT_VALUES
  val titleSeparatorValues: Array[String] = ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_VALUES
  val sheetPresentationValues: Array[String] = ExportConfigConstant.PRESENTATION_SHEET_VALUES

}

object ExportConfig {

  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer

  // build object  from  JSON 
  def fromJSON(jsonConfig: String): ExportConfig = {
    CustomSerializer.deserialize[ExportConfig](jsonConfig)
  }

  // write object to JSON String
  def toJSON(conf: ExportConfig): String = CustomSerializer.serialize(conf)

  // get all config for identification export
  def getAllForIdentificationExport(): ExportConfig = {
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getAllInformationSheet(),
        ExportConfigSheet.getAllImportSheet(),
        ExportConfigSheet.getAllProteinSetSheet(fromXIC = false, fromSC = false),
        ExportConfigSheet.getAllBestPSMSheet(fromXIC = false, fromSC = false),
        ExportConfigSheet.getAllProteinMatchSheet(),
        ExportConfigSheet.getAllAllPSMSheet(fromXIC = false, fromSC = false),
        ExportConfigSheet.getAllStatSheet()
      )
    )
  }

  // get all config for SC export
  def getAllForSCExport(): ExportConfig = {
    
    val bestPSMSheet = ExportConfigSheet.getAllBestPSMSheet(fromXIC = false, fromSC = true)
    bestPSMSheet.defaultDisplayed = false
    
    val statSheet = ExportConfigSheet.getAllStatSheet()
    statSheet.defaultDisplayed = false
    
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getAllInformationSheet(),
        ExportConfigSheet.getAllImportSheet(),
        ExportConfigSheet.getAllProteinSetSheet(fromXIC = false, fromSC =true),
        bestPSMSheet,
        ExportConfigSheet.getAllProteinMatchSheet(),
        ExportConfigSheet.getAllAllPSMSheet(fromXIC = false, fromSC =true),
        statSheet
      )
    )
  }

  // get all config for XIC export
  def getAllForXICExport(): ExportConfig = {
    
    // DBO: why fromXIC == false here ???
    val bestPSMSheet = ExportConfigSheet.getAllBestPSMSheet(fromXIC = false, fromSC = true)
    bestPSMSheet.defaultDisplayed = false
    
    val proteinMatchSheet = ExportConfigSheet.getAllProteinMatchSheet()
    proteinMatchSheet.defaultDisplayed = false
    
    val statSheet = ExportConfigSheet.getAllStatSheet()
    statSheet.defaultDisplayed = false
    
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getAllInformationSheet(),
        ExportConfigSheet.getAllImportSheet(),
        ExportConfigSheet.getAllProteinSetSheet(fromXIC = true, fromSC = false),
        bestPSMSheet,
        proteinMatchSheet,
        ExportConfigSheet.getAllAllPSMSheet(fromXIC = true, fromSC = false),
        ExportConfigSheet.getAllMasterQuantPeptideIon(),
        statSheet
      )
    )

  }
}
