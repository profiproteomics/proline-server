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
  val sheets: Array[ExportConfigSheet] = Array(), // sorted by positions
  val modificationFormat: String = ExportConfigConstant.MODIFICATION_FORMAT_FIRST_THREE_LETTERS
  ) {

  val formatValues: Array[String] = ExportConfigConstant.FORMAT_VALUES
  val decimalSeparatorValues: Array[String] = ExportConfigConstant.DECIMAL_SPEARATOR_VALUES
  val dateFormatValues: Array[String] = ExportConfigConstant.DATE_FORMAT_VALUES
  val titleSeparatorValues: Array[String] = ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_VALUES
  val sheetPresentationValues: Array[String] = ExportConfigConstant.PRESENTATION_SHEET_VALUES
  val modificationFormatValues: Array[String] = ExportConfigConstant.MODIFICATION_FORMAT_VALUES

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
  def getIdentificationFullExportConfig(): ExportConfig = {
    val( fromXIC, fromSC ) = (false, false)
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getInformationSheetExportConfig(fromXIC),
        ExportConfigSheet.getImportSheetExportConfig(fromXIC),
        ExportConfigSheet.getProteinSetsSheetExportConfig( fromXIC, fromSC),
        ExportConfigSheet.getProteinMatchesSheetExportConfig(),
        ExportConfigSheet.getBestPeptideMatchesSheetExportConfig(fromXIC, fromSC),
        ExportConfigSheet.getPeptideMatchesSheetExportConfig(fromXIC, fromSC),
        ExportConfigSheet.getStatisticsSheetExportConfig()
      )
    )
  }

  // get all config for SC export
  def getSCExportFullConfig(): ExportConfig = {
    val( fromXIC, fromSC ) = (false, true)
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getInformationSheetExportConfig(fromXIC),
        ExportConfigSheet.getImportSheetExportConfig(fromXIC),
        ExportConfigSheet.getProteinSetsSheetExportConfig(fromXIC, fromSC),
        ExportConfigSheet.getProteinMatchesSheetExportConfig(),
        ExportConfigSheet.getBestPeptideMatchesSheetExportConfig(fromXIC, fromSC, defaultDisplayed = false),
        ExportConfigSheet.getPeptideMatchesSheetExportConfig(fromXIC, fromSC),
        ExportConfigSheet.getStatisticsSheetExportConfig(defaultDisplayed = false)
      )
    )
  }

  // get all config for XIC export
  def getXicExportFullConfig(): ExportConfig = {
    val( fromXIC, fromSC ) = (true, false)
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getInformationSheetExportConfig(fromXIC),
        ExportConfigSheet.getImportSheetExportConfig(fromXIC),
        ExportConfigSheet.getQuantConfigSheetExportConfig(),
        ExportConfigSheet.getProteinSetsSheetExportConfig(fromXIC, fromSC),
        ExportConfigSheet.getProteinMatchesSheetExportConfig(defaultDisplayed = false),
        //ExportConfigSheet.getBestPeptideMatchesSheetExportConfig(fromXIC, fromSC, defaultDisplayed = false),
        ExportConfigSheet.getMasterQuantPeptidesSheetExportConfig(),
        ExportConfigSheet.getMasterQuantPepIonsSheetSheetExportConfig(),
        ExportConfigSheet.getStatisticsSheetExportConfig(defaultDisplayed = false)
      )
    )
  }
  
  // get all config for IsobaricTagging export
  def getIsobaricTaggingExportFullConfig(): ExportConfig = {
    val( fromXIC, fromSC ) = (true, false)
    ExportConfig(
      sheets = Array(
        ExportConfigSheet.getInformationSheetExportConfig(fromXIC),
        ExportConfigSheet.getImportSheetExportConfig(fromXIC),
        ExportConfigSheet.getQuantConfigSheetExportConfig(),
        ExportConfigSheet.getProteinSetsSheetExportConfig(fromXIC, fromSC),
        ExportConfigSheet.getProteinMatchesSheetExportConfig(defaultDisplayed = false),
        ExportConfigSheet.getMasterQuantPeptidesSheetExportConfig(),
        ExportConfigSheet.getMasterQuantPepIonsSheetSheetExportConfig(),
        ExportConfigSheet.getMasterQuantRepIonsSheetSheetExportConfig()
        // FIXME: re-enable me when the NPE is fixed
        //ExportConfigSheet.getStatisticsSheetExportConfig(defaultDisplayed = false)
      )
    )
  }
  
}
