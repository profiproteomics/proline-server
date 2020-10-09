package fr.proline.module.exporter.commons.config

/**
 * represents the configuration for a sheet in the export file
 */
case class ExportConfigSheet(
  id: String = "",
  title: String = "",
  presentation: String = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
  fields: Array[CustomFieldConfig] = Array(), // sorted by positions
  defaultDisplayed: Boolean = true
) {
  private val fieldTitles = fields.map(_.title)

  require( fieldTitles.length == fieldTitles.distinct.length, "duplicated field titles are not allowed")
  require( fieldTitles.forall(!_.isEmpty), "empty field titles are not allowed")
  

  def copyWithDefaultFields() = this.copy( fields = fields.filter(_.defaultDisplayed) )

}

object ExportConfigSheet {

  // get full config for information sheet
  def getInformationSheetExportConfig(fromXIC: Boolean): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_INFORMATION,
      title = "Search settings and infos",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getInformationSheetFields(fromXIC)
    )
  }

  // get full config for import sheet
  def getImportSheetExportConfig(fromXIC: Boolean): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_IMPORT,
      title = "Import and filters",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getImportSheetFields(fromXIC)
    )
  }

  // get full config for proteinSet sheet
  def getProteinSetsSheetExportConfig(
    fromXIC: Boolean,
    fromSC: Boolean,
    title: String = "Protein sets"
  ): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_PROTEIN_SETS,
      title = title,
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getProteinSetsSheetFields(fromProtein = true, fromXIC = fromXIC, fromSC = fromSC)
    )
  }

  // get full config for best PSMs sheet
  def getBestPeptideMatchesSheetExportConfig(
    fromXIC: Boolean,
    fromSC: Boolean,
    defaultDisplayed: Boolean = true
  ): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_BEST_PSM,
      title = "Best PSM from protein sets",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getBestPeptideMatchesSheetFields(fromXIC, fromSC),
      defaultDisplayed = defaultDisplayed
    )
  }

  // get full config for protein match sheet
  def getProteinMatchesSheetExportConfig(defaultDisplayed: Boolean = true): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_PROTEIN_MATCH,
      title = "Protein matches in protein sets",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getProteinMatchesSheetFields(),
      defaultDisplayed = defaultDisplayed
    )
  }

  // get full config for all psm sheet
  def getPeptideMatchesSheetExportConfig(
    fromXIC: Boolean,
    fromSC: Boolean,
    title: String = "All PSMs from protein sets"
  ): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_ALL_PSM,
      title = title,
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getPeptideMatchesSheetFields(fromXIC, fromSC),
      defaultDisplayed = false
    )
  }
  
  def getMasterQuantPeptidesSheetExportConfig(): ExportConfigSheet = {
    this.getPeptideMatchesSheetExportConfig(fromXIC = true, fromSC = false, title = "Peptides from protein sets")
  }

  // get full config for masterQuantPeptideIon
  def getMasterQuantPepIonsSheetSheetExportConfig(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_MASTER_QUANT_PEPTIDE_ION,
      title = "Quantified peptide ions",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getMasterQuantPepIonSheetFields(),
      defaultDisplayed = false
    )
  }
  
  // get full config for masterQuantReporterIon
  def getMasterQuantRepIonsSheetSheetExportConfig(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_MASTER_QUANT_REPORTER_ION,
      title = "Quantified reporter ions",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getPeptideMatchesSheetFields(fromXIC = true, fromSC = false),
      defaultDisplayed = false
    )
  }

  // get full config for stat sheet
  def getStatisticsSheetExportConfig(defaultDisplayed: Boolean = true): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_STAT,
      title = "Dataset statistics",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getStatisticsSheetFields(),
      defaultDisplayed
    )
  }
  
  // get full config for "quant config" sheet
  def getQuantConfigSheetExportConfig(defaultDisplayed: Boolean = true): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_QUANT_CONFIG,
      title = "Quant config",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = Array(),
      defaultDisplayed
    )
  }

  def getPTMClusterSheetExportConfig(fromXIC: Boolean, defaultDisplayed: Boolean): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_PTM_CLUSTER,
      title = "Modification Clusters",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getPtmClusterSheetFields(fromXIC),
      defaultDisplayed
    )
  }

}