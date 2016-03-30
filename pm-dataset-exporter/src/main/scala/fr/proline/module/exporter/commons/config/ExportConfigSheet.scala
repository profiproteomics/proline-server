package fr.proline.module.exporter.commons.config

/**
 * represents the configuration for a sheet in the export file
 */
case class ExportConfigSheet(
  val id: String = "",
  val title: String = "",
  val presentation: String = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
  val fields: Array[CustomFieldConfig] = Array(), // sorted by positions
  val defaultDisplayed: Boolean = true
) {
  private val fieldTitles = fields.map(_.title)
  //private val fieldTitleSet = fieldTitles.toSet
  
  require( fieldTitles.length == fieldTitles.distinct.length, "duplicated field titles are not allowed")
  require( fieldTitles.forall(_.isEmpty() == false), "empty field titles are not allowed")
  
  // return true if the sheet contains the given title before the given index (not included)
  /*def containsTitle(title: String, index: Int): Boolean = {
    
    if (fields.length >= index) {
      for (i <- 0 until index) {
        if (fields(i).title.equals(title)) {
          return true
        }
      }
    }
    
    return false
  }*/
  
  def copyWithDefaultFields() = this.copy( fields = fields.filter(_.defaultDisplayed) )

}

object ExportConfigSheet {

  // get full config for information sheet
  def getInformationSheetExportConfig(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_INFORMATION,
      title = "Search settings and infos",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getInformationSheetFields()
    )
  }

  // get full config for import sheet
  def getImportSheetExportConfig(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_IMPORT,
      title = "Import and filters",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getImportSheetFields()
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
      fields = CustomFieldConfigFactory.getProteinSetsSheetFields(true, fromXIC, fromSC)
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

}