package fr.proline.module.exporter.commons.config

/**
 * represents the configuration for a sheet in the export file
 */
class ExportConfigSheet(
  var id: String,
  var title: String,
  var presentation: String,
  var fields: Array[ExportConfigField] // sorted by positions
  ) {
  // Plain constructor
  def this() = this("", "", ExportConfigConstant.PRESENTATION_SHEET_COLUMNS, new Array(0))
  var defaultDisplayed: Boolean = true

  // return true if the sheet contains the given title before the given index (not included)
  def isContainTitle(title: String, index: Int): Boolean = {
    if (fields.length >= index) {
      for (i <- 0 until index) {
        if (fields(i).title.equals(title)) {
          return true
        }
      }
    }
    return false
  }

}

object ExportConfigSheet {

  // get all config for information sheet
  def getAllInformationSheet(): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_INFORMATION,
      "search settings and infos",
      ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      ExportConfigField.getAllInformationFieldsArray()
    )
    sheet.defaultDisplayed = true

    return sheet
  }

  // get all for import sheet
  def getAllImportSheet(): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_IMPORT,
      "import and filters",
      ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      ExportConfigField.getAllImportFieldsArray()
    )
    sheet.defaultDisplayed = true
    return sheet
  }

  // get all for proteinSet sheet
  def getAllProteinSetSheet(fromXIC: Boolean, fromSC: Boolean): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_PROTEIN_SETS,
      "protein sets",
      ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      ExportConfigField.getAllProteinSetsFieldsArray(true, fromXIC, fromSC)
    )
    sheet.defaultDisplayed = true
    return sheet
  }

  // get all for best PSM sheet
  def getAllBestPSMSheet(fromXIC: Boolean, fromSC: Boolean): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_BEST_PSM,
      "best PSM from protein sets",
      ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      ExportConfigField.getAllBestPSMFieldsArray(fromXIC, fromSC)
    )
    sheet.defaultDisplayed = true
    return sheet
  }

  // get all for protein match sheet
  def getAllProteinMatchSheet(): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_PROTEIN_MATCH,
      "protein matches in protein set",
      ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      ExportConfigField.getAllProteinMatchFieldsArray()
    )
    sheet.defaultDisplayed = true
    return sheet
  }

  // get all for all psm sheet
  def getAllAllPSMSheet(fromXIC: Boolean, fromSC: Boolean): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_ALL_PSM,
      "all PSMs from protein sets",
      ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      ExportConfigField.getAllPSMFieldsArray(fromXIC, fromSC)
    )
    sheet.defaultDisplayed = false
    return sheet
  }

  // get all for masterQuantPeptideIon
  def getAllMasterQuantPeptideIon(): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_MASTER_QUANT_PEPTIDE_ION,
      "master quant peptide ion",
      ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      ExportConfigField.getAllMasterQuantPeptideIon()
    )
    sheet.defaultDisplayed = false
    return sheet
  }

  // get all for stat sheet
  def getAllStatSheet(): ExportConfigSheet = {
    val sheet: ExportConfigSheet = new ExportConfigSheet(
      ExportConfigConstant.SHEET_STAT,
      "statistics",
      ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      ExportConfigField.getAllStatFieldsArray()
    )
    sheet.defaultDisplayed = true
    return sheet
  }

}