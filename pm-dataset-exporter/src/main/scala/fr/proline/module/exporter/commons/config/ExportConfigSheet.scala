package fr.proline.module.exporter.commons.config

/**
 * represents the configuration for a sheet in the export file
 */
case class ExportConfigSheet(
  val id: String = "",
  val title: String = "",
  val presentation: String = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
  val fields: Array[CustomFieldConfig] = Array(), // sorted by positions
  var defaultDisplayed: Boolean = true
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
  
  def copyWithDisplayedFields() = this.copy( fields = fields.filter(_.defaultDisplayed) )

}

object ExportConfigSheet {

  // get all config for information sheet
  def getAllInformationSheet(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_INFORMATION,
      title = "search settings and infos",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getAllInformationFieldsArray()
    )
  }

  // get all for import sheet
  def getAllImportSheet(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_IMPORT,
      title = "import and filters",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getAllImportFieldsArray()
    )
  }

  // get all for proteinSet sheet
  def getAllProteinSetSheet(fromXIC: Boolean, fromSC: Boolean): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_PROTEIN_SETS,
      title = "protein sets",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getAllProteinSetsFieldsArray(true, fromXIC, fromSC)
    )
  }

  // get all for best PSM sheet
  def getAllBestPSMSheet(fromXIC: Boolean, fromSC: Boolean): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_BEST_PSM,
      title = "best PSM from protein sets",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getAllBestPSMFieldsArray(fromXIC, fromSC)
    )
  }

  // get all for protein match sheet
  def getAllProteinMatchSheet(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_PROTEIN_MATCH,
      title = "protein matches in protein set",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getAllProteinMatchFieldsArray()
    )
  }

  // get all for all psm sheet
  def getAllAllPSMSheet(fromXIC: Boolean, fromSC: Boolean): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_ALL_PSM,
      title = "all PSMs from protein sets",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getAllPSMFieldsArray(fromXIC, fromSC),
      defaultDisplayed = false
    )
  }

  // get all for masterQuantPeptideIon
  def getAllMasterQuantPeptideIon(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_MASTER_QUANT_PEPTIDE_ION,
      title = "peptide ions from protein sets",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS,
      fields = CustomFieldConfigFactory.getAllMasterQuantPeptideIon(),
      defaultDisplayed = false
    )
  }

  // get all for stat sheet
  def getAllStatSheet(): ExportConfigSheet = {
    ExportConfigSheet(
      id = ExportConfigConstant.SHEET_STAT,
      title = "statistics",
      presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS,
      fields = CustomFieldConfigFactory.getAllStatFieldsArray()
    )
  }

}