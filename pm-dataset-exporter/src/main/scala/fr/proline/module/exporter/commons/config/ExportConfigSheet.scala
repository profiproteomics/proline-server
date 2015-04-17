package fr.proline.module.exporter.commons.config




/**
 * represents the configuration for a sheet in the export file
 */
class ExportConfigSheet (
    var id :String, 
    var title  :String, 
    var presentation :String,
    var fields : Array[ExportConfigField] // sorted by positions
)  {
	// Plain constructor
	def this() = this("", "",  ExportConfigConstant.PRESENTATION_SHEET_COLUMNS, new Array(0))
}

object ExportConfigSheet {
  
  // get all config for information sheet
	def getAllInformationSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_INFORMATION
	  sheet.title = "search settings and infos"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS
	  sheet.fields =  ExportConfigField.getAllInformationFieldsArray()
	  return sheet
	}
	
	// get all for import sheet
	def getAllImportSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_IMPORT
	  sheet.title = "import and filters"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS
	  
	  sheet.fields =  ExportConfigField.getAllImportFieldsArray()
	  return sheet
	}
	
	// get all for proteinSet sheet
	def getAllProteinSetSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_PROTEIN_SETS
	  sheet.title = "protein sets"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS
	  
	  sheet.fields =  ExportConfigField.getAllProteinSetsFieldsArray()
	  return sheet
	}
	
	// get all for best PSM sheet
	def getAllBestPSMSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_BEST_PSM
	  sheet.title = "best PSM from protein sets"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS
	  
	  sheet.fields =  ExportConfigField.getAllBestPSMFieldsArray()
	  return sheet
	}
	
	// get all for protein match sheet
	def getAllProteinMatchSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_PROTEIN_MATCH
	  sheet.title = "protein matches in protein set"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS
	  
	  sheet.fields =  ExportConfigField.getAllProteinMatchFieldsArray()
	  return sheet
	}
	
	// get all for all psm sheet
	def getAllAllPSMSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_ALL_PSM
	  sheet.title = "all PSMs from protein sets"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_COLUMNS
	  
	  sheet.fields =  ExportConfigField.getAllPSMFieldsArray()
	  return sheet
	}
	
	
  // get all for stat sheet
	def getAllStatSheet() :ExportConfigSheet={
	  val sheet: ExportConfigSheet = new ExportConfigSheet()
	  sheet.id = ExportConfigConstant.SHEET_STAT
	  sheet.title = "statistics"
	  sheet.presentation = ExportConfigConstant.PRESENTATION_SHEET_ROWS
	  
	  sheet.fields =  ExportConfigField.getAllStatFieldsArray()
	  return sheet
	}
	
}