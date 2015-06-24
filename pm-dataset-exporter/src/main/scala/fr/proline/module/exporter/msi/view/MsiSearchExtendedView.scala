package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._

object MsiSearchExtendedViewFields extends IViewFieldEnumeration {
  
  val PROJECT_NAME = Field("project_name")
  val RESULT_SET_NAME = Field("result_set_name")
  
  val SEARCH_TITLE = Field("search_title")
  val SEARCH_DATE = Field("search_date")
  val RAW_FILE_NAME = Field("raw_file_name")
  val PEAKLIST_FILE_PATH = Field("peaklist_file_path")
  val RESULT_FILE_NAME = Field("result_file_name")
  val RESULT_FILE_DIRECTORY = Field("result_file_directory")
  val JOB_NUMBER = Field("job_number")
  val USER_NAME = Field("user_name")
  val USER_EMAIL = Field("user_email")
  
  val QUERIES_COUNT = Field("queries_count")
  val SEARCHED_SEQUENCES_COUNT = Field("searched_sequences_count")
  val SOFTWARE_NAME = Field("software_name")
  val SOFTWARE_VERSION = Field("software_version")
  val INSTRUMENT_CONFIG  = Field("instrument_config")
  
  val DATABASES_NAMES = Field("database_names")
  val DATABASES_RELEASES = Field("database_releases")
  val TAXONOMY = Field("taxonomy")
  val ENZYMES = Field("enzymes")
  val MAX_MISSED_CLEAVAGES = Field("max_missed_cleavages")
  val FIXED_PTMS = Field("fixed_ptms")
  val VARIABLE_PTMS = Field("variable_ptms")  
  val PEPTIDE_CHARGE_STATES = Field("peptide_charge_states")
  val PEPTIDE_MASS_ERROR_TOLERANCE = Field("peptide_mass_error_tolerance")
  val FRAGMENT_MASS_ERROR_TOLERANCE = Field("fragment_mass_error_tolerance")

  val IS_DECOY = Field("is_decoy")
  // option C13 is missing => we need to import this information ?
}

class MsiSearchExtendedView( val identDS: IdentDataSet ) extends IFixedDatasetView {
  
  val rsm = identDS.resultSummary
  var viewName = "msi_search"
  val fields = MsiSearchExtendedViewFields
  
  case class MyBuildingContext( msiSearch: MSISearch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val msiSearch = myBuildingContext.msiSearch
    val pkl = msiSearch.peakList
    val searchSettings = msiSearch.searchSettings
    val seqDatabases = searchSettings.seqDatabases
    val ms2SettingsOpt = searchSettings.msmsSearchSettings
    
    val fragmentTol = if( ms2SettingsOpt.isEmpty ) ""
    else ms2SettingsOpt.get.ms2ErrorTol + " " + ms2SettingsOpt.get.ms2ErrorTolUnit
    
    Map(
      fields.PROJECT_NAME -> identDS.projectName,
      fields.RESULT_SET_NAME -> rsm.resultSet.map(_.name).getOrElse(""),
      fields.SEARCH_TITLE -> msiSearch.title,
      fields.SEARCH_DATE -> msiSearch.date,
      fields.RAW_FILE_NAME -> Option(pkl.rawFileIdentifier).getOrElse(""),
      fields.PEAKLIST_FILE_PATH -> pkl.path,
      fields.RESULT_FILE_NAME -> msiSearch.resultFileName,
      fields.RESULT_FILE_DIRECTORY -> msiSearch.resultFileDirectory,
      fields.JOB_NUMBER -> msiSearch.jobNumber,
      fields.USER_NAME -> msiSearch.userName,
      fields.USER_EMAIL -> msiSearch.userEmail,
      fields.QUERIES_COUNT -> msiSearch.queriesCount,
      fields.SEARCHED_SEQUENCES_COUNT -> msiSearch.searchedSequencesCount,
      fields.SOFTWARE_NAME -> searchSettings.softwareName,
      fields.SOFTWARE_VERSION -> searchSettings.softwareVersion,
      fields.INSTRUMENT_CONFIG -> searchSettings.instrumentConfig.name,      
      fields.DATABASES_NAMES -> seqDatabases.map(_.name).mkString("; "),
      fields.DATABASES_RELEASES -> seqDatabases.map(_.releaseDate).mkString("; "),
      fields.TAXONOMY -> searchSettings.taxonomy,
      fields.ENZYMES -> searchSettings.usedEnzymes.map(_.name).mkString(", "),
      fields.MAX_MISSED_CLEAVAGES -> searchSettings.maxMissedCleavages,
      fields.FIXED_PTMS -> searchSettings.fixedPtmDefs.map( _.toReadableString ).mkString("; "),
      fields.VARIABLE_PTMS -> searchSettings.variablePtmDefs.map( _.toReadableString ).mkString("; "),
      fields.PEPTIDE_CHARGE_STATES -> searchSettings.ms1ChargeStates,
      fields.PEPTIDE_MASS_ERROR_TOLERANCE -> (searchSettings.ms1ErrorTol + " " + searchSettings.ms1ErrorTolUnit),
      fields.FRAGMENT_MASS_ERROR_TOLERANCE -> fragmentTol,
      fields.IS_DECOY -> searchSettings.isDecoy
    ).map( r => r._1.toString -> r._2)
    
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    
    if( rs.childMsiSearches.isEmpty ) {
      for( msiSearch <- rs.msiSearch if msiSearch != null ) {
        this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
      }
    } else {
      for( msiSearch <- rs.childMsiSearches ) {
        this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
      }
    }
    
  }

}