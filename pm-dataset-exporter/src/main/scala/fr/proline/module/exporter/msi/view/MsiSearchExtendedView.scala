package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._

object MsiSearchExtendedViewFields extends IViewFieldEnumeration {
  
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
  val SUBMITTED_QUERIES_COUNT = Field("submitted_queries_count")
  val SEARCHED_SEQUENCES_COUNT = Field("searched_sequences_count")
  val SOFTWARE_NAME = Field("software_name")
  val SOFTWARE_VERSION = Field("software_version")
  
  val MAIN_DATABASE_NAME = Field("main_database_name")
  val MAIN_DATABASE_RELEASE = Field("main_database_release")
  val TAXONOMY = Field("taxonomy")
  val ENZYMES = Field("enzymes")
  val MAX_MISSED_CLEAVAGES = Field("max_missed_cleavages")
  val FIXED_PTMS = Field("fixed_ptms")
  val VARIABLE_PTMS = Field("variable_ptms")  
  val PEPTIDE_CHARGE_STATES = Field("peptide_charge_states")
  val PEPTIDE_MASS_ERROR_TOLERANCE = Field("peptide_mass_error_tolerance")
  val PEPTIDE_MASS_ERROR_TOLERANCE_UNIT = Field("peptide_mass_error_tolerance_unit")
  //val QUANTITATION = Field("quantitation")
  val IS_DECOY = Field("is_decoy")
}

/*
IRMa infos fields: 

Database
Total # of seq.
Taxonomy
Enzyme
Quantitation
Fixed mod.
Peptide tol.
Search Date
Number of hits
Subset Threshold
E-mail
Release
# of seq. in taxo
Missed cleavages
Variable mod.
MS/MS tol.
Ion score cut-off
FPR
Mascot file
Data file
*/

class MsiSearchExtendedView( val rsm: ResultSummary ) extends IDatasetView {
  
  var viewName = "msi_search"
  val fields = MsiSearchExtendedViewFields
  
  case class MyBuildingContext( msiSearch: MSISearch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val msiSearch = myBuildingContext.msiSearch
    val pkl = msiSearch.peakList
    val searchSettings = msiSearch.searchSettings

    // TODO: add missing fields (from other tables than msi_search and search_settings)
    Map(
      fields.SEARCH_TITLE -> msiSearch.title,
      fields.SEARCH_DATE -> msiSearch.date,
      fields.RAW_FILE_NAME -> Option(pkl.rawFileName).getOrElse(""),
      fields.PEAKLIST_FILE_PATH -> pkl.path,
      fields.RESULT_FILE_NAME -> msiSearch.resultFileName,
      fields.RESULT_FILE_DIRECTORY -> msiSearch.resultFileDirectory,
      fields.JOB_NUMBER -> msiSearch.jobNumber,
      fields.USER_NAME -> msiSearch.userName,
      fields.USER_EMAIL -> msiSearch.userEmail,
      fields.QUERIES_COUNT -> msiSearch.queriesCount,
      fields.SUBMITTED_QUERIES_COUNT -> msiSearch.submittedQueriesCount,
      fields.SEARCHED_SEQUENCES_COUNT -> msiSearch.searchedSequencesCount,
      fields.SOFTWARE_NAME -> searchSettings.softwareName,
      fields.SOFTWARE_VERSION -> searchSettings.softwareVersion,
      fields.MAIN_DATABASE_NAME -> searchSettings.seqDatabases(0).name,
      fields.MAIN_DATABASE_RELEASE -> searchSettings.seqDatabases(0).releaseDate,
      fields.TAXONOMY -> searchSettings.taxonomy,
      fields.ENZYMES -> searchSettings.usedEnzymes.map(_.name).mkString(", "),
      fields.MAX_MISSED_CLEAVAGES -> searchSettings.maxMissedCleavages,
      fields.FIXED_PTMS -> searchSettings.fixedPtmDefs.map( _.toReadableString ).mkString("; "),
      fields.VARIABLE_PTMS -> searchSettings.variablePtmDefs.map( _.toReadableString ).mkString("; "),
      fields.PEPTIDE_CHARGE_STATES -> searchSettings.ms1ChargeStates,
      fields.PEPTIDE_MASS_ERROR_TOLERANCE -> searchSettings.ms1ErrorTol,
      fields.PEPTIDE_MASS_ERROR_TOLERANCE_UNIT -> searchSettings.ms1ErrorTolUnit,
      fields.IS_DECOY -> searchSettings.isDecoy
    ).map( r => r._1.toString -> r._2)
    
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    for( msiSearch <- rs.msiSearch if msiSearch != null ) {
      this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
    }
    
  }

}