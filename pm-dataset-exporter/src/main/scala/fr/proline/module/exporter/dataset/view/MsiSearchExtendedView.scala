package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import scala.collection.mutable.ArrayBuffer

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.CustomViewFields
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

class MsiSearchExtendedView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat
) extends IFixedTableView {
  
  var viewName = "msi_search"
  val fieldsTitles = sheetConfig.fields.map(_.title)
  val fields = new CustomViewFields(fieldsTitles)
  
  case class MyBuildingContext( resultSet: LazyResultSet, msiSearch: MSISearch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val resultSet = myBuildingContext.resultSet
    val msiSearch = myBuildingContext.msiSearch
    val pkl = msiSearch.peakList
    val searchSettings = msiSearch.searchSettings
    val seqDatabases = searchSettings.seqDatabases
    val ms2SettingsOpt = searchSettings.msmsSearchSettings
    
    val ms1ErrorTol = decimalFormat.format(searchSettings.ms1ErrorTol) + " " + searchSettings.ms1ErrorTolUnit
    
    val fragmentTol = if( ms2SettingsOpt.isEmpty ) ""
    else decimalFormat.format(ms2SettingsOpt.get.ms2ErrorTol) + " " + ms2SettingsOpt.get.ms2ErrorTolUnit
    
    val seqDbCount = seqDatabases.length
    val releaseDates = new ArrayBuffer[String](seqDbCount)
    val databasesNames = new ArrayBuffer[String](seqDbCount)
    val databasesSeqCounts = new ArrayBuffer[Int](seqDbCount)
    for (seqDB <- seqDatabases){
      releaseDates += dateFormat.format(seqDB.releaseDate)
      databasesNames += seqDB.filePath
      databasesSeqCounts += seqDB.sequencesCount
    }
    
    val recordBuilder = Map.newBuilder[String,Any]

    for (fieldConfig <- sheetConfig.fields) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_INFORMATION_PROJECT_NAME                  => identDS.projectName
        case FIELD_INFORMATION_RESULT_SET_NAME               => resultSet.descriptor.name
        case FIELD_INFORMATION_SEARCH_TITLE                  => msiSearch.title
        case FIELD_INFORMATION_SEARCH_DATE                   => dateFormat.format(msiSearch.date)
        case FIELD_INFORMATION_RAW_FILE_NAME                 => Option(pkl.rawFileIdentifier).getOrElse("")
        case FIELD_INFORMATION_PEAKLIST_FILE_PATH            => pkl.path
        case FIELD_INFORMATION_RESULT_FILE_NAME              => msiSearch.resultFileName
        case FIELD_INFORMATION_RESULT_FILE_DIRECTORY         => msiSearch.resultFileDirectory
        case FIELD_INFORMATION_JOB_NUMBER                    => msiSearch.jobNumber
        case FIELD_INFORMATION_USER_NAME                     => msiSearch.userName
        case FIELD_INFORMATION_USER_EMAIL                    => msiSearch.userEmail
        case FIELD_INFORMATION_QUERIES_COUNT                 => msiSearch.queriesCount
        case FIELD_INFORMATION_SEARCHED_SEQUENCES_COUNT      => msiSearch.searchedSequencesCount
        case FIELD_INFORMATION_SOFTWARE_NAME                 => searchSettings.softwareName
        case FIELD_INFORMATION_SOFTWARE_VERSION              => searchSettings.softwareVersion
        case FIELD_INFORMATION_INSTRUMENT_CONFIG             => searchSettings.instrumentConfig.name
        case FIELD_INFORMATION_DATABASE_NAMES                => databasesNames.mkString("; ")
        case FIELD_INFORMATION_DATABASE_RELEASES             => releaseDates.mkString("; ")
        case FIELD_INFORMATION_DATABASE_SEQ_COUNT            => databasesSeqCounts.mkString("; ")
        case FIELD_INFORMATION_TAXONOMY                      => searchSettings.taxonomy
        case FIELD_INFORMATION_ENZYMES                       => searchSettings.usedEnzymes.map(_.name).mkString(", ")
        case FIELD_INFORMATION_MAX_MISSED_CLEAVAGES          => searchSettings.maxMissedCleavages
        case FIELD_INFORMATION_FIXED_PTMS                    => searchSettings.fixedPtmDefs.map(_.toReadableString).mkString("; ")
        case FIELD_INFORMATION_VARIABLE_PTMS                 => searchSettings.variablePtmDefs.map(_.toReadableString).mkString("; ")
        case FIELD_INFORMATION_PEPTIDE_CHARGE_STATES         => searchSettings.ms1ChargeStates
        case FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE  => ms1ErrorTol
        case FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE => fragmentTol
        case FIELD_INFORMATION_IS_DECOY                      => searchSettings.isDecoy
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    for( rs <- identDS.allResultSets if rs.msiSearch.isDefined ) {
      this.formatRecord(MyBuildingContext(rs, rs.msiSearch.get), recordFormatter)
    }
    
    /*val rs = identDS.resultSummary.lazyResultSet
    
    if( rs.msiSearch.isDefined ) {
      this.formatRecord(MyBuildingContext(rs, rs.msiSearch.get), recordFormatter)
    } else {
      for( childRs <- identDS.childResultSets if childRs.msiSearch.isDefined ) {
        this.formatRecord(MyBuildingContext(childRs, childRs.msiSearch.get), recordFormatter)
      }
    }*/ // rs is in the child childRs list ;  DBO: what does this mean ???

    /*for (childRs <- identDS.childResultSets) {
    	if (childRs.childMsiSearches.isEmpty) {
    		for (msiSearch <- childRs.msiSearch if msiSearch != null) {
    			this.formatRecord(MyBuildingContext(childRs,msiSearch), recordFormatter)
    		}
    	} else {
    		for (msiSearch <- childRs.childMsiSearches) {
    			this.formatRecord(MyBuildingContext(childRs,msiSearch), recordFormatter)
    		}
    	}
    }*/

  }

}