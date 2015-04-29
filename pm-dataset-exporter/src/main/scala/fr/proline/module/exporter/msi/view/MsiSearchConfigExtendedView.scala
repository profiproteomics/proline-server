package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.SheetViewFieldsConfig
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import java.text.SimpleDateFormat
import java.text.DecimalFormat
import scala.collection.immutable.ListMap


class MsiSearchConfigExtendedView( val identDS: IdentDataSet , val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat) extends IFixedDatasetView {
  
  val rsm = identDS.resultSummary
  var viewName = "msi_search"
  val fields = new SheetViewFieldsConfig(sheetConfig)
  val childResultSets = identDS.childsResultSets
  
  case class MyBuildingContext( msiSearch: MSISearch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val msiSearch = myBuildingContext.msiSearch
    val pkl = msiSearch.peakList
    val searchSettings = msiSearch.searchSettings
    val seqDatabases = searchSettings.seqDatabases
    val ms2SettingsOpt = searchSettings.msmsSearchSettings
    
    val fragmentTol = if( ms2SettingsOpt.isEmpty ) ""
    else decimalFormat.format(ms2SettingsOpt.get.ms2ErrorTol) + " " + ms2SettingsOpt.get.ms2ErrorTolUnit
    
    var releaseDates:String = ""
    for (i <- 0 to (seqDatabases.length-1)){
      val seqDB = seqDatabases(i)
      releaseDates = releaseDates + dateFormat.format(seqDB.releaseDate) + "; "
    }
    
   var exportMap:ListMap[String,Any] = ListMap()
    
    val listFields :Array[ExportConfigField] = sheetConfig.fields
    for ( f <- listFields ) {
      f.id match{
        case ExportConfigConstant.FIELD_INFORMATION_PROJECT_NAME => {
          	exportMap += ( fields.addField(f.title) -> identDS.projectName)
        }
        case ExportConfigConstant.FIELD_INFORMATION_RESULT_SET_NAME => {
          	exportMap += ( fields.addField(f.title) -> rsm.resultSet.map(_.name).getOrElse(""))
        }
        case ExportConfigConstant.FIELD_INFORMATION_SEARCH_TITLE => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.title)
        }
        case ExportConfigConstant.FIELD_INFORMATION_SEARCH_DATE => {
          
          	exportMap += ( fields.addField(f.title) -> dateFormat.format(msiSearch.date))
        }
        case ExportConfigConstant.FIELD_INFORMATION_RAW_FILE_NAME => {
          	exportMap += ( fields.addField(f.title) -> Option(pkl.rawFileName).getOrElse(""))
        }
        case ExportConfigConstant.FIELD_INFORMATION_PEAKLIST_FILE_PATH => {
          	exportMap += ( fields.addField(f.title) -> pkl.path)
        }
        case ExportConfigConstant.FIELD_INFORMATION_RESULT_FILE_NAME => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.resultFileName)
        }
        case ExportConfigConstant.FIELD_INFORMATION_RESULT_FILE_DIRECTORY => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.resultFileDirectory)
        }
        case ExportConfigConstant.FIELD_INFORMATION_JOB_NUMBER => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.jobNumber)
        }
        case ExportConfigConstant.FIELD_INFORMATION_USER_NAME => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.userName)
        }
        case ExportConfigConstant.FIELD_INFORMATION_USER_EMAIL => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.userEmail)
        }
        case ExportConfigConstant.FIELD_INFORMATION_QUERIES_COUNT => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.queriesCount)
        }
        case ExportConfigConstant.FIELD_INFORMATION_SUBMITTED_QUERIES_COUNT => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.submittedQueriesCount)
        }
        case ExportConfigConstant.FIELD_INFORMATION_SEARCHED_SEQUENCES_COUNT => {
          	exportMap += ( fields.addField(f.title) -> msiSearch.searchedSequencesCount)
        }
        case ExportConfigConstant.FIELD_INFORMATION_SOFTWARE_NAME => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.softwareName)
        }
        case ExportConfigConstant.FIELD_INFORMATION_SOFTWARE_VERSION => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.softwareVersion)
        }
        case ExportConfigConstant.FIELD_INFORMATION_INSTRUMENT_CONFIG => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.instrumentConfig.name)
        }
        case ExportConfigConstant.FIELD_INFORMATION_DATABASE_NAMES => {
          	exportMap += ( fields.addField(f.title) -> seqDatabases.map(_.name).mkString("; "))
        }
        case ExportConfigConstant.FIELD_INFORMATION_DATABASE_RELEASES => {
          
          	exportMap += ( fields.addField(f.title) -> releaseDates)
        }
        case ExportConfigConstant.FIELD_INFORMATION_TAXONOMY => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.taxonomy)
        }
        case ExportConfigConstant.FIELD_INFORMATION_ENZYMES => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.usedEnzymes.map(_.name).mkString(", "))
        }
        case ExportConfigConstant.FIELD_INFORMATION_MAX_MISSED_CLEAVAGES => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.maxMissedCleavages)
        }
        case ExportConfigConstant.FIELD_INFORMATION_FIXED_PTMS => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.fixedPtmDefs.map( _.toReadableString ).mkString("; "))
        }
        case ExportConfigConstant.FIELD_INFORMATION_VARIABLE_PTMS => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.variablePtmDefs.map( _.toReadableString ).mkString("; "))
        }
        case ExportConfigConstant.FIELD_INFORMATION_PEPTIDE_CHARGE_STATES => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.ms1ChargeStates)
        }
        case ExportConfigConstant.FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE => {
          	exportMap += ( fields.addField(f.title) -> (decimalFormat.format(searchSettings.ms1ErrorTol) + " " + searchSettings.ms1ErrorTolUnit))
        }
        case ExportConfigConstant.FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE => {
          	exportMap += ( fields.addField(f.title) -> fragmentTol)
        }
        case ExportConfigConstant.FIELD_INFORMATION_IS_DECOY => {
          	exportMap += ( fields.addField(f.title) -> searchSettings.isDecoy)
        }
        case other => {
          // should not happen
        }
      }
    }
    
    //exportMap.map( r => r._1.toString -> r._2)
    exportMap
    
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    
    /*if( rs.childMsiSearches.isEmpty ) {
      for( msiSearch <- rs.msiSearch if msiSearch != null ) {
        this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
      }
    } else {
      for( msiSearch <- rs.childMsiSearches ) {
        this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
      }
    }*/  // rs is in the child childRs list

    for (childRs <- childResultSets) {
    	if (childRs.childMsiSearches.isEmpty) {
    		for (msiSearch <- childRs.msiSearch if msiSearch != null) {
    			this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
    		}
    	} else {
    		for (msiSearch <- childRs.childMsiSearches) {
    			this.formatRecord(MyBuildingContext(msiSearch), recordFormatter)
    		}
    	}
    }

  }

}