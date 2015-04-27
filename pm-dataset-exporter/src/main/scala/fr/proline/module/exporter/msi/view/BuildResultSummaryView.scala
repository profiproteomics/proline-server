package fr.proline.module.exporter.msi.view

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.ProteinMatch
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.exporter.api.view.IFixedDatasetView
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import java.text.SimpleDateFormat
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import scala.collection.immutable.ListMap

case class IdentDataSet (
  projectName: String,
  resultSummary: ResultSummary
  // TODO: represent the result set hierarchy here (merge) ???
) extends Logging {

  // Count the number of protein sets and proteins matches related to a given peptide match
  val validProtSetIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
  val validProtMatchIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
    
  // Init Maps 
  resultSummary.proteinSets.filter(_.isValidated).foreach( protSet =>{
	  
	  protSet.peptideSet.getPeptideMatchIds.foreach(validProtSetIdSetByPepMatchId.getOrElseUpdate(_, new HashSet[Long] ) += protSet.id )
    
	  val protMatchIdByPepSet =  protSet.getAllProteinMatchesIdByPeptideSet
	  protMatchIdByPepSet.foreach( entry =>{
		  val pepSet = entry._1
		  for(prMId <- entry._2; 
			  pepMatchId<-pepSet.getPeptideMatchIds) {
			  	validProtMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long] ) += prMId
  			}
	  })
  })

    
  lazy val pepMatchById = resultSummary.resultSet.get.getPeptideMatchById
  
  //   Create list of all ProtMatches for pepMatches (validated or not) 
  lazy val allProtMatchSetByPepId =  { 
    val resultBuilder = new HashMap[Long, HashSet[ProteinMatch]]
    for( protMatch <- resultSummary.resultSet.get.proteinMatches ) {
      if (protMatch.sequenceMatches != null) {
        protMatch.sequenceMatches.foreach(seqMatch => {
          val pepId = seqMatch.getPeptideId
          resultBuilder.getOrElseUpdate(seqMatch.getPeptideId, new HashSet[ProteinMatch]) +=protMatch          
        }) //end go throudh protMatch sequenceMatches 
      }      
    } //End go through ProtMarches
    resultBuilder
  }
}

object BuildResultSummaryView {
  
  private def _builders(exportConfig :ExportConfig): Map[IViewTypeEnumeration#Value, IdentDataSet => IDataView ] = {
    var buildMap: ListMap[IViewTypeEnumeration#Value, IdentDataSet => IDataView ] =  ListMap()
    if (exportConfig == null) {
      // TODO to be removed as soon as the current templates are moved to config JSON
      Map( 
       ResultSummaryViewTypes.MSI_SEARCH_EXTENDED -> { ds: IdentDataSet => new MsiSearchExtendedView(ds) },
       ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS -> { ds: IdentDataSet => new ResultParseAndFiltersView(ds.resultSummary) },
       ResultSummaryViewTypes.STATISTICS -> { ds: IdentDataSet => new StatisticsView(ds.resultSummary) },
       ResultSummaryViewTypes.PEP_SET_TO_PROT_MATCH -> { ds: IdentDataSet => new PepSetToProtMatchView(ds) },
       ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToProtMatchView(ds) },
       ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToTypicalProtMatchView(ds) },
       ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: IdentDataSet => new ProtSetToBestPepMatchView(ds) },
       ResultSummaryViewTypes.PROT_SET_TO_ALL_PEPTIDE_MATCHES -> { ds: IdentDataSet => new ProtSetToAllPepMatchesView(ds) },
       ResultSummaryViewTypes.TYPICAL_PROT_MATCH_TO_ALL_PEP_MATCHES -> { ds: IdentDataSet => new TypicalProtMatchToAllPepMatchesView(ds) } 
      )
    }else{
       val dateFormat : SimpleDateFormat = new SimpleDateFormat(exportConfig.dateFormat)
       val decimalFormat : DecimalFormat = new DecimalFormat()
       var decSep: DecimalFormatSymbols = new DecimalFormatSymbols()
       decSep.setDecimalSeparator(exportConfig.decimalSeparator)
       decimalFormat.setDecimalFormatSymbols(decSep)
       decimalFormat.setGroupingUsed(false)
       val exportAllProteinSet: Boolean = exportConfig.dataExport.allProteinSet
    	for ( sheet <- exportConfig.sheets ) {
    		sheet.id match {
            	case ExportConfigConstant.SHEET_INFORMATION => {
            		buildMap += (ResultSummaryViewTypes.MSI_SEARCH_EXTENDED -> { ds: IdentDataSet => new MsiSearchConfigExtendedView(ds, sheet, dateFormat, decimalFormat) })
            	}
            	case ExportConfigConstant.SHEET_IMPORT => {
            		buildMap += (ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS -> { ds: IdentDataSet => new ImportAndValidationPropsView(ds.resultSummary, sheet, dateFormat, decimalFormat) })
            	}
            	case ExportConfigConstant.SHEET_PROTEIN_SETS => {
            		buildMap += (ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToTypicalProtMatchConfigView(ds, sheet, dateFormat, decimalFormat, exportAllProteinSet) })
            	}
            	case ExportConfigConstant.SHEET_BEST_PSM => {
            		buildMap += (ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: IdentDataSet => new ProtSetToBestPepMatchConfigView(ds, sheet, dateFormat, decimalFormat, exportAllProteinSet) })
            	}
            	case ExportConfigConstant.SHEET_PROTEIN_MATCH => {
            		buildMap += (ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToProtMatchConfigView(ds, sheet, dateFormat, decimalFormat, exportAllProteinSet) })
            	}
            	case ExportConfigConstant.SHEET_ALL_PSM => {
            		buildMap += (ResultSummaryViewTypes.PROT_SET_TO_ALL_PEPTIDE_MATCHES -> { ds: IdentDataSet => new ProtSetToAllPepMatchesConfigView(ds, sheet, dateFormat, decimalFormat, exportAllProteinSet) })
            	}
            	case ExportConfigConstant.SHEET_STAT => {
            		buildMap += (ResultSummaryViewTypes.STATISTICS -> { ds: IdentDataSet => new StatisticsConfigView(ds.resultSummary, sheet, dateFormat, decimalFormat) })
            	}
            	case other => {
			  
            	}
    		}
    	}
    	return buildMap
    }
}

  def apply( identDS: IdentDataSet, viewType: IViewTypeEnumeration#Value, exportConfig :ExportConfig ): IDataView = {
    _builders(exportConfig)(viewType)(identDS)
  }
  
}