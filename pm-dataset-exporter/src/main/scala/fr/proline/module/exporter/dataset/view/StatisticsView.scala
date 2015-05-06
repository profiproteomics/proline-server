package fr.proline.module.exporter.dataset.view


import fr.proline.module.exporter.commons.config.view.SheetViewFieldsConfig
import java.text.SimpleDateFormat
import fr.proline.module.exporter.api.view.IFormLikeView
import java.text.DecimalFormat
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.core.om.model.msi._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.immutable.ListMap


class StatisticsView ( val rsm: ResultSummary , val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat) extends IFormLikeView {
  var viewName = "stats"
   var listFields: ArrayBuffer[String] = new ArrayBuffer()
   for ( f <- sheetConfig.fields ) {
    	 listFields += f.title
   }
  val fields = new SheetViewFieldsConfig(listFields.toArray)
  
  def getFieldValueMap() = _fieldValueMap
  def getFieldsNames() = _fieldValueMap.keys.toArray  

  private val _fieldValueMap = {
    require( rsm.resultSet.isDefined, "a result set must loaded and attached to the result summary")
    
    val rsmValResultsOpt = rsm.properties.flatMap( _.getValidationProperties.map( _.getResults ) )
    val psmValResultsAsStr = _stringifyValResults( rsmValResultsOpt.flatMap( _.getPeptideResults ) )
    val protValResultsAsStr = _stringifyValResults( rsmValResultsOpt.flatMap( _.getProteinResults ) )
    
    val rs = rsm.resultSet.get
    val pepMatches = rs.peptideMatches
    val allPrecursorCharges = pepMatches.groupBy(pm => pm.peptide.id + pm.msQuery.charge).map(_._2.head.msQuery.charge)
    val z2Count = allPrecursorCharges.count( _ == 2 )
    val z3Count = allPrecursorCharges.count( _ == 3 )
    val distinctSeqCount = rs.peptides.map(_.sequence).distinct.length
    val pepCount = rs.peptides.length
    val modPepCount = rs.peptides.count(_.ptmString.isEmpty == false)
    val unmodPepCount = pepCount - modPepCount
    val protSetCount = rsm.proteinSets.count(_.isValidated == true)

    
    // --- Group specific peptide sequences by protein sets ---
    val pepInstById = rsm.getPeptideInstanceById    
    val speSeqsByProtSet = new HashMap[ProteinSet,ArrayBuffer[String]]
    
    for(
      protSet <- rsm.proteinSets;
      item <- protSet.peptideSet.items      
    ) {
      val pepInst = item.peptideInstance
      
      if( pepInst.isProteinSetSpecific ) {
        speSeqsByProtSet.getOrElseUpdate(protSet, new ArrayBuffer[String]) += pepInst.peptide.sequence
      }
      
    }
    
    // --- Count the number of protein sets having a single or multiple specific peptide sequences ---
    val speSeqsCounts = for( (protSet,speSeqs) <- speSeqsByProtSet ) yield speSeqs.distinct.length
    val singleSpeSeqProtSetCount = speSeqsCounts.count(_ == 1)
    val multiSpeSeqsProtSetCount = speSeqsCounts.size - singleSpeSeqProtSetCount
    
     var exportMap:ListMap[String,Any] = ListMap()
    
    
    val listFields :Array[ExportConfigField] = sheetConfig.fields
    for ( f <- listFields ) {
      f.id match{
        case ExportConfigConstant.FIELD_STAT_NB_PROTEIN_SETS => {
          exportMap += ( fields.addField(f.title) -> protSetCount)
        }
        case ExportConfigConstant.FIELD_STAT_PSM_VALIDATION => {
          exportMap += ( fields.addField(f.title) -> psmValResultsAsStr)
        }
        case ExportConfigConstant.FIELD_STAT_NB_TOTAL_PRECURSORS => {
          exportMap += ( fields.addField(f.title) -> allPrecursorCharges.size)
        }
        case ExportConfigConstant.FIELD_STAT_NB_PROTEIN_SETS_SINGLE_SPECIFIC_PEPTIDE => {
           exportMap += ( fields.addField(f.title) -> singleSpeSeqProtSetCount)
        }
        case ExportConfigConstant.FIELD_STAT_NB_MODIFIED_PEPTIDES => {
           exportMap += ( fields.addField(f.title) -> modPepCount)
        }
        case ExportConfigConstant.FIELD_STAT_NB_Z3_PRECURSORS => {
           exportMap += ( fields.addField(f.title) -> z3Count)
        }
        case ExportConfigConstant.FIELD_STAT_NB_UNMODIFIED_PEPTIDES => {
           exportMap += ( fields.addField(f.title) -> unmodPepCount )
        }
        case ExportConfigConstant.FIELD_STAT_NB_PROTEIN_SETS_MULTI_SPECIFIC_PEPTIDE => {
           exportMap += ( fields.addField(f.title) -> multiSpeSeqsProtSetCount)
        }
        case ExportConfigConstant.FIELD_STAT_NB_Z2_PRECURSORS => {
           exportMap += ( fields.addField(f.title) -> z2Count)
        }
        case ExportConfigConstant.FIELD_STAT_NB_PEPTIDES => {
           exportMap += ( fields.addField(f.title) -> pepCount)
        }
        case ExportConfigConstant.FIELD_STAT_NB_DISTINCT_SEQ => {
           exportMap += ( fields.addField(f.title) -> distinctSeqCount)
        }
        case ExportConfigConstant.FIELD_STAT_PROT_VALIDATION => {
           exportMap += ( fields.addField(f.title) -> protValResultsAsStr)
        }
        case other => {
          // should not happen
        }
      }
    }
    //exportMap.map( r => r._1.toString -> r._2)
    exportMap
    
  }
  
  private def _stringifyValResults( valResultsOpt: Option[RsmValidationResultProperties] ): String = {
    if( valResultsOpt.isEmpty  ) ""
    else {
      val valResults = valResultsOpt.get
      val resultsBuffer = new ArrayBuffer[String](3)
      
      resultsBuffer += "TARGET matches count: " + valResults.getTargetMatchesCount
      
      if ( valResults.getDecoyMatchesCount.isDefined )
        resultsBuffer += "DECOY matches count: " + valResults.getDecoyMatchesCount.get
      
      if ( valResults.getFdr.isDefined )
        resultsBuffer += "FDR: " + valResults.getFdr.get
      
      resultsBuffer.mkString("; ")
    }
  }
}