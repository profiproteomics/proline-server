package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object StatisticsFields extends IViewFieldEnumeration {

  val PSM_VALIDATION_RESULTS = Field("psm_validation")
  val PROT_VALIDATION_RESULTS = Field("prot_validation")
  
  // VALID_PSM_COUNT is included into PSM_VALIDATION_COUNTS
  val Z2_PRECURSORS_COUNT = Field("#z2_precursors")
  val Z3_PRECURSORS_COUNT = Field("#z3_precursors")
  val ALL_PRECURSORS_COUNT = Field("#total_precursors")
  
  val DISTINCT_SEQUENCES_COUNT = Field("#distinct_sequences")
  val PEPTIDES_COUNT = Field("#peptides")
  val MODIFIED_PEPTIDES_COUNT = Field("#modified_peptides")
  val UNMODIFIED_PEPTIDES_COUNT = Field("#unmodified_peptides")
  
  val PROTEIN_SETS_COUNT = Field("#protein_sets")
  val SINGLE_SPECIFIC_SEQ_PROTEIN_SETS_COUNT = Field("#protein_sets_with_single_specific_peptide")
  val MULITPLE_SPECIFIC_SEQ_PROTEIN_SETS_COUNT = Field("#protein_sets_with_multiple_specific_peptides")
  
  // nb peptides par modifications => needs an other kind of views
  
}

class StatisticsView( val rsm: ResultSummary ) extends IDatasetView {
  
  var viewName = "stats"
  val fields = StatisticsFields
  
  def buildRecord( nullBuildingContext: IRecordBuildingContext ): Map[String,Any] = {    
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
    val pepInstById = rsm.peptideInstanceById    
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
    
    Map(
      fields.PSM_VALIDATION_RESULTS -> psmValResultsAsStr,
      fields.PROT_VALIDATION_RESULTS -> protValResultsAsStr,
      fields.Z2_PRECURSORS_COUNT -> z2Count,
      fields.Z3_PRECURSORS_COUNT -> z3Count,
      fields.ALL_PRECURSORS_COUNT -> allPrecursorCharges.size,
      fields.DISTINCT_SEQUENCES_COUNT -> distinctSeqCount,
      fields.PEPTIDES_COUNT -> pepCount,
      fields.MODIFIED_PEPTIDES_COUNT -> modPepCount,
      fields.UNMODIFIED_PEPTIDES_COUNT -> unmodPepCount,
      fields.PROTEIN_SETS_COUNT -> protSetCount,
      fields.SINGLE_SPECIFIC_SEQ_PROTEIN_SETS_COUNT -> singleSpeSeqProtSetCount,
      fields.MULITPLE_SPECIFIC_SEQ_PROTEIN_SETS_COUNT -> multiSpeSeqsProtSetCount
    ).map( r => r._1.toString -> r._2)
    
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {    
    this.formatRecord(null, recordFormatter)
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