package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._
import scala.collection.mutable.ArrayBuffer

object ProtSetToProtMatchViewFields extends IViewFieldEnumeration {
  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val DESCRIPTION = Field("description")
  val IS_TYPICAL_PROTEIN = Field("is_typical_protein")
  val IS_SAMESET = Field("is_sameset")
  val PROTEIN_MATCH_SCORE = Field("protein_match_score")
  //val GENE_NAME = Field("gene_name")
  //val TAXON_ID = Field("taxon_id")
  val COVERAGE = Field("coverage")
  val MW = Field("MW")
  val SEQUENCES_COUNT = Field("#sequences")
  val SPECIFIC_SEQUENCES_COUNT = Field("#specific_sequences")
  val PEPTIDES_COUNT = Field("#peptides")
  val SPECIFIC_PEPTIDES_COUNT = Field("#specific_peptides")
  val PEPTIDE_MATCHES_COUNT = Field("#peptide_matches")
  val SPECIFIC_PEPTIDE_MATCHES_COUNT = Field("#specific_peptide_matches")
}

class ProtSetToProtMatchView( rsm: ResultSummary ) extends IDatasetView {

  var viewName = "prot_set_to_prot_match"
  val fields = ProtSetToProtMatchViewFields
  
  case class MyBuildingContext( protSet: ProteinSet, peptideSet: PeptideSet, protMatch: ProteinMatch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val protSet = myBuildingContext.protSet
    val protMatch = myBuildingContext.protMatch
    val peptideSet = myBuildingContext.peptideSet
    
    // --- BEGIN OF CODE DUPLICATED WITH AbstractProtSetToTypicalProtMatchView ---
    // TODO: put this in a trait or a singleton ???
    val pepCount = peptideSet.items.length
    val speSeqs = new ArrayBuffer[String]( pepCount )
    val spePeps = new ArrayBuffer[Peptide]( pepCount )
    val spePsmsIds = new ArrayBuffer[Long]( pepCount )
    
    for(item <- protSet.peptideSet.items ) {
      val pepInst = item.peptideInstance
      
      if( pepInst.isProteinSetSpecific ) {        
        speSeqs += pepInst.peptide.sequence
        spePeps += pepInst.peptide
        spePsmsIds ++= pepInst.getPeptideMatchIds
      }
    }
    // --- END OF CODE DUPLICATED WITH AbstractProtSetToTypicalProtMatchView ---
    
    Map(
      fields.PROTEIN_SET_ID -> protSet.id,
      fields.ACCESSION -> protMatch.accession,
      fields.DESCRIPTION -> protMatch.description,
      fields.IS_TYPICAL_PROTEIN -> (protSet.getTypicalProteinMatchId == protMatch.id),
      fields.IS_SAMESET -> !peptideSet.isSubset,
      fields.PROTEIN_MATCH_SCORE -> "%.1f".format(protMatch.score).toDouble,
      //fields.GENE_NAME -> protMatch.geneName,
      //fields.TAXON_ID -> protMatch.taxonId,
      fields.COVERAGE -> "%.1f".format(protMatch.coverage).toDouble,
      fields.MW -> Option(protMatch.protein).flatMap( _.map( _.mass ) ).getOrElse(0.0),
      fields.PEPTIDE_MATCHES_COUNT -> peptideSet.peptideMatchesCount,
      fields.SEQUENCES_COUNT -> protMatch.sequenceMatches.length,
      fields.SPECIFIC_SEQUENCES_COUNT -> speSeqs.distinct.length,
      fields.PEPTIDES_COUNT -> pepCount,
      fields.SPECIFIC_PEPTIDES_COUNT -> spePeps.length,
      fields.PEPTIDE_MATCHES_COUNT -> protMatch.peptideMatchesCount,
      fields.SPECIFIC_PEPTIDE_MATCHES_COUNT -> spePsmsIds.length
    ).map( r => r._1.toString -> r._2)
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    
    for( protSet <- rsm.proteinSets ) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      
      // Typical Protein Match is put first
      val typicalProteinMatchId = protSet.getTypicalProteinMatchId
      val typicalProtMatch = protMatchById.get(typicalProteinMatchId).get
      this.formatRecord(MyBuildingContext(protSet, protSet.peptideSet, typicalProtMatch ), recordFormatter)

      // Sort strict subsets by descending score
      val strictSubsetsSortedByDescScore = protSet.peptideSet.strictSubsets.get.sortWith(_.score > _.score)
     
      for (
        // Go through all peptide set
        peptideSet <- strictSubsetsSortedByDescScore;
        // Go through all peptide matches of the peptide set
        protMatchId <- peptideSet.proteinMatchIds;
        // Retrieve the protein match
        protMatch <- protMatchById.get(protMatchId)
      ) {
         this.formatRecord(MyBuildingContext(protSet, peptideSet, protMatch ), recordFormatter)
      }
      
    }
  }

}