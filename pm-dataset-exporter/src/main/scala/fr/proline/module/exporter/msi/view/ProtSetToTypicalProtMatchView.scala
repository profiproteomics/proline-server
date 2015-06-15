package fr.proline.module.exporter.msi.view

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._

trait IProtSetToToTypicalProtMatchViewFields extends IViewFieldEnumeration {
  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val DESCRIPTION = Field("description")
  val PROTEIN_SET_SCORE = Field("protein_set_score")
  val IS_PROTEIN_SET_VALIDATED = Field("is_protein_set_validated")
  val SAMESET_PROTEIN_MATCHES_COUNT = Field("#sameset_protein_matches")
  val SUBSET_PROTEIN_MATCHES_COUNT = Field("#subset_protein_matches")  
  val COVERAGE = Field("coverage")
  val MW = Field("MW")
  val SEQUENCES_COUNT = Field("#sequences")
  val SPECIFIC_SEQUENCES_COUNT = Field("#specific_sequences")
  val PEPTIDES_COUNT = Field("#peptides")
  val SPECIFIC_PEPTIDES_COUNT = Field("#specific_peptides")
  val PEPTIDE_MATCHES_COUNT = Field("#peptide_matches")
  val SPECIFIC_PEPTIDE_MATCHES_COUNT = Field("#specific_peptide_matches")
}

object ProtSetToToTypicalProtMatchViewFields extends IProtSetToToTypicalProtMatchViewFields

abstract class AbstractProtSetToTypicalProtMatchView extends IFixedDatasetView {
  
  val identDS: IdentDataSet
  private val protSetFields = ProtSetToToTypicalProtMatchViewFields
  
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val buildingCtx = buildingContext.asInstanceOf[ProtMatchBuildingContext]
    val protSet = buildingCtx.protSet
    val protMatch = buildingCtx.protMatch // here it is the typical protein match
    
    
    Map(
      protSetFields.PROTEIN_SET_ID -> protSet.id,
      protSetFields.ACCESSION -> protMatch.accession,
      protSetFields.DESCRIPTION -> protMatch.description,
      protSetFields.PROTEIN_SET_SCORE -> "%.1f".format(protSet.peptideSet.score).toDouble,
      protSetFields.IS_PROTEIN_SET_VALIDATED -> protSet.isValidated.toString,
      protSetFields.SAMESET_PROTEIN_MATCHES_COUNT -> protSet.getSameSetProteinMatchIds.length,
      protSetFields.SUBSET_PROTEIN_MATCHES_COUNT -> protSet.getSubSetProteinMatchIds.length,
      protSetFields.COVERAGE -> "%.1f".format(protMatch.coverage).toDouble,
      protSetFields.MW -> Option(protMatch.protein).flatMap( _.map( _.mass ) ).getOrElse(0.0),
      protSetFields.SEQUENCES_COUNT -> buildingCtx.allSeqs.distinct.length,
      protSetFields.SPECIFIC_SEQUENCES_COUNT -> buildingCtx.specificSeqs.distinct.length,
      protSetFields.PEPTIDES_COUNT -> buildingCtx.peptideCount,
      protSetFields.SPECIFIC_PEPTIDES_COUNT -> buildingCtx.specificPeps.length,
      protSetFields.PEPTIDE_MATCHES_COUNT -> protSet.peptideSet.peptideMatchesCount,
      protSetFields.SPECIFIC_PEPTIDE_MATCHES_COUNT -> buildingCtx.specificPepMatchIds.length
    ).map( r => r._1.toString -> r._2 )
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    
    // Go through protein sets
    for( protSet <- rsm.proteinSets ) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      
      // Typical Protein Match is put first
      val typicalProtMatchId = protSet.getRepresentativeProteinMatchId
      
      val typicalProtMatch = if( typicalProtMatchId != 0 ) { 
        protMatchById(typicalProtMatchId)
      } else {
        protMatchById( protSet.getSameSetProteinMatchIds.head )
      }
      
      val buildingContext = new ProtMatchBuildingContext(
        protSet,
        protSet.peptideSet,
        typicalProtMatch
      )
      
      this.formatRecord( buildingContext, recordFormatter )
    }
  }
  
}

class ProtSetToTypicalProtMatchView( val identDS: IdentDataSet ) extends AbstractProtSetToTypicalProtMatchView {
  
  var viewName = "prot_set_to_typical_prot_match"
  val fields = ProtSetToToTypicalProtMatchViewFields
  
}