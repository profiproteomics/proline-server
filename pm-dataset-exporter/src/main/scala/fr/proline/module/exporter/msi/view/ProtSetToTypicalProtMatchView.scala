package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._

trait IProtSetToToTypicalProtMatchViewFields extends IViewFieldEnumeration {
  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val DESCRIPTION = Field("description")
  val GENE_NAME = Field("gene_name")
  val TAXON_ID = Field("taxon_id")
  val PROTEIN_SET_SCORE = Field("protein_set_score")
  val PROTEIN_MATCHES_COUNT = Field("#protein_matches")
  val SUBSET_PROTEIN_MATCHES_COUNT = Field("#subset_protein_matches")
  val PEPTIDE_MATCHES_COUNT = Field("#peptide_matches")
  val COVERAGE = Field("coverage")
  val MW = Field("MW")
}

object ProtSetToToTypicalProtMatchViewFields extends IProtSetToToTypicalProtMatchViewFields

abstract class AbstractProtSetToTypicalProtMatchView extends IDatasetView {
  
  val rsm: ResultSummary
  private val protSetFields = ProtSetToToTypicalProtMatchViewFields
  
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingCtx = buildingContext.asInstanceOf[ProtSetToToTypicalProtMatchBuildingContext]
    val protSet = myBuildingCtx.protSet
    val protMatch = myBuildingCtx.typicalProtMatch
    val pepSetById = myBuildingCtx.pepSetById
    
    val subsetPepSets = Option( protSet.peptideSet.strictSubsetIds ).map( _.map( pepSetById(_) ) ).getOrElse(Array.empty[PeptideSet])
    val subsetProtMatchesCount = subsetPepSets.flatMap( _.proteinMatchIds ).length
    
    Map(
      protSetFields.PROTEIN_SET_ID -> protSet.id,
      protSetFields.PROTEIN_SET_SCORE -> protSet.peptideSet.score,
      protSetFields.PROTEIN_MATCHES_COUNT -> protSet.getProteinMatchIds.length,
      protSetFields.SUBSET_PROTEIN_MATCHES_COUNT -> subsetProtMatchesCount,
      protSetFields.ACCESSION -> protMatch.accession,
      protSetFields.DESCRIPTION -> protMatch.description,
      protSetFields.GENE_NAME -> protMatch.geneName,
      protSetFields.TAXON_ID -> protMatch.taxonId,
      protSetFields.COVERAGE -> protMatch.coverage,
      protSetFields.PEPTIDE_MATCHES_COUNT -> protMatch.peptideMatchesCount,
      protSetFields.MW -> Option(protMatch.protein).map( _.map( _.mass ).getOrElse(0.0) ).getOrElse(0.0)
    ).map( r => r._1.toString -> r._2 )
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    val pepMatchById = rs.peptideMatchById    
    val pepSetById = Map() ++ rsm.peptideSets.map( ps => ps.id -> ps )
    
    for( protSet <- rsm.proteinSets ) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      
      // Typical Protein Match is put first
      val typicalProtMatchId = protSet.getTypicalProteinMatchId
      
      val typicalProtMatch = if( typicalProtMatchId != 0 ) { 
        protMatchById(typicalProtMatchId)
      } else {
        protMatchById( protSet.getSameSetProteinMatchIds.head )
      }
      
      val buildingContext = new ProtSetToToTypicalProtMatchBuildingContext(protSet,typicalProtMatch,pepMatchById,pepSetById)
      this.formatRecord( buildingContext, recordFormatter )
    }
  }
  
}

case class ProtSetToToTypicalProtMatchBuildingContext(
  protSet: ProteinSet,
  typicalProtMatch: ProteinMatch,
  pepMatchById: Map[Long,PeptideMatch],
  pepSetById: Map[Long,PeptideSet]
) extends IRecordBuildingContext

class ProtSetToTypicalProtMatchView( val rsm: ResultSummary ) extends AbstractProtSetToTypicalProtMatchView {
  
  var viewName = "prot_set_to_typical_prot_match"
  val fields = ProtSetToToTypicalProtMatchViewFields
  
}