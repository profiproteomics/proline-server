package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._

object ProtSetToProtMatchViewFields extends IViewFieldEnumeration {
  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val DESCRIPTION = Field("description")
  val GENE_NAME = Field("gene_name")
  val TAXON_ID = Field("taxon_id")
  val PROTEIN_SET_SCORE = Field("protein_set_score")
  val PROTEIN_MATCHES_COUNT = Field("#protein_matches")
  val PEPTIDE_MATCHES_COUNT = Field("#peptide_matches")
  val COVERAGE = Field("coverage")
}

class ProtSetToProtMatchView( rsm: ResultSummary ) extends IDatasetView {

  var viewName = "prot_set_to_prot_match"
  val fields = ProtSetToProtMatchViewFields
  
  case class MyBuildingContext( protSet: ProteinSet, protMatch: ProteinMatch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val protSet = myBuildingContext.protSet
    val protMatch = myBuildingContext.protMatch
    
    Map(
      fields.PROTEIN_SET_ID -> protSet.id,
      fields.PROTEIN_SET_SCORE -> protSet.peptideSet.score,
      fields.PROTEIN_MATCHES_COUNT -> protSet.proteinMatchIds.length,
      fields.ACCESSION -> protMatch.accession,
      fields.DESCRIPTION -> protMatch.description,
      fields.GENE_NAME -> protMatch.geneName,
      fields.TAXON_ID -> protMatch.taxonId,
      fields.COVERAGE -> protMatch.coverage,
      fields.PEPTIDE_MATCHES_COUNT -> protMatch.peptideMatchesCount
    ).map( r => r._1.toString -> r._2)
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    
    for( protSet <- rsm.proteinSets ) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      for( protMatchId <- protSet.proteinMatchIds; protMatch <- protMatchById.get(protMatchId) ) {
        this.formatRecord(MyBuildingContext(protSet,protMatch), recordFormatter)
      }
    }
  }

}