package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._

object PepSetToProtMatchViewFields extends IViewFieldEnumeration {
  val PEPTIDE_SET_ID = Field("peptide_set_id")
  val ACCESSION = Field("accession")
  val DESCRIPTION = Field("description")
  val GENE_NAME = Field("gene_name")
  val TAXON_ID = Field("taxon_id")
  val PEPTIDE_SET_SCORE = Field("peptide_set_score")
  val PROTEIN_MATCHES_COUNT = Field("#protein_matches")
  val PEPTIDE_MATCHES_COUNT = Field("#peptide_matches")
  val COVERAGE = Field("coverage")
  
}

// TODO: remove this view ???
class PepSetToProtMatchView( val identDS: IdentDataSet ) extends IFixedDatasetView {

  var viewName = "pep_set_to_prot_match"
  val fields = PepSetToProtMatchViewFields
  
  case class MyBuildingContext( pepSet: PeptideSet, protMatch: ProteinMatch ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val pepSet = myBuildingContext.pepSet
    val protMatch = myBuildingContext.protMatch
    
    Map(
      fields.PEPTIDE_SET_ID -> pepSet.id,
      fields.PEPTIDE_SET_SCORE -> pepSet.score,
      fields.PROTEIN_MATCHES_COUNT -> pepSet.proteinMatchIds.length,
      fields.ACCESSION -> protMatch.accession,
      fields.DESCRIPTION -> protMatch.description,
      fields.GENE_NAME -> protMatch.geneName,
      fields.TAXON_ID -> protMatch.taxonId,
      fields.COVERAGE -> protMatch.coverage,
      fields.PEPTIDE_MATCHES_COUNT -> protMatch.peptideMatchesCount
    ).map( r => r._1.toString -> r._2)
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    
    for( pepSet <- rsm.peptideSets ) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      for( protMatchId <- pepSet.proteinMatchIds; protMatch <- protMatchById.get(protMatchId) ) {
        this.formatRecord(MyBuildingContext(pepSet,protMatch), recordFormatter)
      }
    }
  }

}