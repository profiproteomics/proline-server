package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._

object ProtSetToProtMatchViewFields extends IViewFieldEnumeration {
  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val TYPICAL_PROTEIN = Field("typical protein")
  val SAME_SET = Field("same set")
  val DESCRIPTION = Field("description")
  val PROTEIN_MATCH_SCORE = Field("protein_match_score")
  val PEPTIDE_MATCHES_COUNT = Field("#peptide_matches")
  val GENE_NAME = Field("gene_name")
  val TAXON_ID = Field("taxon_id")
  //val PROTEIN_MATCHES_COUNT = Field("#protein_matches")
  val COVERAGE = Field("coverage")
}

class ProtSetToProtMatchView( rsm: ResultSummary ) extends IDatasetView {

  var viewName = "prot_set_to_prot_match"
  val fields = ProtSetToProtMatchViewFields
  
  case class MyBuildingContext( protSet: ProteinSet, protMatch: ProteinMatch, peptideSet: PeptideSet ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val protSet = myBuildingContext.protSet
    val protMatch = myBuildingContext.protMatch
    val peptideSet = myBuildingContext.peptideSet
    
    Map(
      fields.PROTEIN_SET_ID -> protSet.id,
      fields.PROTEIN_MATCH_SCORE -> protMatch.score,
      //fields.PROTEIN_MATCHES_COUNT -> protSet.proteinMatchIds.length,
      fields.ACCESSION -> protMatch.accession,
      fields.DESCRIPTION -> protMatch.description,
      fields.GENE_NAME -> protMatch.geneName,
      fields.TAXON_ID -> protMatch.taxonId,
      fields.COVERAGE -> protMatch.coverage,
      fields.PEPTIDE_MATCHES_COUNT -> peptideSet.peptideMatchesCount,
      fields.TYPICAL_PROTEIN -> (protSet.getTypicalProteinMatchId == protMatch.id),
      fields.SAME_SET -> !peptideSet.isSubset
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
      this.formatRecord(MyBuildingContext(protSet, typicalProtMatch, protSet.peptideSet), recordFormatter)

      // all peptide Sets
      val protMatchIdByPepSet : Map[PeptideSet, Array[Long]] = protSet.getAllProteinMatchesIdByPeptideSet
      
      // go through all the PeptideSet and sort them by score
      var peptideSetArray : Array[PeptideSet] = new Array[PeptideSet](protMatchIdByPepSet.size);
      var i = 0
      for( (peptideSet,protMatchIds) <- protMatchIdByPepSet) {
         peptideSetArray(i) = peptideSet
          i += 1
      }
      var peptideSetSortedArray = peptideSetArray.sortWith(_.score > _.score)
      

      // go through all peptide Set
      for (peptideSet <- peptideSetSortedArray) {
        // go through all PeptideMath of Peptide Set
        for( protMatchId <- peptideSet.proteinMatchIds; protMatch <- protMatchById.get(protMatchId) ) {
          if (protMatchId != typicalProteinMatchId) {
            this.formatRecord(MyBuildingContext(protSet, protMatch, peptideSet), recordFormatter)
          }
        }
      }
      
      
      // Put another Protein Match in an array and sort them by score
      /*var i = 0
      var protMatchArray : Array[ProteinMatch] = new Array[ProteinMatch](protSet.proteinMatchIds.length-1);
      for( protMatchId <- protSet.proteinMatchIds; protMatch <- protMatchById.get(protMatchId) ) {
        if (protMatchId != typicalProteinMatchId) {
          protMatchArray(i) = protMatchById.get(protMatchId).get
          i += 1
        }
      }
      var sortedArray = protMatchArray.sortWith(_.score > _.score)

      // Same Set and Sub Set Protein Matches
      for( protMatch <- sortedArray ) {
        
    	  this.formatRecord(MyBuildingContext(protSet, protMatch,  (protMatch.peptideMatchesCount == sameSetCount)), recordFormatter)
      }*/

      
    }
  }

}