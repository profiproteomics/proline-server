package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view._

// TODO: remove me (replaced by applying the appropriate set of selected fields in corresponding template)
object ProtSetToBestPepMatchIrmaViewFields extends IViewFieldEnumeration {
  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val SEQUENCE = Field("sequence")
  val PEPMATCH_SCORE = Field("score")
  val CALCULATED_MASS = Field("calculated_mass")
  val MISSED_CLEAVAGES = Field("missed_cleavages")
  val EXPERIMENTAL_MOZ = Field("experimental_moz")
  val DELTA_MOZ = Field("delta_moz")
  val RANK = Field("rank")
  val FRAGMENT_MATCHES_COUNT = Field("fragment_matches_count")  
}


class ProtSetToBestPepMatchIrmaView( rsm: ResultSummary ) extends IDatasetView {

  var viewName = "prot_set_to_best_peptide_match_irma"
  val fields = ProtSetToBestPepMatchIrmaViewFields
  
  case class MyBuildingContext( protSet: ProteinSet, protMatch: ProteinMatch, pepMatch: PeptideMatch, peptide: Peptide ) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val protSet = myBuildingContext.protSet
    val protMatch = myBuildingContext.protMatch

    val pepMatch = myBuildingContext.pepMatch
    val peptide = myBuildingContext.peptide
    
    val experimentalMoz = peptide.calculatedMass+ pepMatch.deltaMoz
    
    Map(
      fields.PROTEIN_SET_ID -> protSet.id,
      fields.ACCESSION -> protMatch.accession,
      fields.SEQUENCE -> peptide.sequence,
      fields.PEPMATCH_SCORE -> pepMatch.score,
      fields.CALCULATED_MASS -> peptide.calculatedMass,
      fields.EXPERIMENTAL_MOZ -> experimentalMoz,
      fields.DELTA_MOZ -> pepMatch.deltaMoz,
      fields.RANK -> pepMatch.rank,
      fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
      fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage
      
      
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
      
      val sequenceMatches = typicalProtMatch.sequenceMatches
      val pepMatchById = rs.peptideMatchById  
      
      for (seqMatch <- sequenceMatches) {
        val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId)
        
        for( pepMatch <- pepMatchOpt ) {
          val peptide = pepMatch.peptide
          this.formatRecord(MyBuildingContext(protSet, typicalProtMatch, pepMatch, peptide), recordFormatter)
        }
      }

      
    }
  }
  
}
  