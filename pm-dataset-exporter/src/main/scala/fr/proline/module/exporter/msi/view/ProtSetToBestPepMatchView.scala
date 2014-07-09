package fr.proline.module.exporter.msi.view

import java.io.File
import java.io.OutputStream
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.api.view.IRecordBuildingContext

object ProtSetToBestPepMatchViewFields extends IProtSetToToTypicalProtMatchViewFields {
  val PEPTIDE_ID = Field("peptide_id")
  val SEQUENCE = Field("sequence")  
  val MODIFICATIONS = Field("modifications")
  val MISSED_CLEAVAGES = Field("missed_cleavages")
  val RANK = Field("rank")
  val CD_PRETTY_RANK = Field("cd_pretty_rank")
  val PEPMATCH_SCORE = Field("score")
  //val IS_PSM_VALIDATED = Field("is_psm_validated")
  val CALCULATED_MASS = Field("calculated_mass")
  val CHARGE = Field("charge")
  val EXPERIMENTAL_MOZ = Field("experimental_moz")
  val DELTA_MOZ = Field("delta_moz")  
  val RT = Field("rt")
  val PEPTIDE_LENGTH = Field("peptide_length")
  val INITIAL_QUERY_ID = Field("initial_query_id")
  //val NOM_DU_MS_QUERY_DATASET = Field("NOM_DU_MS_QUERY_DATASET") => result file ???
  val FRAGMENT_MATCHES_COUNT = Field("fragment_matches_count")
  val SPECTRUM_TITLE = Field("spectrum_title")
  val PROTEIN_SETS_COUNT = Field("#protein_sets")
  val PROTEIN_MATCHES_COUNT = Field("#protein_matches")
  val START = Field("start")
  val END = Field("end")
  val RESIDUE_BEFORE = Field("residue_before")
  val RESIDUE_AFTER = Field("residue_after")
}

class ProtSetToBestPepMatchView( val rsm: ResultSummary ) extends AbstractProtSetToTypicalProtMatchView {
  
  var viewName = "prot_set_to_best_pep_match"
  override val fields = ProtSetToBestPepMatchViewFields
  
  override def formatRecord(
    buildingContext: IRecordBuildingContext,
    recordFormatter: Map[String,Any] => Unit
  ): Unit = {
    
    val myBuildingCtx = buildingContext.asInstanceOf[ProtSetToToTypicalProtMatchBuildingContext]
    val pepMatchById = myBuildingCtx.pepMatchById
    val protSetIdSetByPepMatchId = myBuildingCtx.protSetIdSetByPepMatchId
    val protMatchIdSetByPepMatchId = myBuildingCtx.protMatchIdSetByPepMatchId
    
    myBuildingCtx.typicalProtMatch.sequenceMatches.foreach { seqMatch =>
          
      val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
      
      for( pepMatch <- pepMatchOpt ) {
        
        // BEGIN OF CODE DUPLICATED WITH AllPepMatchesView //
        
        val peptide = pepMatch.peptide
        val initialQueryId = Option(pepMatch.msQuery).map(_.initialId).getOrElse(null)
        val experimentalMoz = Option(pepMatch.msQuery).map(_.moz).getOrElse(null)
        val resBefore = if( seqMatch.residueBefore == '\0' ) '-' else seqMatch.residueBefore
        val resAfter = if( seqMatch.residueAfter == '\0' ) '-' else seqMatch.residueAfter
        
        val protMatchRecord = this.buildRecord(myBuildingCtx)
        
        val record = protMatchRecord ++ Map(
          fields.PEPTIDE_ID -> peptide.id,
          fields.SEQUENCE -> peptide.sequence,
          fields.MODIFICATIONS -> peptide.readablePtmString,
          fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage,
          fields.RANK -> pepMatch.rank,
          fields.CD_PRETTY_RANK -> pepMatch.cdPrettyRank,
          fields.PEPMATCH_SCORE -> "%.1f".format(pepMatch.score).toDouble,
          //fields.IS_PSM_VALIDATED -> pepMatch.isValidated,
          fields.CALCULATED_MASS -> peptide.calculatedMass,
          fields.CHARGE -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null),
          fields.EXPERIMENTAL_MOZ -> experimentalMoz,
          fields.DELTA_MOZ -> pepMatch.deltaMoz, // FIXME: to convert in PPM we need the experimentalMoz and thus the msQuery
          //fields.RT -> pepMatch.,
          fields.PEPTIDE_LENGTH -> peptide.sequence.length,
          fields.INITIAL_QUERY_ID -> initialQueryId,
          fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
          fields.SPECTRUM_TITLE -> Option(pepMatch.getMs2Query).map( _.spectrumTitle ).getOrElse(""),
          fields.PROTEIN_SETS_COUNT -> protSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
          fields.PROTEIN_MATCHES_COUNT -> protMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
          fields.START -> seqMatch.start,
          fields.END -> seqMatch.end,
          fields.RESIDUE_BEFORE -> resBefore,
          fields.RESIDUE_AFTER -> resAfter
        ).map( r => r._1.toString -> r._2)
        
        // END OF CODE DUPLICATED WITH AllPepMatchesView //
        
        recordFormatter( record )
      }
    }
  }
  

  // def onEachRecord is inherited from AbstractProtSetToTypicalProtMatchView

}