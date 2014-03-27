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
  val START = Field("start")
  val END = Field("end")
  val RESIDUE_BEFORE = Field("residue_before")
  val SEQUENCE = Field("sequence")
  val RESIDUE_AFTER = Field("residue_after")
  val MODIFICATIONS = Field("modifications")
  val MISSED_CLEAVAGES = Field("missed_cleavages")
  val RANK = Field("rank")
  val PEPMATCH_SCORE = Field("score")
  val CALCULATED_MASS = Field("calculated_mass")  
  val CHARGE = Field("charge") 
  val EXPERIMENTAL_MOZ = Field("experimental_moz")
  val DELTA_MOZ = Field("delta_moz")  
  val FRAGMENT_MATCHES_COUNT = Field("fragment_matches_count")
  val SPECTRUM_TITLE = Field("spectrum_title")
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
    myBuildingCtx.typicalProtMatch.sequenceMatches.foreach { seqMatch =>
          
      val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
      
      // TODO: add seq match data
      for( pepMatch <- pepMatchOpt ) {
        
        val peptide = pepMatch.peptide
        val experimentalMoz = Option(pepMatch.msQuery).map(_.moz).getOrElse(null)  
        val resBefore = if( seqMatch.residueBefore == '\0' ) '-' else seqMatch.residueBefore
        val resAfter = if( seqMatch.residueAfter == '\0' ) '-' else seqMatch.residueAfter
        
        val protMatchRecord = this.buildRecord(myBuildingCtx)
        
        val record = protMatchRecord ++ Map(
          fields.START -> seqMatch.start,
          fields.END -> seqMatch.end,
          fields.SEQUENCE -> peptide.sequence,
          fields.RESIDUE_BEFORE -> resBefore,
          fields.RESIDUE_AFTER -> resAfter,
          fields.MODIFICATIONS -> peptide.readablePtmString,
          fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage,
          fields.RANK -> pepMatch.rank,
          fields.PEPMATCH_SCORE -> pepMatch.score,
          fields.CALCULATED_MASS -> peptide.calculatedMass,
          fields.CHARGE -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null),
          fields.EXPERIMENTAL_MOZ -> experimentalMoz,
          fields.DELTA_MOZ -> pepMatch.deltaMoz,
          fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
          fields.SPECTRUM_TITLE -> Option(pepMatch.getMs2Query).map( _.spectrumTitle ).getOrElse("")
        ).map( r => r._1.toString -> r._2)
        
        recordFormatter( record )
      }
    }
  }
  

  // def onEachRecord is inherited from AbstractProtSetToTypicalProtMatchView

}