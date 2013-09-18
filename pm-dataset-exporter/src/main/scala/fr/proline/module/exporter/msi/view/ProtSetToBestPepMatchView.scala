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
  val SEQUENCE = Field("sequence")
  val CALCULATED_MASS = Field("calculated_mass")
  val MISSED_CLEAVAGES = Field("missed_cleavages")
  val DELTA_MOZ = Field("delta_moz")
  val FRAGMENT_MATCHES_COUNT = Field("fragment_matches_count")  
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
        
        val protMatchRecord = this.buildRecord(myBuildingCtx)
        val record = protMatchRecord ++ Map(
          fields.DELTA_MOZ -> pepMatch.deltaMoz,
          fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
          fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage,
          fields.SEQUENCE -> peptide.sequence,
          fields.CALCULATED_MASS -> peptide.calculatedMass
        ).map( r => r._1.toString -> r._2)
        
        recordFormatter( record )
      }
    }
  }
  

  /*
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    val pepMatchById = rs.peptideMatchById    
    
    for( protSet <- rsm.proteinSets ) {
      
      // Retrieve the typical protein match
      val typicalProtMatchId = protSet.getTypicalProteinMatchId
      val typicalProtMatch = if( typicalProtMatchId != 0 ) protMatchById(typicalProtMatchId)
      else protMatchById( protSet.proteinMatchIds(0) )
      
      typicalProtMatch.sequenceMatches.foreach { seqMatch =>
        
        val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
        
        // TODO: add seq match data
        for( pepMatch <- pepMatchOpt ) {
          val peptide = pepMatch.peptide
          
          val record = protSetAndProtMatchToRecord(protSet,typicalProtMatch,pepSetById)
          val record = protMatchRecord ++ Map(
            fields.DELTA_MOZ -> pepMatch.deltaMoz,
            fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
            fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage,
            fields.SEQUENCE -> peptide.sequence,
            fields.CALCULATED_MASS -> peptide.calculatedMass
          ).map( r => r._1.toString -> r._2)
          
          recordFormatter( record )
        }
      }
      
    }
    
  }*/

}