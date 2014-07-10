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

class ProtSetToBestPepMatchView( val identDS: IdentDataSet ) extends AbstractPeptideMatchView {
  
  var viewName = "prot_set_to_best_pep_match"
  override val fields = ProtSetToPepMatchViewFields
  
  // TODO: override buildRecord instead ???
  override def formatRecord(
    buildingContext: IRecordBuildingContext,
    recordFormatter: Map[String,Any] => Unit
  ): Unit = {
    
    val myBuildingCtx = buildingContext.asInstanceOf[ProtMatchBuildingContext]
    val pepMatchById = identDS.pepMatchById
    val protSetIdSetByPepMatchId = identDS.protSetIdSetByPepMatchId
    val protMatchIdSetByPepMatchId = identDS.protMatchIdSetByPepMatchId
    
    myBuildingCtx.protMatch.sequenceMatches.foreach { seqMatch =>
          
      val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
      
      for( pepMatch <- pepMatchOpt ) {
        
        // Build the protein match record
        val protMatchRecord = this.buildRecord(myBuildingCtx)
        
        // Build the peptide match record
        val pepMatchRecord = this.buildPepMatchRecord(protMatchRecord, pepMatch, seqMatch)        
        
        recordFormatter( pepMatchRecord )
      }
    }
  }

  // def onEachRecord is inherited from AbstractProtSetToTypicalProtMatchView

}