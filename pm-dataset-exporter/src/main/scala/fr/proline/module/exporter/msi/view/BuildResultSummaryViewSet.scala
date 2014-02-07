package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.template._
import fr.proline.context.IExecutionContext
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideInstanceProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideSetProvider
import scala.collection.mutable.ArrayBuffer

object BuildResultSummaryViewSet {

  def apply(rsm: ResultSummary, viewSetName: String, viewSetTemplate: IViewSetTemplate ): ResultSummaryViewSet = {
    
    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate( BuildResultSummaryView(rsm,templatedViewType.viewType), templatedViewType.template )
      if( templatedViewType.viewName.isDefined ) viewWithTpl.datasetView.viewName = templatedViewType.viewName.get
      
      viewWithTpl
    }
    
    new ResultSummaryViewSet(viewSetName,templatedViews)
  }
  
  def apply(executionContext: IExecutionContext, rsmId: Long, peptideSetExport : Boolean, viewSetName: String, viewSetTemplate: IViewSetTemplate ): ResultSummaryViewSet = {
    
    var udsSQLCtx: DatabaseConnectionContext = null
    var psSQLCtx: DatabaseConnectionContext = null
    var msiSQLCtx: DatabaseConnectionContext = null

    udsSQLCtx = executionContext.getUDSDbConnectionContext()
    psSQLCtx = executionContext.getPSDbConnectionContext()
    msiSQLCtx = executionContext.getMSIDbConnectionContext()

    val rsmProvider = new SQLResultSummaryProvider(msiSQLCtx, psSQLCtx, udsSQLCtx)
    val rsm = rsmProvider.getResultSummary(rsmId, true).get

    
    //
    // Load all Subsets of Protein Sets
    //
    if (peptideSetExport) {

      // Instantiate additional providers
      val pepProvider = new SQLPeptideProvider(executionContext.getPSDbConnectionContext())
      val pepInstProvider = new SQLPeptideInstanceProvider(executionContext.getMSIDbConnectionContext, pepProvider)
      val pepSetProvider = new SQLPeptideSetProvider(executionContext.getMSIDbConnectionContext, pepInstProvider)

      // Retrieve all subsets ids
      val allSubsetIds = new ArrayBuffer[Long]
      rsm.peptideSets.map { peptideSet =>
        val strictSubsetIds = Option(peptideSet.strictSubsetIds).getOrElse(Array())
        val subsumableSubsetIds = Option(peptideSet.subsumableSubsetIds).getOrElse(Array())
        allSubsetIds ++= strictSubsetIds ++ subsumableSubsetIds
      }

      // Load all subsets
      val allSubsets = pepSetProvider.getPeptideSets(allSubsetIds.distinct)
      val subsetById = allSubsets.map(ps => ps.id -> ps).toMap
      rsm.peptideSets.map { peptideSet =>
        if (peptideSet.strictSubsetIds != null && peptideSet.strictSubsets == null) {
          peptideSet.strictSubsets = Some(peptideSet.strictSubsetIds.map(subsetById(_)))
        }
        if (peptideSet.subsumableSubsetIds != null && peptideSet.subsumableSubsets == null) {
          peptideSet.subsumableSubsets = Some(peptideSet.subsumableSubsetIds.map(subsetById(_)))
        }
      }
    }
    
    return apply(rsm, viewSetName, viewSetTemplate)

  }
  
 
  
  
}