package fr.proline.module.exporter.msi.view

import scala.collection.mutable.ArrayBuffer

import fr.proline.context.DatabaseConnectionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.provider.msi.impl._
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IDatasetView

object BuildResultSummaryViewSet {

  def apply(ds: DataSet, viewSetName: String, viewSetTemplate: IViewSetTemplate ): ResultSummaryViewSet = {
    
    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate( BuildResultSummaryView(ds,templatedViewType.viewType), templatedViewType.template )
      if( templatedViewType.viewName.isDefined ) viewWithTpl.datasetView.viewName = templatedViewType.viewName.get
      
      viewWithTpl
    }
    
    new ResultSummaryViewSet(viewSetName,templatedViews)
  }
  
  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    loadSubsets: Boolean,
    loadFullResultSet: Boolean,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate
  ): ResultSummaryViewSet = {
    
    var udsSQLCtx: DatabaseConnectionContext = null
    var psSQLCtx: DatabaseConnectionContext = null
    var msiSQLCtx: DatabaseConnectionContext = null

    udsSQLCtx = executionContext.getUDSDbConnectionContext()
    psSQLCtx = executionContext.getPSDbConnectionContext()
    msiSQLCtx = executionContext.getMSIDbConnectionContext()
    
    // FIXME: load the project name
    val projectName = ""

    val rsmProvider = new SQLResultSummaryProvider(msiSQLCtx, psSQLCtx, udsSQLCtx)
    
    val rsm = if( loadFullResultSet == false ) rsmProvider.getResultSummary(rsmId, true).get
    else {
      val tmpRsm = rsmProvider.getResultSummary(rsmId, false).get
      val rsProvider = new SQLResultSetProvider(msiSQLCtx, psSQLCtx, udsSQLCtx)
      tmpRsm.resultSet = rsProvider.getResultSet(tmpRsm.getResultSetId)
      tmpRsm
    }
    
    //
    // Load all Subsets of Protein Sets
    //
    if (loadSubsets) {

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
    
    return apply( DataSet( projectName, rsm), viewSetName, viewSetTemplate)

  }
  
 
  
  
}