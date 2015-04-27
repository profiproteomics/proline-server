package fr.proline.module.exporter.msi.view

import scala.collection.mutable.ArrayBuffer
import fr.profi.jdbc.easy._
import fr.proline.context.DatabaseConnectionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.dal.tables.SelectQueryBuilder1
import fr.proline.core.dal.tables.SelectQueryBuilder._
import fr.proline.core.dal.tables.uds.UdsDbProjectTable
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.core.dal.DoJDBCReturningWork
import java.sql.Connection
import fr.proline.repository.util.JDBCWork
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.template.ProlineConfigViewSetTemplateAsXLSX
import fr.proline.module.exporter.commons.config.ExportConfigManager
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.commons.config.template.ProlineConfigViewSetTemplateAsTSV

object BuildResultSummaryViewSet {

  def apply(ds: IdentDataSet, viewSetName: String, viewSetTemplate: IViewSetTemplate, exportConfig :ExportConfig): ResultSummaryViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate(BuildResultSummaryView(ds, templatedViewType.viewType, exportConfig), templatedViewType.template)
      if (templatedViewType.viewName.isDefined) viewWithTpl.dataView.viewName = templatedViewType.viewName.get

      viewWithTpl
    }

    new ResultSummaryViewSet(viewSetName, templatedViews, exportConfig)
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
    return apply(executionContext, projectId, rsmId,loadSubsets, loadFullResultSet, viewSetName, viewSetTemplate, null )
  }
  
  
  
  // build template from the  config
  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    viewSetName: String,
    exportConfigStr: String
  ): ResultSummaryViewSet = {

    val exportConfig : ExportConfig = ExportConfigManager.readConfig(exportConfigStr)
    val loadFullResultSet:Boolean = exportConfig.dataExport.allProteinSet
    val loadSubsets :Boolean = true // TODO moved in the config export param?
    
    // Build the template
    var viewSetTemplate: IViewSetTemplate = if (exportConfig.format == ExportConfigConstant.FORMAT_XLSX) new ProlineConfigViewSetTemplateAsXLSX(exportConfig) else new ProlineConfigViewSetTemplateAsTSV(exportConfig)
    
    return apply(executionContext, projectId, rsmId,loadSubsets, loadFullResultSet, viewSetName, viewSetTemplate, exportConfig )
  }
  

  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    loadSubsets: Boolean,
    loadFullResultSet: Boolean,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate, 
    exportConfig: ExportConfig
  ): ResultSummaryViewSet = {

    val udsSQLCtx = executionContext.getUDSDbConnectionContext()
    val psSQLCtx = executionContext.getPSDbConnectionContext()
    val msiSQLCtx = executionContext.getMSIDbConnectionContext()
    
    // Retrieve the project name
    val projectName = DoJDBCReturningWork.withEzDBC(udsSQLCtx, { udsEzDBC =>
      
      val sqlQuery = new SelectQueryBuilder1( UdsDbProjectTable ).mkSelectQuery { (t,c) =>
        List(t.NAME) -> "WHERE id = ?"
      }
      
      udsEzDBC.selectString(sqlQuery, projectId)
    })

    // Load th RSM
    val rsmProvider = new SQLResultSummaryProvider(msiSQLCtx, psSQLCtx, udsSQLCtx)

    val rsm = if (loadFullResultSet == false) rsmProvider.getResultSummary(rsmId, true).get
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

    return apply(IdentDataSet(projectName, rsm), viewSetName, viewSetTemplate, exportConfig)

  }
  

}