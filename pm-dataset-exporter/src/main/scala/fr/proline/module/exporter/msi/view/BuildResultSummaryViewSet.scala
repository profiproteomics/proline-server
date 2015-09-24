package fr.proline.module.exporter.msi.view

import scala.collection.mutable.ArrayBuffer
import fr.profi.jdbc.easy._
import com.typesafe.scalalogging.LazyLogging
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
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.orm.msi.MsiSearch
import fr.proline.core.om.provider.msi.IResultSummaryProvider

object BuildResultSummaryViewSet extends LazyLogging {

  def apply(ds: IdentDataSet, viewSetName: String, viewSetTemplate: IViewSetTemplate, exportConfig :ExportConfig): ResultSummaryViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate(BuildResultSummaryView(ds, templatedViewType.viewType), templatedViewType.template)
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
    
    // load childs resultSummary (merge)
    var childsResultSummarys: ArrayBuffer[ResultSummary] = new ArrayBuffer[ResultSummary]()
    
    
    // load childs resultSets (merge)
    var childsResultSets: ArrayBuffer[ResultSet] = new ArrayBuffer[ResultSet]()
    

    return apply(IdentDataSet(projectName, rsm, childsResultSummarys.toArray, childsResultSets.toArray), viewSetName, viewSetTemplate, exportConfig)

  }
  
  private def getLeafChildsID(rsId: Long, execContext: IExecutionContext): Seq[Long] = {
    var allRSIds = Seq.newBuilder[Long]

    val jdbcWork = new JDBCWork() {

      override def execute(con: Connection) {

        val stmt = con.prepareStatement("select child_result_set_id from result_set_relation where result_set_relation.parent_result_set_id = ?")
        stmt.setLong(1, rsId)
        val sqlResultSet = stmt.executeQuery()
        var childDefined = false
        while (sqlResultSet.next) {
          childDefined = true
          val nextChildId = sqlResultSet.getInt(1)
          allRSIds ++= getLeafChildsID(nextChildId, execContext)
        }
        if (!childDefined)
          allRSIds += rsId
        stmt.close()
      } // End of jdbcWork anonymous inner class
    }
    execContext.getMSIDbConnectionContext().doWork(jdbcWork, false)

    allRSIds.result
  }
  
  private def getRsmLeafChildsID(rsmId: Long, execContext: IExecutionContext): Seq[Long] = {
    var allRSMIds = Seq.newBuilder[Long]

    val jdbcWork = new JDBCWork() {

      override def execute(con: Connection) {

        val stmt = con.prepareStatement("select child_result_summary_id from result_summary_relation where result_summary_relation.parent_result_summary_id = ?")
        stmt.setLong(1, rsmId)
        val sqlResultSummary = stmt.executeQuery()
        var childDefined = false
        while (sqlResultSummary.next) {
          childDefined = true
          val nextChildId = sqlResultSummary.getInt(1)
          allRSMIds ++= getRsmLeafChildsID(nextChildId, execContext)
        }
        if (!childDefined)
          allRSMIds += rsmId
        stmt.close()
      } // End of jdbcWork anonymous inner class
    }
    execContext.getMSIDbConnectionContext().doWork(jdbcWork, false)

    allRSMIds.result
  }
  
  private def getResultSummaryProvider(execContext: IExecutionContext): IResultSummaryProvider = {
		  
    new SQLResultSummaryProvider(execContext.getMSIDbConnectionContext,
      execContext.getPSDbConnectionContext,
      execContext.getUDSDbConnectionContext)
  }
  
  

}