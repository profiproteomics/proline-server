package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.ArrayBuffer
import fr.profi.jdbc.easy._
import com.typesafe.scalalogging.slf4j.Logging
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
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.dal.tables.SelectQueryBuilder3
import fr.proline.core.dal.tables.uds.UdsDbQuantChannelTable
import fr.proline.core.dal.tables.msi.MsiDbResultSetTable
import fr.proline.core.dal.tables.msi.MsiDbResultSummaryTable
import fr.proline.core.dal.tables.msi.MsiDbMsiSearchTable
import fr.profi.util.primitives.toLong
import fr.proline.core.om.provider.msq.impl.SQLExperimentalDesignProvider
import fr.proline.core.om.provider.msq.impl.SQLQuantResultSummaryProvider
import fr.proline.core.om.model.msq.RatioDefinition

object BuildDatasetViewSet extends Logging {

  def apply(ds: IdentDataSet, viewSetName: String, viewSetTemplate: IViewSetTemplate, exportConfig: ExportConfig): DatasetViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate(BuildDatasetView(ds, templatedViewType.viewType, exportConfig), templatedViewType.template)
      if (templatedViewType.viewName.isDefined) viewWithTpl.dataView.viewName = templatedViewType.viewName.get

      viewWithTpl
    }

    new DatasetViewSet(viewSetName, templatedViews, exportConfig)
  }

  // build template from the  config
  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    dsId: Long,
    rsmId: Long,
    viewSetName: String,
    exportConfigStr: String): DatasetViewSet = {

    val exportConfig: ExportConfig = ExportConfigManager.readConfig(exportConfigStr)
    val checkTitle : Boolean = ExportConfigManager.checkTitle(exportConfig)
    if (!checkTitle){
        val msg = " Some titles in the configuration file for export are incorrect! " 
        logger.warn(msg)
        throw new Exception(msg)
    }
    val loadFullResultSet: Boolean = exportConfig.dataExport.allProteinSet
    val loadSubsets: Boolean = true // TODO moved in the config export param?

    // Build the template
    var viewSetTemplate: IViewSetTemplate = if (exportConfig.format == ExportConfigConstant.FORMAT_XLSX) new ProlineConfigViewSetTemplateAsXLSX(exportConfig) else new ProlineConfigViewSetTemplateAsTSV(exportConfig)

    return apply(executionContext, projectId, dsId, rsmId, loadSubsets, loadFullResultSet, viewSetName, viewSetTemplate, exportConfig)
  }

  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    loadSubsets: Boolean,
    loadFullResultSet: Boolean,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate): DatasetViewSet = {
    return apply(executionContext, projectId, -1, rsmId, loadSubsets, loadFullResultSet, viewSetName, viewSetTemplate, null)
  }

  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    dsId: Long,
    rsmId: Long,
    loadSubsets: Boolean,
    loadFullResultSet: Boolean,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate,
    exportConfig: ExportConfig): DatasetViewSet = {

    val udsSQLCtx = executionContext.getUDSDbConnectionContext()
    val psSQLCtx = executionContext.getPSDbConnectionContext()
    val msiSQLCtx = executionContext.getMSIDbConnectionContext()

    // Retrieve the project name
    val projectName = DoJDBCReturningWork.withEzDBC(udsSQLCtx, { udsEzDBC =>

      val sqlQuery = new SelectQueryBuilder1(UdsDbProjectTable).mkSelectQuery { (t, c) =>
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
    val providerRsm: IResultSummaryProvider = getResultSummaryProvider(executionContext)
    var leavesRsmIds: Seq[Long] = getRsmLeafChildsID(rsm.id, executionContext)
    leavesRsmIds.foreach(rsmID => {
      val resultSummaryRS = providerRsm.getResultSummary(rsmID, true)
      if (resultSummaryRS.isDefined) {
        val resultSummaryLeaf: ResultSummary = resultSummaryRS.get
        childsResultSummarys += resultSummaryLeaf
      } else {
        val msg = " !!! Unable to get leave search result with id " + rsmID
        logger.warn(msg)
        throw new Exception(msg)
      }
    })

    // load childs resultSets (merge)
    var childsResultSets: ArrayBuffer[ResultSet] = new ArrayBuffer[ResultSet]()
    if (rsm.resultSet.isDefined) {
      val providerContext = ProviderDecoratedExecutionContext(executionContext)
      val provider: IResultSetProvider = new SQLResultSetProvider(msiSQLCtx, psSQLCtx, udsSQLCtx)
      val rs = rsm.resultSet.get
      var leavesRsIds: Seq[Long] = getLeafChildsID(rs.id, executionContext)
      leavesRsIds.foreach(rsID => {
        val resultRS = provider.getResultSet(rsID)
        if (resultRS.isDefined) {
          val resultSetLeaf: ResultSet = resultRS.get
          childsResultSets += resultSetLeaf
        } else {
          val msg = " !!! Unable to get leave search result with id " + rsID
          logger.warn(msg)
          throw new Exception(msg)
        }
      })
    }

    var identDs: IdentDataSet = new IdentDataSet(projectName, rsm, childsResultSummarys.toArray, childsResultSets.toArray)

    // identRs
    if (dsId > 0) {
      // retrieve masterQuantChannelId
      val udsDbHelper = new UdsDbHelper(udsSQLCtx)
      val masterQuantIds = udsDbHelper.getMasterQuantChannelIdsForQuantId(dsId)
      if (masterQuantIds != null && masterQuantIds.length > 0) {
        val masterQuantChannelId = masterQuantIds(0);
        val quantRsmId = udsDbHelper.getMasterQuantChannelQuantRsmId(masterQuantChannelId)
        val qcIds = udsDbHelper.getQuantChannelIds(masterQuantChannelId)

        lazy val nameByQchId: Map[Long, String] = {

          val qChIdByRsmId: Map[Long, Long] = {
            DoJDBCReturningWork.withEzDBC(udsSQLCtx, { ezDBC =>

              val rsmIdForquantChannelQuery = new SelectQueryBuilder1(UdsDbQuantChannelTable).mkSelectQuery(
                (t1, c1) => List(t1.IDENT_RESULT_SUMMARY_ID, t1.ID) ->
                  " WHERE " ~ t1.ID ~ " IN(" ~ qcIds.mkString(",") ~ ")")

              ezDBC.select(rsmIdForquantChannelQuery) { r =>
                toLong(r.nextAny) -> toLong(r.nextAny)
              } toMap

            })
          }

          DoJDBCReturningWork.withEzDBC(msiSQLCtx, { ezDBC =>

            val rsNameForRsmIdQuery = new SelectQueryBuilder3(MsiDbResultSetTable, MsiDbResultSummaryTable, MsiDbMsiSearchTable).mkSelectQuery(
              (t1, c1, t2, c2, t3, c3) => List(t2.ID, t3.RESULT_FILE_NAME) ->
                " WHERE " ~ t2.ID ~ " IN(" ~ qChIdByRsmId.keys.mkString(",") ~ ") " ~
                "AND " ~ t1.ID ~ "=" ~ t2.RESULT_SET_ID ~ " AND " ~ t3.ID ~ " = " ~ t1.MSI_SEARCH_ID)

            val resultBuilder = Map.newBuilder[Long, String]
            ezDBC.selectAndProcess(rsNameForRsmIdQuery) { r =>

              resultBuilder += qChIdByRsmId(toLong(r.nextAny)) -> r.nextString
              ()
            }

            resultBuilder.result

          })
        }

        // quant RSM
        val quantRSM = {
          val quantRsmProvider = new SQLQuantResultSummaryProvider(msiSQLCtx, psSQLCtx, udsSQLCtx)
          quantRsmProvider.getQuantResultSummary(quantRsmId.get, qcIds, true).get
        }

        val expDesignProvider = new SQLExperimentalDesignProvider(udsSQLCtx)
        val expDesign = if (expDesignProvider.getExperimentalDesign(dsId).isDefined) expDesignProvider.getExperimentalDesign(dsId).get else null
        var ratioDefs: Array[RatioDefinition] = null
        //TODO: retrieve the right value
        val groupSetupNumber = 1
        if (expDesign != null && expDesign.groupSetupByNumber != null && expDesign.groupSetupByNumber.contains(groupSetupNumber) && expDesign.groupSetupByNumber(groupSetupNumber) != null) {
          ratioDefs = expDesign.groupSetupByNumber(groupSetupNumber).ratioDefinitions
        }
        var protMatchStatusByIdPepMatchByQCId: Map[Long, Map[Long, String]] = Map()
        var protMatchPeptideNumberByPepMatchIdByQCId: Map[Long, Map[Long, Int]] = Map()
        var rsmIdByQChId: Map[Long, Long] = Map()
        childsResultSummarys = new ArrayBuffer[ResultSummary]()
        //iterate over qc Id
        qcIds.foreach(qcId => {
          var protMatchStatusByIdPepMatch: Map[Long, String] = Map()
          var protMatchPeptideNumberByPepMatchId: Map[Long, Int] = Map()
          // get the rsmId
          val rsmIdQCWork = new JDBCWork() {
            override def execute(con: Connection) {
              val getRSMQCQuery = "SELECT  ident_result_summary_id from quant_channel WHERE id = ?"
              val pStmt = con.prepareStatement(getRSMQCQuery)
              pStmt.setLong(1, qcId)
              val sqlResultSet = pStmt.executeQuery()
              while (sqlResultSet.next) { //Should be One ! 
                val idfRsmQC = sqlResultSet.getLong("ident_result_summary_id")
                rsmIdByQChId += qcId -> idfRsmQC
              }
              pStmt.close()
            }
          }
          udsSQLCtx.doWork(rsmIdQCWork, false)
          // replace childResultSummarys
          childsResultSummarys += providerRsm.getResultSummary(rsmIdByQChId.get(qcId).get, true).get
          //Read Prot Status and Nbr Peptides from DBs
          val protStatJdbcWork = new JDBCWork() {
            override def execute(con: Connection) {
              //---- Read Prot Status

              val getProtStatus = "SELECT protein_set_id, protein_match_id, is_in_subset, typical_protein_match_id FROM protein_set_protein_match_item, protein_set " +
                " WHERE protein_set.id = protein_set_protein_match_item.protein_set_id " +
                " AND protein_set_protein_match_item.result_summary_id = ? "
              val pStmt = con.prepareStatement(getProtStatus)
              pStmt.setLong(1, rsmIdByQChId.get(qcId).get)

              val sqlResultSet = pStmt.executeQuery()
              while (sqlResultSet.next) {
                val isInSubset = sqlResultSet.getBoolean("is_in_subset")
                val protSetTypID = sqlResultSet.getLong("typical_protein_match_id")
                val protMatchId = sqlResultSet.getLong("protein_match_id")
                val protSetId = sqlResultSet.getLong("protein_set_id")
                var protMatchStatus: String = null
                if (isInSubset) {
                  protMatchStatus = "Subset"
                } else if (protSetTypID.equals(protMatchId)) { //is the typical 
                  protMatchStatus = "Typical"
                } else
                  protMatchStatus = "Sameset"
                protMatchStatusByIdPepMatch += protMatchId -> protMatchStatus
              }
              pStmt.close()

              //---- Read Prot Status
              val getPepCount = "SELECT peptide_count, protein_match_id FROM peptide_set_protein_match_map pspmm, peptide_set " +
                "WHERE  pspmm.result_summary_id = ?  AND peptide_set.id = pspmm.peptide_set_id"
              val pStmt2 = con.prepareStatement(getPepCount)
              pStmt2.setLong(1, rsmIdByQChId.get(qcId).get)
              val sqlResultSet2 = pStmt2.executeQuery()
              while (sqlResultSet2.next) {
                val protMatchPepNbr = sqlResultSet2.getInt("peptide_count")
                val protMatchId = sqlResultSet2.getLong("protein_match_id")
                protMatchPeptideNumberByPepMatchId += protMatchId -> protMatchPepNbr
              }
              pStmt2.close()
            }
          } // End of jdbcWork anonymous inner class    
          msiSQLCtx.doWork(protStatJdbcWork, false)

          protMatchStatusByIdPepMatchByQCId += qcId -> protMatchStatusByIdPepMatch
          protMatchPeptideNumberByPepMatchIdByQCId += qcId -> protMatchPeptideNumberByPepMatchId
        })

        identDs = new QuantiDataSet(projectName, rsm, childsResultSummarys.toArray, childsResultSets.toArray, masterQuantChannelId, quantRSM, qcIds, expDesign, ratioDefs, nameByQchId, protMatchStatusByIdPepMatchByQCId, protMatchPeptideNumberByPepMatchIdByQCId)
      } // end masterQuantIds not null
    }

    return apply(identDs, viewSetName, viewSetTemplate, exportConfig)

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