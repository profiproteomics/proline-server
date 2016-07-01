package fr.proline.module.exporter.dataset.view

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap
import com.typesafe.scalalogging.LazyLogging
import fr.profi.jdbc.easy._
import fr.profi.util.StringUtils
import fr.profi.util.collection._
import fr.proline.context.DatabaseConnectionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.dal.tables.SelectQueryBuilder._
import fr.proline.core.dal.tables.SelectQueryBuilder1
import fr.proline.core.dal.tables.SelectQueryBuilder3
import fr.proline.core.dal.tables.msi.MsiDbMsiSearchTable
import fr.proline.core.dal.tables.msi.MsiDbResultSetTable
import fr.proline.core.dal.tables.msi.MsiDbResultSummaryTable
import fr.proline.core.dal.tables.uds.UdsDbProjectTable
import fr.proline.core.om.model.msi.LazyResultSummary
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.om.provider.msq.impl.SQLExperimentalDesignProvider
import fr.proline.core.om.provider.msq.impl.SQLLazyQuantResultSummaryProvider
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.commons.config._
import fr.proline.module.exporter.commons.view.ViewSet
import fr.proline.module.exporter.dataset._
import fr.proline.module.exporter.dataset.template._
import java.sql.Connection
import fr.proline.repository.util.JDBCWork
import fr.proline.core.dal.tables.uds.UdsDbDataSetTable
import fr.proline.core.dal.tables.uds.UdsDbQuantChannelTable
import fr.proline.core.dal.tables.SelectQueryBuilder2

object BuildDatasetViewSet extends LazyLogging {

  def apply(ds: IdentDataset, viewSetName: String, viewSetTemplate: IViewSetTemplate, exportConfig: ExportConfig): ViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>

      val dsView = BuildDatasetView(ds, templatedViewType.viewType, exportConfig)
      val viewWithTpl = ViewWithTemplate(dsView, templatedViewType.template)

      if (templatedViewType.viewName.isDefined) {
        viewWithTpl.dataView.viewName = templatedViewType.viewName.get
      }

      viewWithTpl
    }

    new ViewSet(viewSetName, viewSetTemplate, templatedViews, exportConfig)
  }

  // Build template from the config
  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    dsId: Long,
    rsmId: Long,
    viewSetName: String,
    mode: String,
    exportConfig: ExportConfig): ViewSet = {

    //val exportConfig = ExportConfigManager.readConfig(exportConfigStr)

    /*require(
      ExportConfigManager.checkTitle(exportConfig),
      "Some titles in the configuration file for export are incorrect !"
    )*/

    val loadFullResultSet = exportConfig.dataExport.allProteinSet
    val loadSubsets = true // TODO moved in the config export param ?

    // Build the template
    val viewSetTemplate = if (exportConfig.format == ExportConfigConstant.FORMAT_XLSX)
      new DatasetTemplateAsXLSX(exportConfig)
    else
      new DatasetTemplateAsTSV(exportConfig)

    this.apply(
      executionContext,
      projectId,
      dsId,
      rsmId,
      loadSubsets,
      loadFullResultSet,
      viewSetName,
      viewSetTemplate,
      mode,
      exportConfig)
  }

  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    loadSubsets: Boolean,
    loadFullResultSet: Boolean,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate,
    mode: String): ViewSet = {
    this.apply(
      executionContext,
      projectId,
      dsId = 0,
      rsmId,
      loadSubsets,
      loadFullResultSet,
      viewSetName,
      viewSetTemplate,
      mode,
      exportConfig = null)
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
    mode: String,
    exportConfig: ExportConfig): ViewSet = {

    val udsDbCtx = executionContext.getUDSDbConnectionContext()
    val psDbCtx = executionContext.getPSDbConnectionContext()
    val msiDbCtx = executionContext.getMSIDbConnectionContext()
    val msiDbHelper = new MsiDbHelper(msiDbCtx)
    val lazyRsmProvider = new SQLLazyResultSummaryProvider(msiDbCtx, psDbCtx, udsDbCtx)
    val lazyRsProvider = new SQLLazyResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    // Retrieve the project name
    val projectName = DoJDBCReturningWork.withEzDBC(udsDbCtx, { udsEzDBC =>

      val sqlQuery = new SelectQueryBuilder1(UdsDbProjectTable).mkSelectQuery { (t, c) =>
        List(t.NAME) -> "WHERE id = ?"
      }

      udsEzDBC.selectString(sqlQuery, projectId)
    })

    if (mode == ExportConfigConstant.MODE_IDENT) {

      // Load the RSM
      logger.debug(s"Loading result summary #$rsmId...")
      val lazyRsmOpt = lazyRsmProvider.getLazyResultSummary(
        rsmId,
        loadFullResultSet = loadFullResultSet,
        linkPeptideSets = loadSubsets,
        linkResultSetEntities = true)
      require(lazyRsmOpt.isDefined, "can't load the result summary with id=" + rsmId)

      val lazyRsm = lazyRsmOpt.get

      // Load child result summaries (merge)
      // VDS : Load only leaf RSM not all children
      // VDS To test get all hierarchy level
      // TODO: add children ids to the RsmDescriptor
      val childResultSummariesLoader = () => {
        logger.debug("Loading child result summaries (merge)...")
        val leavesRsmIds = msiDbHelper.getResultSummaryLeavesIds(rsmId)
        val childResultSummaries = lazyRsmProvider.getLazyResultSummaries(
          leavesRsmIds,
          loadFullResultSet = loadFullResultSet,
          linkPeptideSets = loadSubsets,
          linkResultSetEntities = true)

        if (childResultSummaries.filter(lrsm => { lrsm.lazyResultSet.descriptor.name == null || lrsm.lazyResultSet.descriptor.name.isEmpty }).length > 0)
          childResultSummaries.sortBy(_.lazyResultSet.id) //If no name, use ID should always have one !
        else
          childResultSummaries.sortBy(_.lazyResultSet.descriptor.name)
      }

      val leaveResultSetLoader = () => {
        logger.debug("Loading leave result set (merge)...")
        val leavesRsIds = msiDbHelper.getResultSetLeavesId(lazyRsm.getResultSetId())

        val leavesResultSets = lazyRsProvider.getLazyResultSets(leavesRsIds)

        if (leavesResultSets.filter(lrs => { lrs.descriptor.name == null || lrs.descriptor.name.isEmpty }).length > 0)
          leavesResultSets.sortBy(_.id) //If no name, use ID should always have one !
        else
          leavesResultSets.sortBy(_.descriptor.name)
      }

      logger.debug("Build IdentDataSet")

      val identDs = new IdentDataset(
        projectName,
        lazyRsm,
        childResultSummariesLoader,
        leaveResultSetLoader,
        this._buildBioSequenceLoader(msiDbCtx, lazyRsm),
        this._buildSpectraLoader(msiDbCtx))

      this.apply(identDs, viewSetName, viewSetTemplate, exportConfig)

    } else {

      // TODO: handle other group setups
      val groupSetupNumber = 1

      // Retrieve masterQuantChannelId
      val udsDbHelper = new UdsDbHelper(udsDbCtx)
      val masterQcIds = udsDbHelper.getMasterQuantChannelIdsForQuantId(dsId)
      require(masterQcIds.isEmpty == false, "can't retrieve master quant channels for quantitation id=" + dsId)

      // FIXME: how to deal with other MQC ids
      val masterQuantChannelId = masterQcIds.head

      // Load the experimental design
      val expDesignProvider = new SQLExperimentalDesignProvider(udsDbCtx)
      val expDesignOpt = expDesignProvider.getExperimentalDesign(dsId)
      require(expDesignOpt.isDefined, "can't load the experimental design of the dataset with id=" + dsId)
      val expDesign = expDesignOpt.get

      val masterQcOpt = expDesign.masterQuantChannels.find(_.id == masterQuantChannelId)
      require(masterQcOpt.isDefined, "undefined master quant channel with id=" + masterQuantChannelId)

      val masterQc = masterQcOpt.get

      val quantRsmId = masterQc.quantResultSummaryId.get
      val quantChannels = masterQc.quantChannels
      var identRsmIds = quantChannels.map(_.identResultSummaryId)

      // Workaround for SC quantitations
      if (mode == ExportConfigConstant.MODE_QUANT_SC) {
        // FIXME: it should not be required to merge parent and child ids
        identRsmIds ++= quantChannels.flatMap { qc =>
          msiDbHelper.getResultSummaryLeavesIds(qc.identResultSummaryId)
        }
      }

      // Load the quant RSM
      logger.debug(s"Loading quant result summary #$rsmId...")

      val quantRsmProvider = new SQLLazyQuantResultSummaryProvider(msiDbCtx, psDbCtx, udsDbCtx)
      val lazyQuantRSM = quantRsmProvider.getLazyQuantResultSummaries(
        dsId,
        Seq(quantRsmId),
        loadFullResultSet = loadFullResultSet,
        linkPeptideSets = loadSubsets,
        linkResultSetEntities = true).head

      // Create a lazy loader of ident RSMs
      val identResultSummariesLoader = () => {
        logger.debug("Loading ident result summaries (quantitation)...")
        lazyRsmProvider.getLazyResultSummaries(
          identRsmIds,
          loadFullResultSet = false,
          linkPeptideSets = false, // TODO: set to true ?
          linkResultSetEntities = false // TODO: set to true ?
          )
      }

      val leaveResultSetLoader = () => {
        logger.debug("Loading leaves result set (quantitation)...")
        val leavesRsIds = msiDbHelper.getResultSetLeavesId(lazyQuantRSM.lazyResultSummary.getResultSetId())

        val leavesResultSets = lazyRsProvider.getLazyResultSets(leavesRsIds)

        if (leavesResultSets.filter(lrs => { lrs.descriptor.name == null || lrs.descriptor.name.isEmpty }).length > 0)
          leavesResultSets.sortBy(_.id) //If no name, use ID should always have one !
        else
          leavesResultSets.sortBy(_.descriptor.name)
      }

      var qcNameById = quantChannels.toLongMapWith(qc => qc.id -> qc.name)
      val qcNames = qcNameById.values.toList
      val definedQcNames = qcNames.filter(StringUtils.isNotEmpty(_))

      // Check if we have retrieved defined names
      if (qcNames.length != definedQcNames.length) {

        logger.warn("Some quantitation channels are not named, we will use the result file names instead")

        val qcIdByIdentRsmId = quantChannels.toLongMapWith(qc => qc.identResultSummaryId -> qc.id)

        // Set QC names as the result file name         
        qcNameById = {

          val resultLongMap = DoJDBCReturningWork.withEzDBC(msiDbCtx) { ezDBC =>

            val qcIdentRsmIds = identRsmIds.filter(qcIdByIdentRsmId.contains(_))

            val sqlQuery = new SelectQueryBuilder3(MsiDbResultSetTable, MsiDbResultSummaryTable, MsiDbMsiSearchTable).mkSelectQuery { (t1, c1, t2, c2, t3, c3) =>
              List(t2.ID, t3.RESULT_FILE_NAME) ->
                "WHERE " ~ t2.ID ~ " IN (" ~ qcIdentRsmIds.mkString(",") ~ ") " ~
                "AND " ~ t1.ID ~ "=" ~ t2.RESULT_SET_ID ~ " AND " ~ t3.ID ~ " = " ~ t1.MSI_SEARCH_ID
            }

            val longMap = new LongMap[String]()
            ezDBC.selectAndProcess(sqlQuery) { r =>
              longMap += qcIdByIdentRsmId(r.nextLong) -> r.nextString
              ()
            }

            longMap
          }

          if (mode == ExportConfigConstant.MODE_QUANT_SC) {
            resultLongMap ++= DoJDBCReturningWork.withEzDBC(udsDbCtx) { ezDBC =>

              val longMap = new LongMap[String]()

              for (qc <- quantChannels) {
                // get the name
                val sqlQuery2 = new SelectQueryBuilder2(UdsDbDataSetTable, UdsDbQuantChannelTable).mkSelectQuery { (t1, c1, t2, c2) =>
                  List(t1.NAME) ->
                    "WHERE " ~ t2.ID ~ " = " ~ qc.id ~
                    " AND " ~ t2.IDENT_RESULT_SUMMARY_ID ~ "=" ~ t1.RESULT_SUMMARY_ID ~ " AND " ~ t1.PROJECT_ID ~ " = " ~ projectId
                }

                ezDBC.selectAndProcess(sqlQuery2) { r =>
                  longMap += qc.id -> r.nextString
                  ()
                }
              }
              longMap
            }

          }

          resultLongMap
        }

      }

      val protMatchPeptideNumberByPepMatchIdByQCId: scala.collection.mutable.Map[Long, Map[Long, Int]] = scala.collection.mutable.Map.empty[Long, Map[Long, Int]]
      for (qc <- quantChannels) {
        val protMatchPeptideNumberByPepMatchId: scala.collection.mutable.Map[Long, Int] = scala.collection.mutable.Map.empty[Long, Int]
        val protStatJdbcWork = new JDBCWork() {
          override def execute(con: Connection) {

            //---- Read PepCount
            val getPepCount = "SELECT peptide_count, protein_match_id FROM peptide_set_protein_match_map pspmm, peptide_set " +
              "WHERE  pspmm.result_summary_id = ?  AND peptide_set.id = pspmm.peptide_set_id"
            val pStmt2 = con.prepareStatement(getPepCount)
            pStmt2.setLong(1, qc.identResultSummaryId)
            val sqlResultSet2 = pStmt2.executeQuery()
            while (sqlResultSet2.next) {
              val protMatchPepNbr = sqlResultSet2.getInt("peptide_count")
              val protMatchId = sqlResultSet2.getLong("protein_match_id")
              protMatchPeptideNumberByPepMatchId += protMatchId -> protMatchPepNbr
            }
            pStmt2.close()
          }
        }
        msiDbCtx.doWork(protStatJdbcWork, false)
        protMatchPeptideNumberByPepMatchIdByQCId.put(qc.id, protMatchPeptideNumberByPepMatchId.toMap)
      }

      logger.debug("Build QuantDataSet")

      val quantDs = new QuantDataset(
        projectName,
        lazyQuantRSM,
        expDesignOpt,
        masterQc,
        groupSetupNumber,
        identResultSummariesLoader,
        leaveResultSetLoader,
        this._buildBioSequenceLoader(msiDbCtx, lazyQuantRSM.lazyResultSummary),
        this._buildSpectraLoader(msiDbCtx),
        qcNameById,
        protMatchPeptideNumberByPepMatchIdByQCId.toMap)

      this.apply(quantDs, viewSetName, viewSetTemplate, exportConfig)
    }

  }

  private def _buildBioSequenceLoader(msiDbCtx: DatabaseConnectionContext, lazyRsm: LazyResultSummary) = {
    () =>
      {
        logger.debug("Loading biological sequences...")

        val bioSeqProvider = new SQLBioSequenceProvider(msiDbCtx)

        val protMatches = lazyRsm.lazyResultSet.proteinMatches
        val protIds = new ArrayBuffer[Long](protMatches.length)

        for (protMatch <- protMatches) {
          val protId = protMatch.getProteinId
          if (protId > 0) {
            protIds += protId
          }
        }

        bioSeqProvider.getBioSequences(protIds, loadSequence = false)
      }
  }

  private def _buildSpectraLoader(msiDbCtx: DatabaseConnectionContext) = {
    (peaklistIds: Array[Long]) =>
      {
        logger.debug("Loading spectra descriptors...")

        val spectrumProvider = new SQLSpectrumProvider(msiDbCtx)

        spectrumProvider.getPeaklistsSpectra(peaklistIds, loadPeaks = false)

        /*val peptideMatches = lazyRsm.lazyResultSet.peptideMatches
      val spectraIds = new ArrayBuffer[Long](peptideMatches.length)
      for( peptideMatch <- peptideMatches) {
        if( peptideMatch.msQuery != null ) {
          peptideMatch.msQuery match {
            case ms2Query: Ms2Query => spectraIds += ms2Query.spectrumId
          }
        }
      }
      
      spectrumProvider.getSpectra(spectraIds, loadPeaks = false)*/
      }
  }

}