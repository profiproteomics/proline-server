package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

import com.typesafe.scalalogging.LazyLogging

import fr.profi.jdbc.easy._
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

object BuildDatasetViewSet extends LazyLogging {

  def apply(ds: IdentDataset, viewSetName: String, viewSetTemplate: IViewSetTemplate, exportConfig: ExportConfig): ViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      
      val dsView = BuildDatasetView(ds, templatedViewType.viewType, exportConfig)
      val viewWithTpl = ViewWithTemplate(dsView, templatedViewType.template)
      
      if(templatedViewType.viewName.isDefined) {
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
    exportConfigStr: String
  ): ViewSet = {

    val exportConfig = ExportConfigManager.readConfig(exportConfigStr)
    
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
      exportConfig
    )
  }

  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    loadSubsets: Boolean,
    loadFullResultSet: Boolean,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate
  ): ViewSet = {
    this.apply(
      executionContext,
      projectId,
      dsId = -1,
      rsmId,
      loadSubsets,
      loadFullResultSet,
      viewSetName,
      viewSetTemplate,
      exportConfig = null
    )
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
    exportConfig: ExportConfig
  ): ViewSet = {
    
    val udsDbCtx = executionContext.getUDSDbConnectionContext()
    val psDbCtx = executionContext.getPSDbConnectionContext()
    val msiDbCtx = executionContext.getMSIDbConnectionContext()
    val msiDbHelper = new MsiDbHelper(msiDbCtx)
    val lazyRsmProvider = new SQLLazyResultSummaryProvider(msiDbCtx, psDbCtx, udsDbCtx)

    // Retrieve the project name
    val projectName = DoJDBCReturningWork.withEzDBC(udsDbCtx, { udsEzDBC =>

      val sqlQuery = new SelectQueryBuilder1(UdsDbProjectTable).mkSelectQuery { (t, c) =>
        List(t.NAME) -> "WHERE id = ?"
      }

      udsEzDBC.selectString(sqlQuery, projectId)
    })

    // If dsId > 0 it seems to mean that we are loading a quantitative dataset => DBO: why ?
    if (dsId == 0) {
      
      // Load the RSM
      logger.debug(s"Loading result summary #$rsmId...")
      val lazyRsmOpt = lazyRsmProvider.getLazyResultSummary(
        rsmId,
        loadFullResultSet = loadFullResultSet,
        linkPeptideSets = loadSubsets,
        linkResultSetEntities = true
      )
      require( lazyRsmOpt.isDefined, "can't load the result summary with id="+rsmId)
      
      val lazyRsm = lazyRsmOpt.get
  
      // Load child result summaries (merge)
      val childResultSummariesLoader = () => {
        logger.debug("Loading child result summaries (merge)...")
        
        // TODO: add children ids to the RsmDescriptor
        val childRsmIds = msiDbHelper.getResultSummaryChildrenIds(rsmId)
        val childResultSummaries = lazyRsmProvider.getLazyResultSummaries(
          childRsmIds,
          loadFullResultSet = loadFullResultSet,
          linkPeptideSets = loadSubsets,
          linkResultSetEntities = true
        )
        
        childResultSummaries
      }
      
      logger.debug("Build IdentDataSet")
      
      val identDs = new IdentDataset(
        projectName,
        lazyRsm,
        childResultSummariesLoader,
        this._buildBioSequenceLoader(msiDbCtx, lazyRsm),
        this._buildSpectraLoader(msiDbCtx, lazyRsm)
      )
      
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
      require( expDesignOpt.isDefined, "can't load the experimental design of the dataset with id=" + dsId)
      val expDesign = expDesignOpt.get
      
      val masterQcOpt = expDesign.masterQuantChannels.find(_.id == masterQuantChannelId)
      require(masterQcOpt.isDefined, "undefined master quant channel with id=" + masterQuantChannelId)
      
      val masterQc = masterQcOpt.get

      // Get entity manager
      //val udsEM = executionContext.getUDSDbConnectionContext().getEntityManager()
      
      // Retrieve the master quant channel
      //val udsMasterQuantChannel = udsEM.find(classOf[fr.proline.core.orm.uds.MasterQuantitationChannel], masterQuantChannelId)
      //require(udsMasterQuantChannel != null, "undefined master quant channel with id=" + masterQuantChannelId)

      val quantRsmId = masterQc.quantResultSummaryId.get
      val quantChannels = masterQc.quantChannels
      val qcIds = quantChannels.map(_.id)
      val identRsmIds = masterQc.quantChannels.map(_.identResultSummaryId)
      val qcIdByIdentRsmId = quantChannels.toLongMap(qc => qc.identResultSummaryId -> qc.id)
      
      // Load the quant RSM
      logger.debug(s"Loading quant result summary #$rsmId...")
      
      val quantRsmProvider = new SQLLazyQuantResultSummaryProvider(msiDbCtx, psDbCtx, udsDbCtx)
      val lazyQuantRSM = quantRsmProvider.getLazyQuantResultSummaries(
        dsId,
        Seq(quantRsmId),
        loadFullResultSet = loadFullResultSet,
        linkPeptideSets = loadSubsets,
        linkResultSetEntities = true
      ).head

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

      // TODO: update and use the QuantChannel name in the UDSdb
      val qcNameById = {

        DoJDBCReturningWork.withEzDBC(msiDbCtx, { ezDBC =>

          val sqlQuery = new SelectQueryBuilder3(MsiDbResultSetTable, MsiDbResultSummaryTable, MsiDbMsiSearchTable).mkSelectQuery { (t1, c1, t2, c2, t3, c3) =>
            List(t2.ID, t3.RESULT_FILE_NAME) ->
              " WHERE " ~ t2.ID ~ " IN (" ~ identRsmIds.mkString(",") ~ ") " ~
              "AND " ~ t1.ID ~ "=" ~ t2.RESULT_SET_ID ~ " AND " ~ t3.ID ~ " = " ~ t1.MSI_SEARCH_ID
          }

          val longMap = new LongMap[String]()
          ezDBC.selectAndProcess(sqlQuery) { r =>
            longMap += qcIdByIdentRsmId(r.nextLong) -> r.nextString
            ()
          }
          /*
       // if (resultBuilder.result().size == 0) {
          for (qcId <- qcIds){
          // get the name
          val qcNameWork = new JDBCWork() {
            override def execute(con: Connection) {
              val getQCName = "SELECT ds.name FROM data_set ds, quant_channel qc WHERE qc.id = ? AND qc.ident_result_summary_id = ds.result_summary_id AND ds.project_id = ? "
              val pStmt = con.prepareStatement(getQCName)
              pStmt.setLong(1, qcId)
              pStmt.setLong(2, projectId)
              val sqlResultSet = pStmt.executeQuery()
              while (sqlResultSet.next) { //Should be One ! 
                val nameQC = sqlResultSet.getString("name")
                resultBuilder += qcId -> nameQC
              }
              pStmt.close()
            }
          }
          udsDbCtx.doWork(qcNameWork, false)
            
             //resultBuilder += qcId -> (""+qcId)
          }
       // }
          */

          longMap
        })
      }
      
      logger.debug("Build QuantDataSet")

      val quantDs = new QuantDataset(
        projectName,
        lazyQuantRSM,
        expDesign,
        masterQc,
        groupSetupNumber,
        identResultSummariesLoader,
        this._buildBioSequenceLoader(msiDbCtx, lazyQuantRSM.lazyResultSummary),
        this._buildSpectraLoader(msiDbCtx, lazyQuantRSM.lazyResultSummary),
        qcNameById
      )

      this.apply(quantDs, viewSetName, viewSetTemplate, exportConfig)
    }
    

    
    // Load the experimental design
    /*val expDesignProvider = new SQLExperimentalDesignProvider(udsDbCtx)
    val expDesignOpt = expDesignProvider.getExperimentalDesign(dsId)
    require( expDesignOpt.isDefined, "can't load the experimental design of the dataset with id=" + dsId)
    val expDesign = expDesignOpt.get*/
    
    // TODO: handle other group setups
    //val ratioDefs = expDesign.groupSetups.head.ratioDefinitions
    
    /*val protMatchStatusByIdPepMatchByQCId: Map[Long, Map[Long, String]] = Map()
    val protMatchPeptideNumberByPepMatchIdByQCId: Map[Long, Map[Long, Int]] = Map()
    
    // Iterate over quant channels
    for (qcId <- qcIds) {
      val protMatchStatusByIdPepMatch: Map[Long, String] = Map()
      val protMatchPeptideNumberByPepMatchId: Map[Long, Int] = Map()
      
      //Read Prot Status and Nbr Peptides from DBs
      val protStatJdbcWork = new JDBCWork() {
        override def execute(con: Connection) {
          //---- Read Prot Status

          val getProtStatus = "SELECT protein_set_id, protein_match_id, is_in_subset, representative_protein_match_id FROM protein_set_protein_match_item, protein_set " +
            " WHERE protein_set.id = protein_set_protein_match_item.protein_set_id " +
            " AND protein_set_protein_match_item.result_summary_id = ? "
          val pStmt = con.prepareStatement(getProtStatus)
          pStmt.setLong(1, rsmIdByQChId.get(qcId).get)

          val sqlResultSet = pStmt.executeQuery()
          while (sqlResultSet.next) {
            val isInSubset = sqlResultSet.getBoolean("is_in_subset")
            val protSetTypID = sqlResultSet.getLong("representative_protein_match_id")
            val protMatchId = sqlResultSet.getLong("protein_match_id")
            val protSetId = sqlResultSet.getLong("protein_set_id")
            var protMatchStatus: String = null
            if (isInSubset) {
              protMatchStatus = "Subset"
            } else if (protSetTypID.equals(protMatchId)) { //is the typical 
              protMatchStatus = "Representative"
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
    }*/

 
  }

  private def _buildBioSequenceLoader(msiDbCtx: DatabaseConnectionContext, lazyRsm: LazyResultSummary) = {
    () => {
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
  
  private def _buildSpectraLoader(msiDbCtx: DatabaseConnectionContext, lazyRsm: LazyResultSummary) = {
    () => {
      logger.debug("Loading spectra descriptors...")
      
      val spectrumProvider = new SQLSpectrumProvider(msiDbCtx)
      
      /*var msQueryIdByPeptideMatchId : Map[Long, Long] = Map()
      for ((peptideMatchId, peptideMatch) <- rsm.resultSet.get.getPeptideMatchById()) {
        if (peptideMatch != null && peptideMatch.msQuery != null){
          msQueryIdByPeptideMatchId += peptideMatchId -> peptideMatch.msQuery.id
        }
      }
      var spectrumFirstTimeByMsQueryId: Map[Long, Double] =getSpectrumData(msQueryIdByPeptideMatchId, executionContext)
      */
      
      val peptideMatches = lazyRsm.lazyResultSet.peptideMatches
      val spectraIds = new ArrayBuffer[Long](peptideMatches.length)
      for( peptideMatch <- peptideMatches) {
        if( peptideMatch.msQuery != null ) {
          peptideMatch.msQuery match {
            case ms2Query: Ms2Query => spectraIds += ms2Query.spectrumId
          }
        }
      }
      
      spectrumProvider.getSpectra(spectraIds, loadPeaks = false)
    }
  }
  
  /*
  private def getSpectrumData(msQueryIdByPeptideMatchId: Map[Long, Long], execContext: IExecutionContext): Map[Long, Double] = {
    var spectrumBySpectrumId: Map[Long, Double] = Map()
    if (msQueryIdByPeptideMatchId.values.toList.length > 0){
      var ids: String = msQueryIdByPeptideMatchId.values.toList.mkString(",")

      val jdbcWork = new JDBCWork() {

        override def execute(con: Connection) {

          val stmt = con.prepareStatement("select msq.id, s.first_time from spectrum s, ms_query msq where msq.id IN (" + ids + ") AND msq.spectrum_id = s.id ")
          val sqlSpectra = stmt.executeQuery()
          while (sqlSpectra.next) {
            var msQueryId = sqlSpectra.getLong("id")
            var firstTime = sqlSpectra.getDouble("first_time")
            spectrumBySpectrumId += msQueryId -> firstTime
          }
          stmt.close()
        } // End of jdbcWork anonymous inner class
      }
      execContext.getMSIDbConnectionContext().doWork(jdbcWork, false)
    }
    spectrumBySpectrumId
  }*/

}