package fr.proline.module.exporter.dataset.view

import com.typesafe.scalalogging.LazyLogging
import fr.profi.jdbc.easy._
import fr.profi.util.StringUtils
import fr.profi.util.collection._
import fr.proline.context.IExecutionContext
import fr.proline.context.MsiDbConnectionContext
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.core.dal.DoJDBCWork
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.dal.tables.SelectQueryBuilder1
import fr.proline.core.dal.tables.uds.UdsDbProjectTable
import fr.proline.core.om.model.msi.LazyResultSet
import fr.proline.core.om.model.msi.LazyResultSummary
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.om.provider.msq.impl._
import fr.proline.core.orm.uds.ObjectTreeSchema
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.config._
import fr.proline.module.exporter.commons.view.ViewSet
import fr.proline.module.exporter.dataset._
import fr.proline.module.exporter.dataset.template._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

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
    exportConfig: ExportConfig
  ): ViewSet = {


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
    viewSetTemplate: IViewSetTemplate,
    mode: String
  ): ViewSet = {
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
    mode: String,
    exportConfig: ExportConfig
  ): ViewSet = {

    val udsDbCtx = executionContext.getUDSDbConnectionContext
    val msiDbCtx = executionContext.getMSIDbConnectionContext
    val msiDbHelper = new MsiDbHelper(msiDbCtx)
    val lazyRsmProvider = new SQLLazyResultSummaryProvider(PeptideCacheExecutionContext(executionContext))
    val lazyRsProvider = new SQLLazyResultSetProvider(PeptideCacheExecutionContext(executionContext))

    // Retrieve the project name
    val projectName = DoJDBCReturningWork.withEzDBC(udsDbCtx) { udsEzDBC =>

      val sqlQuery = new SelectQueryBuilder1(UdsDbProjectTable).mkSelectQuery { (t, c) =>
        List(t.NAME) -> "WHERE id = ?"
      }

      udsEzDBC.selectString(sqlQuery, projectId)
    }

    if (mode == ExportConfigConstant.MODE_IDENT) {

      // Load the RSM
      logger.debug(s"Loading result summary #$rsmId...")
      val lazyRsmOpt = lazyRsmProvider.getLazyResultSummary(
        rsmId,
        loadFullResultSet = loadFullResultSet,
        linkPeptideSets = loadSubsets,
        linkResultSetEntities = true
      )
      require(lazyRsmOpt.isDefined, "can't load the result summary with id=" + rsmId)

      val lazyRsm = lazyRsmOpt.get

      // Load leave result summaries (merge)
      // TODO: add children ids to the RsmDescriptor
      val leaveResultSummariesLoader = () => {
        logger.debug("Loading leave result summaries (merge)...")
        val leavesRsmIds = msiDbHelper.getResultSummaryLeavesIds(rsmId)
        val childResultSummaries = lazyRsmProvider.getLazyResultSummaries(
          leavesRsmIds,
          loadFullResultSet = loadFullResultSet,
          linkPeptideSets = loadSubsets,
          linkResultSetEntities = true
        )

        if (childResultSummaries.exists(lrsm => StringUtils.isEmpty(lrsm.lazyResultSet.descriptor.name)))
          childResultSummaries.sortBy(_.lazyResultSet.id) //If no name, use ID should always have one !
        else
          childResultSummaries.sortBy(_.lazyResultSet.descriptor.name)
      }

      // DBO => why loading leaves result sets ? To get leave description info : search params....
      val leaveResultSetLoader = () => {
        logger.debug("Loading leave result set (merge)...")
        _loadLeavesResultSets(lazyRsm.getResultSetId(), msiDbHelper, lazyRsProvider)
      }

      logger.debug("Build IdentDataSet")

      val identDs = new IdentDataset(
        projectName,
        lazyRsm,
        leaveResultSummariesLoader,
        leaveResultSetLoader,
        this._buildBioSequenceLoader(msiDbCtx, lazyRsm),
        this._buildSpectraLoader(msiDbCtx)
      )

      this.apply(identDs, viewSetName, viewSetTemplate, exportConfig)

    } else {

      // TODO: handle other group setups
      val groupSetupNumber = 1

      // Retrieve masterQuantChannelId
      val udsDbHelper = new UdsDbHelper(udsDbCtx)
      val masterQcIds = udsDbHelper.getMasterQuantChannelIdsForQuantId(dsId)
      require(!masterQcIds.isEmpty, "can't retrieve master quant channels for quantitation id=" + dsId)

      // FIXME: how to deal with other MQC ids
      val masterQuantChannelId = masterQcIds.head
      
      val( quantConfigAndMethodOpt, quantConfigSchemaName, profilizerConfigOpt) = if (mode != ExportConfigConstant.MODE_QUANT_XIC) (None,None, None)
      else {
        // Load the quant config and the profilizer config
        var quantSchemaName : Option[ObjectTreeSchema.SchemaName]= None
        val quantConfigProvider = new SQLQuantConfigProvider(udsDbCtx)
        val profilizerConfigProvider = new SQLProfilizerConfigProvider(udsDbCtx)
        val quantConfigAsStringOpt = quantConfigProvider.getQuantConfigAsString(dsId)
        val quantConfigAndMethod = if (quantConfigAsStringOpt.isDefined) {
          quantSchemaName = Some(quantConfigAsStringOpt.get._2)
          Some(quantConfigAsStringOpt.get._1,  quantConfigAsStringOpt.get._3)
        } else {
          None
        }
        (quantConfigAndMethod,quantSchemaName, profilizerConfigProvider.getProfilizerConfigAsString(dsId))
      }

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
      var identRsmIds = quantChannels.map(_.identResultSummaryId).filter(_>0)

      // Workaround for SC quantitations
      if (mode == ExportConfigConstant.MODE_QUANT_SC) {
        // FIXME: it should not be required to merge parent and child ids
        identRsmIds ++= quantChannels.flatMap { qc =>
          msiDbHelper.getResultSummaryLeavesIds(qc.identResultSummaryId)
        }
      }

      //VDS MAY NOT NEED Workaround for Agg of Quant
      if(quantConfigSchemaName.isDefined && quantConfigSchemaName.get.equals(ObjectTreeSchema.SchemaName.AGGREGATION_QUANT_CONFIG) && masterQc.identResultSummaryId.isDefined ){
        val parentRsmID = masterQc.identResultSummaryId.get
        identRsmIds ++= msiDbHelper.getResultSummaryLeavesIds(parentRsmID)
      }

      // Load the quant RSM
      logger.debug(s"Loading quant result summary #$rsmId...")

      val loadReporterIons = mode == ExportConfigConstant.MODE_QUANT_TAGGING
      val quantRsmProvider = new SQLLazyQuantResultSummaryProvider(PeptideCacheExecutionContext(executionContext), loadReporterIons)
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
        val rsms = lazyRsmProvider.getLazyResultSummaries(
          identRsmIds,
          loadFullResultSet = false,
          linkPeptideSets = false, // TODO: set to true ?
          linkResultSetEntities = false // TODO: set to true ?
        )
        
        rsms
      }
      
      // Get all leaves result sets to get MSISearch information and MsQuery info
      val leaveResultSetLoader = () => {
        logger.debug("Loading leaves result sets (quantitation)...")
        val resultSets = _loadLeavesResultSets(lazyQuantRSM.lazyResultSummary.getResultSetId(), msiDbHelper, lazyRsProvider)

        logger.debug("Sorting leaves result sets by quant channel order (quantitation)...")
        val lazyRsById = resultSets.mapByLong(_.id)

        val rsIdByRsmId = msiDbHelper.getResultSetIdByResultSummaryId(identRsmIds)
        val sortedResultSets = quantChannels.map { qc => 
            rsIdByRsmId.get(qc.identResultSummaryId).map(lazyRsById.getOrElse(_, null)).orNull
        }

        // Check we had not problem to retrieve the result sets corresponding to the quant channels
        // If we have a problem then we return the result sets in the previous order
        if (sortedResultSets.contains(null) || (sortedResultSets.length!=resultSets.length)) resultSets else sortedResultSets
      }


      var peptideCountByProtMatchIdByQCId: LongMap[LongMap[Int]] = null
      var peptideCountByMqProtSetIdByQCId: LongMap[LongMap[Int]] = null
      
      if (mode == ExportConfigConstant.MODE_QUANT_XIC) {
        
        peptideCountByMqProtSetIdByQCId = new LongMap[LongMap[Int]]()
        
        // --- TEMPORARY FIX TO COMPUTE THE NUMBER OF MASTER QUANT PEPTIDES --- //
        // FIXME: do this in the IMQProteinSetSummarizer trait and update the MSIdb
        
        val qcCount = quantChannels.length

        // Initialize the map peptideCountByMqProtSetIdByQCId
        for ( quantChannel <- quantChannels) {
          peptideCountByMqProtSetIdByQCId.put(quantChannel.id,new LongMap[Int])
        }
        
        val mqProtSets = lazyQuantRSM.masterQuantProteinSets
        
        val mqPepByPepInstId = lazyQuantRSM.masterQuantPeptides
        .withFilter(_.peptideInstance.isDefined)
        .toLongMapWith { mqp => mqp.peptideInstance.get.id -> mqp }
        
        val qcPeptideCountMap = new LongMap[LongMap[Int]]()
        qcPeptideCountMap.sizeHint(mqProtSets.length)
        
        for( mqProtSet <- mqProtSets ) {

          val qProtSetByQChId =  mqProtSet.getQuantComponentMap()
          val nbPepCountDefined = qProtSetByQChId.values.count(_.peptidesCount.isDefined)
          if(nbPepCountDefined >0 ) {
            for ((qcId, qCom) <- qProtSetByQChId) {
              val nbPep = if (qCom.peptidesCount.isDefined) qCom.peptidesCount.get else 0
              peptideCountByMqProtSetIdByQCId(qcId).getOrElseUpdate(mqProtSet.id, nbPep)
            }
          } else {

            val pepInsts = mqProtSet.proteinSet.peptideSet.getPeptideInstances()
            val pepCountByQcId = new LongMap[Int](qcCount)

            for (pepInst <- pepInsts) {

              // If the peptide has been quantified
              val mqPepOpt = mqPepByPepInstId.get(pepInst.id)
              if (mqPepOpt.isDefined) {

                val mqPep = mqPepOpt.get

                if (mqPep.selectionLevel >= 2) {
                  for ((qcId, quantPep) <- mqPep.quantPeptideMap) {
                    if (quantPep.abundance > 0) {
                      pepCountByQcId.getOrElseUpdate(qcId, 0)
                      pepCountByQcId(qcId) += 1
                    }
                  }
                }
              }
            }
            for ((qcId, pepCount) <- pepCountByQcId) {
              peptideCountByMqProtSetIdByQCId(qcId).getOrElseUpdate(mqProtSet.id, pepCount)
            }
          }
        }
      } else {
        
        peptideCountByProtMatchIdByQCId = new LongMap[LongMap[Int]]()
        
        for (qc <- quantChannels) {
          val pepCountByProtMatchId = new LongMap[Int]()
          
          DoJDBCWork.withEzDBC(msiDbCtx) { ezDBC =>
            //---- Read PepCount
            val getPepCountSqlQuery = "SELECT peptide_count, protein_match_id " +
              "FROM peptide_set_protein_match_map pspmm, peptide_set " +
              "WHERE pspmm.result_summary_id = ? AND peptide_set.id = pspmm.peptide_set_id"
            
            ezDBC.selectAndProcess(getPepCountSqlQuery, qc.identResultSummaryId) { record =>
              val protMatchPepNbr = record.getInt("peptide_count")
              val protMatchId = record.getLong("protein_match_id")
              pepCountByProtMatchId.put( protMatchId, protMatchPepNbr )
            }
          }
          
          peptideCountByProtMatchIdByQCId.put(qc.id, pepCountByProtMatchId)
        }
        
      }

      logger.debug("Build QuantDataSet")

      val quantDs = new QuantDataset(
        projectName,
        lazyQuantRSM,
        expDesignOpt,
        quantConfigAndMethodOpt,
        profilizerConfigOpt,
        masterQc,
        groupSetupNumber,
        identResultSummariesLoader,
        leaveResultSetLoader,
        this._buildBioSequenceLoader(msiDbCtx, lazyQuantRSM.lazyResultSummary),
        this._buildSpectraLoader(msiDbCtx),
        this._buildPepMatchesLoader(executionContext),
        this._buildPepMatchesByMsQIdLoader(executionContext),
        Option(peptideCountByProtMatchIdByQCId),
        Option(peptideCountByMqProtSetIdByQCId)
      )

      this.apply(quantDs, viewSetName, viewSetTemplate, exportConfig)
    }

  }

  private def _loadLeavesResultSets(rsId: Long, msiDbHelper: MsiDbHelper, lazyRsProvider: SQLLazyResultSetProvider): Array[LazyResultSet] = {

    val leavesRsIds = msiDbHelper.getResultSetLeavesId(rsId)
    val leavesResultSets = lazyRsProvider.getLazyResultSets(leavesRsIds)

    if (leavesResultSets.exists(lrs => StringUtils.isEmpty(lrs.descriptor.name)) )
      leavesResultSets.sortBy(_.id) //If no name, use ID should always have one !
    else
      leavesResultSets.sortBy(_.descriptor.name)
  }

  private def _buildBioSequenceLoader(msiDbCtx: MsiDbConnectionContext, lazyRsm: LazyResultSummary) = { () =>
    logger.debug("Loading biological sequences...")

    val bioSeqProvider = new SQLBioSequenceProvider(msiDbCtx)

    val protMatches = lazyRsm.lazyResultSet.proteinMatches
    val protIds = new ArrayBuffer[Long](protMatches.length)

    for (protMatch <- protMatches) {
      val protId = protMatch.getProteinId()
      if (protId > 0) {
        protIds += protId
      }
    }

    bioSeqProvider.getBioSequences(protIds, loadSequence = false)
  }

  private def _buildSpectraLoader(msiDbCtx: MsiDbConnectionContext) = { spectraIds: Array[Long] =>
    logger.debug(s"Loading spectra descriptors... ${spectraIds.length}")

    val spectrumProvider = new SQLSpectrumProvider(msiDbCtx)
    spectrumProvider.getSpectra(spectraIds, loadPeaks = false)
  }

  private def _buildPepMatchesLoader(executionContext: IExecutionContext) = { pepMatchesId: Array[Long] =>
    logger.debug(s"Loading peptide Matches ... ${pepMatchesId.length}")
    val peptideMatchProvider = new SQLPeptideMatchProvider(PeptideCacheExecutionContext(executionContext))
    peptideMatchProvider.getPeptideMatches(pepMatchesId)

  }

  private def _buildPepMatchesByMsQIdLoader(executionContext: IExecutionContext) = { msQueriesIds: Array[Long] =>
    logger.debug(s"Loading peptide Matches from msQuery Ids ... ${msQueriesIds.length}")
    val peptideMatchProvider = new SQLPeptideMatchProvider(PeptideCacheExecutionContext(executionContext))
    peptideMatchProvider.getPeptideMatchesByMsQueryIds(msQueriesIds)

  }
}