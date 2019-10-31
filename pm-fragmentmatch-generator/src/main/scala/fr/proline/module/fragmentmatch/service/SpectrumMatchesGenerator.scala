package fr.proline.module.fragmentmatch.service

import java.sql.Connection

import com.typesafe.scalalogging.LazyLogging
import fr.profi.api.service.IService
import fr.proline.context.DatabaseConnectionContext
import fr.proline.context.IExecutionContext
import fr.proline.context.MsiDbConnectionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.om.model.msi.FragmentationRuleSet
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.SearchSettings
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLFragmentationRuleProvider
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideMatchProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import fr.proline.core.om.storer.msi.impl.SQLPeaklistWriter
import fr.proline.core.om.storer.msi.impl.SQLRsWriter
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.module.fragmentmatch.FragmentationRuleSetSource
import fr.proline.module.fragmentmatch._
import fr.proline.repository.util.JDBCWork

import scala.Array.fallbackCanBuildFrom
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object SpectrumMatchesGenerator {


  def _loadResultSet(rsId: Long, execContext: IExecutionContext): ResultSet = {
    val rsProvider = new SQLResultSetProvider(PeptideCacheExecutionContext(execContext))
    val rs = rsProvider.getResultSet(rsId)
    require(rs.isDefined, "Unknown ResultSet Id: " + rsId)
    rs.get
  }

  def _getSpectraIds(rs: ResultSet): Seq[Long] = {
    rs.peptideMatches.map(_.getMs2Query().spectrumId)
  }

  def _loadResultSummary(rsmId: Long, execContext: IExecutionContext): ResultSummary = {
    val rsmProvider = new SQLResultSummaryProvider(PeptideCacheExecutionContext(execContext))
    val rsm = rsmProvider.getResultSummary(rsmId, loadResultSet = true)
    require(rsm.isDefined, "Unknown ResultSummary Id: " + rsmId)

    rsm.get
  }

  val DEFAULT_FRS_NAME = "ESI-QUAD-TOF"


}

class SpectrumMatchesGenerator(
                                executionContext: IExecutionContext,
                                resultSetId: Long,
                                resultSummaryId: Option[Long] = None,
                                peptideMatchIds: Option[Array[Long]] = None,
                                fragmentationRuleSetIdOpt: Option[Long] = None,
                                forceInsert: Boolean = false) extends IService with LazyLogging {



  def runService(): Boolean = {
    logger.info("Run service SpectrumMatchesGenerator on ResultSet.id=" + resultSetId)

    val msiDbCtx = executionContext.getMSIDbConnectionContext
    var storerContext: StorerContext = null

    var localMSITransaction: Boolean = false
    var msiTransacOk: Boolean = false

    try {

      // Check if a transaction is already initiated
      if (!msiDbCtx.isInTransaction) {
        msiDbCtx.beginTransaction()
        localMSITransaction = true
        msiTransacOk = false
      }

      val spectrumProvider = new SQLSpectrumProvider(msiDbCtx)
      val pepProvider = new SQLPeptideProvider(PeptideCacheExecutionContext(executionContext))

      if (peptideMatchIds.isDefined) {

        val msiSearch = getAndTestMSISearch(resultSetId, msiDbCtx)
        val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, pepProvider)
        //Generate SpectrumMatches for specified peptide Matches without updating associated ResultSet.SearchSetting.FragmentationRuleSet
        generateSpectrumMatchesFor(peptideMatchIds.get, msiSearch.searchSettings, false, peptideMatchProvider, spectrumProvider, msiDbCtx)

      } else {
        storerContext = StorerContext(executionContext)
        val allPepMatchIdsByMsiSearchId = new HashMap[Long, ArrayBuffer[Long]] //TODO : Reverse Map PepMatchId[] per MsiSearchId


        if (resultSummaryId.isEmpty) {
          logger.debug("Get all peptide matches of the given RS")
          val jdbcWork = new JDBCWork() {

            override def execute(con: Connection) {
              val pStmt = con.prepareStatement("SELECT pm.id, mq.msi_search_id from peptide_match pm join ms_query mq on pm.ms_query_id = mq.id WHERE pm.result_set_id = ?")
              pStmt.setLong(1, resultSetId)
              val sqlResultSet = pStmt.executeQuery()
              while (sqlResultSet.next) {
                allPepMatchIdsByMsiSearchId.getOrElseUpdate(sqlResultSet.getLong(2), new ArrayBuffer()) += sqlResultSet.getLong(1)
              }
              pStmt.close()
            }

          } // End of jdbcWork anonymous inner class    	 

          executionContext.getMSIDbConnectionContext.doWork(jdbcWork, false)
        } else {
          logger.debug("Get all peptide matches of the given RSM")
          val jdbcWork = new JDBCWork() {

            override def execute(con: Connection) {
              val pStmt = con.prepareStatement("SELECT pm.id, mq.msi_search_id FROM peptide_match pm, peptide_instance_peptide_match_map pipm, ms_query mq " +
                " WHERE pipm.peptide_match_id = pm.id AND pm.ms_query_id = mq.id AND pipm.result_summary_id = ? ")
              pStmt.setLong(1, resultSummaryId.get)
              val sqlResultSet = pStmt.executeQuery()
              while (sqlResultSet.next) {
                allPepMatchIdsByMsiSearchId.getOrElseUpdate(sqlResultSet.getLong(2), new ArrayBuffer()) += sqlResultSet.getLong(1)
              }
              pStmt.close()
            }

          } // End of jdbcWork anonymous inner class    	 

          executionContext.getMSIDbConnectionContext.doWork(jdbcWork, false)
        }

        val msiSearchesById = getAndTestAllMSISearches(resultSetId, msiDbCtx)
        val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, pepProvider)

        logger.info("" + allPepMatchIdsByMsiSearchId.values.flatten.size + " peptide matches will be generated")
        // group by msi_search_id
        allPepMatchIdsByMsiSearchId.keys.foreach(msiSearchId => {
          // then do like before (split by 1000)
          val pepMatchIDsIterator = allPepMatchIdsByMsiSearchId(msiSearchId).iterator.sliding(1000, 1000)
          while (pepMatchIDsIterator.hasNext) {
            val pepMatchIds = pepMatchIDsIterator.next
            //Generate SpectrumMatches for specified peptide Matches from RS or RSM and update associated ResultSet.SearchSetting.FragmentationRuleSet if needed
            generateSpectrumMatchesFor(pepMatchIds, msiSearchesById(msiSearchId).searchSettings, true, peptideMatchProvider, spectrumProvider, msiDbCtx)
          } //End go through slinding windows
        })

      } // End peptideMatchIds Not Defined

      // Commit transaction if it was initiated locally
      if (localMSITransaction) {
        msiDbCtx.commitTransaction()
      }

      msiTransacOk = true
    } finally {

      if (storerContext != null) {
        storerContext.clearContext()
      }

      if (localMSITransaction && !msiTransacOk) {
        logger.info("Rollbacking MSI Db Transaction")

        try {
          msiDbCtx.rollbackTransaction()
        } catch {
          case ex: Exception => logger.error("Error rollbacking MSI Db Transaction", ex)
        }

      }

      // Execution context should be closed by the service caller
      //executionContext.closeAll()

    }

    this.beforeInterruption()
    logger.debug("End of result file importer service")

    msiTransacOk
  }

  private def getAndTestMSISearch(resultSetId: Long, msiDbCtx: DatabaseConnectionContext): MSISearch = {
    val msiDbHelper = new MsiDbHelper(msiDbCtx)
    val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext,
      executionContext.getMSIDbConnectionContext)

    val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(resultSetId))(0))
    if (msiSearch.isEmpty || msiSearch.get.searchSettings.msmsSearchSettings.isEmpty) {
      logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
      throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
    }
    msiSearch.get
  }

  private def getAndTestAllMSISearches(resultSetId: Long, msiDbCtx: DatabaseConnectionContext): Map[Long, MSISearch] = {
    val msiDbHelper = new MsiDbHelper(msiDbCtx)
    val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext,
      executionContext.getMSIDbConnectionContext)

    val msiSearches = new HashMap[Long, MSISearch]
    val msiSearchIds = msiDbHelper.getResultSetsMsiSearchIds(Array(resultSetId))
    msiSearchIds.foreach(msiSearchId => {
      val msiSearch = msiSearchProvider.getMSISearch(msiSearchId)
      if (msiSearch.isEmpty || msiSearch.get.searchSettings.msmsSearchSettings.isEmpty) {
        logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
        throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
      }
      msiSearches(msiSearchId) = msiSearch.get
    })
    msiSearches.toMap
  }


  private def getFragmentationRuleSet(searchSettings: SearchSettings): (Option[FragmentationRuleSet], FragmentationRuleSetSource.Value) = {

    // Get FramentationRuleSet to use
    val fragmentationRulesetProvider: SQLFragmentationRuleProvider = new SQLFragmentationRuleProvider(executionContext.getUDSDbConnectionContext)
    var frsSource = FragmentationRuleSetSource.NONE

    val fragmentationRuleSet2Use: Option[FragmentationRuleSet] = if (fragmentationRuleSetIdOpt.isDefined) {
      val userFragmentationRuleSet = fragmentationRulesetProvider.getFragmentationRuleSet(fragmentationRuleSetIdOpt.get)
      if (userFragmentationRuleSet.isEmpty) { //user specified FRS don't exist.
        if (searchSettings.fragmentationRuleSet.isDefined) {
          frsSource = FragmentationRuleSetSource.SEARCH_SETTINGS
          searchSettings.fragmentationRuleSet
        } else {
          frsSource = FragmentationRuleSetSource.DEFAULT
          fragmentationRulesetProvider.getFragmentationRuleSet(SpectrumMatchesGenerator.DEFAULT_FRS_NAME)
        }
      } else {
        frsSource = FragmentationRuleSetSource.USER_SPECIFIED
        userFragmentationRuleSet
      }
    } else { //No User specified
      if (searchSettings.fragmentationRuleSet.isDefined) {
        frsSource = FragmentationRuleSetSource.SEARCH_SETTINGS
        searchSettings.fragmentationRuleSet
      } else {
        frsSource = FragmentationRuleSetSource.DEFAULT
        fragmentationRulesetProvider.getFragmentationRuleSet(SpectrumMatchesGenerator.DEFAULT_FRS_NAME)
      }
    }

    (fragmentationRuleSet2Use, frsSource)
  }

  private def generateSpectrumMatchesFor(pepMatchIds: Seq[Long], searchSettings: SearchSettings, updateSearchSettingAllowed: Boolean, peptideMatchProvider: SQLPeptideMatchProvider,
                                         spectrumProvider: SQLSpectrumProvider, msiDbCtx: MsiDbConnectionContext) {

    val ms2ErrorTol: Double = searchSettings.msmsSearchSettings.get.ms2ErrorTol
    val ms2ErrorTolUnitStr: String = searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit


    var fragmentationRuleSet2Use: Option[FragmentationRuleSet] = None
    var frsSource: FragmentationRuleSetSource.Value = FragmentationRuleSetSource.NONE

      // Get FramentationRuleSet to use
      try {
        val (fragmentationRuleSet2Use1, frsSource1) = getFragmentationRuleSet(searchSettings)
        fragmentationRuleSet2Use = fragmentationRuleSet2Use1
        frsSource = frsSource1
      } catch {
        case e:Exception => {
          throw new Exception(" Unable to get Fragmentation Rule Set. Try another one... ")
        }
      }

    if (fragmentationRuleSet2Use.isEmpty || fragmentationRuleSet2Use.get.fragmentationRules.isEmpty)
      frsSource = FragmentationRuleSetSource.NONE

    //Get list of PeptideMatches having spectrum matches registered for.
    val existingMatchPepMatchIdsBuilder = Array.newBuilder[Long]
    val associatedObjectTreeBuilder = Array.newBuilder[Long]
    var generatePepMatchesId = pepMatchIds
    val getExistingMatchesWork = new JDBCWork() {
      override def execute(con: Connection) {

        val query = "SELECT peptide_match_object_tree_map.peptide_match_id, peptide_match_object_tree_map.object_tree_id FROM peptide_match_object_tree_map " +
          " WHERE peptide_match_object_tree_map.peptide_match_id IN (" + pepMatchIds.mkString(",") + ") AND peptide_match_object_tree_map.schema_name = 'peptide_match.spectrum_match'; "
        val stmt = con.createStatement()
        val sqlResultSet = stmt.executeQuery(query)
        while (sqlResultSet.next) {
          existingMatchPepMatchIdsBuilder += sqlResultSet.getLong(1)
          associatedObjectTreeBuilder += sqlResultSet.getLong(2)
        }
        stmt.close()
      }
    } // End of jdbcWork anonymous inner class
    executionContext.getMSIDbConnectionContext.doWork(getExistingMatchesWork, false)

    val existingMatchPepMatchIds = existingMatchPepMatchIdsBuilder.result
    if (existingMatchPepMatchIds.length > 0) {
      logger.warn("There is " + existingMatchPepMatchIds.length + " peptide matches with existing spectrum match.")

      //If forceInsert set to true, remove existing spectrum matches Otherwise, remove these Ids from peptideMatches to considere.
      if (forceInsert) {
        // Remove existing spectrum matches

        val removeExistingMatchesWork = new JDBCWork() {
          override def execute(con: Connection) {

            var query = "DELETE FROM peptide_match_object_tree_map WHERE peptide_match_id IN (" + existingMatchPepMatchIds.mkString(",") + ") AND schema_name = 'peptide_match.spectrum_match'; "
            val stmt = con.createStatement()
            stmt.execute(query)

            query = "DELETE FROM object_tree WHERE id IN (" + associatedObjectTreeBuilder.result.mkString(",") + ");"
            stmt.execute(query)

            stmt.close()
          }

        } // End of jdbcWork anonymous inner class
        executionContext.getMSIDbConnectionContext.doWork(removeExistingMatchesWork, false)

      } else {
        // just Ignore existing spectrum matches
        generatePepMatchesId = pepMatchIds.diff(existingMatchPepMatchIds)
      }
    }

    //Generate and save Spectrum Matches
    val peptideMatches = peptideMatchProvider.getPeptideMatches(generatePepMatchesId)
    val spectrumIds = peptideMatches.map(_.getMs2Query().spectrumId)
    val spectra = spectrumProvider.getSpectra(spectrumIds)
    val spectraById = Map() ++ spectra.map { sp => sp.id -> sp }
    val psmMatcher = {
      searchSettings.softwareName.toLowerCase() match {
        case "mascot" => new PeptideSpectrumMatcherMascot(spectraById, ms2ErrorTol, ms2ErrorTolUnitStr, fragmentationRuleSet2Use, frsSource)
        case "omssa" => new PeptideSpectrumMatcherOmssa(spectraById, ms2ErrorTol, ms2ErrorTolUnitStr, fragmentationRuleSet2Use, frsSource)
        case "xtandem" => new PeptideSpectrumMatcherXtandem(spectraById, ms2ErrorTol, ms2ErrorTolUnitStr, fragmentationRuleSet2Use, frsSource)
        case _ =>
          logger.error("Spectrum matches cannot be generated for this search engine")
          throw new RuntimeException("Spectrum matches cannot be generated for this search engine")
      }
    }

    val updateFragRuleSetNeeded: Boolean = psmMatcher.isUpdateFragRuleSetNeeded(searchSettings, fragmentationRuleSet2Use, frsSource)


    logger.info("Storing spectrum matches...")
    for (peptideMatch <- peptideMatches) {
      val spectrumMatch = psmMatcher.getSpectrumMatch(peptideMatch)
      SQLRsWriter.insertSpectrumMatch(peptideMatch, spectrumMatch, msiDbCtx)
    //  if (updateFragRuleSetNeeded) { Alawys save frs used in spectrum
        SQLPeaklistWriter.updateSpectraFragmentationRuleSet(peptideMatch.getMs2Query().spectrumId, fragmentationRuleSet2Use.get.id, msiDbCtx)
      //}
    }

    if (updateFragRuleSetNeeded && updateSearchSettingAllowed) {
      //Save in ss. if(fragmentationRuleSet2Use.isDefined) => Not needed updateFragRuleSetNeeded ensure its true.
      SQLRsWriter.updateSearchSettingsFragmentationRuleSet(searchSettings.id, fragmentationRuleSet2Use.get.id, msiDbCtx)

    }
  }

}


