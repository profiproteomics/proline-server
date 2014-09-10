package fr.proline.module.fragment_match.service

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.api.service.IService
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.dal.tables.SelectQueryBuilder1
import fr.proline.core.om.model.msi.IRsContainer
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.IResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLMsQueryProvider
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideMatchProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import fr.proline.core.om.storer.msi.RsStorer
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.module.fragment_match.PeptideSpectrumMatcher
import fr.proline.core.om.storer.msi.impl.SQLRsWriter

object SpectrumMatchesGenerator {

  def _loadResultSet(rsId: Long, execContext: IExecutionContext): ResultSet = {
    val rsProvider = getResultSetProvider(execContext)
    val rs = rsProvider.getResultSet(rsId)
    require(rs.isDefined, "Unknown ResultSet Id: " + rsId)
    rs.get
  }

  def _getSpectraIds(rs: ResultSet): Seq[Long] = {
    rs.peptideMatches.map(_.getMs2Query.spectrumId)
  }

  def _loadResultSummary(rsmId: Long, execContext: IExecutionContext): ResultSummary = {
    val rsmProvider = getResultSummaryProvider(execContext)
    val rsm = rsmProvider.getResultSummary(rsmId, true)
    require(rsm.isDefined, "Unknown ResultSummary Id: " + rsmId)

    rsm.get
  }

  private def getResultSummaryProvider(execContext: IExecutionContext): IResultSummaryProvider = {
    new SQLResultSummaryProvider(execContext.getMSIDbConnectionContext,
      execContext.getPSDbConnectionContext,
      execContext.getUDSDbConnectionContext)
  }

  private def getResultSetProvider(execContext: IExecutionContext): IResultSetProvider = {
    new SQLResultSetProvider(execContext.getMSIDbConnectionContext,
      execContext.getPSDbConnectionContext,
      execContext.getUDSDbConnectionContext)

  }

}

class SpectrumMatchesGenerator(
  executionContext: IExecutionContext,
  resultSetId: Long,
  resultSummaryId: Option[Long] = None,
  peptideMatchIds: Option[Array[Long]] = None) extends IService with Logging {

  def runService(): Boolean = {
    logger.info("Run service SpectrumMatchesGenerator on ResultSet.id=" + resultSetId)

    val msiDbCtx = executionContext.getMSIDbConnectionContext
    var storerContext: StorerContext = null
    val rsStorer = RsStorer(msiDbCtx)
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

      if (peptideMatchIds.isDefined) {

        val msiDbHelper = new MsiDbHelper(msiDbCtx)
        val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext(),
          executionContext.getMSIDbConnectionContext(),
          executionContext.getPSDbConnectionContext())
        val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(resultSetId))(0))
//        if (msiSearch.isDefined && msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
        if (msiSearch.isDefined && msiSearch.get.resultFileName.endsWith("dat")) {
	        if (msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
	          val msmsSearchSettings = msiSearch.get.searchSettings.msmsSearchSettings.get
	
	          val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, executionContext.getPSDbConnectionContext)
	          val peptideMatches = peptideMatchProvider.getPeptideMatches(peptideMatchIds.get)
	          val spectrumIds = peptideMatches.map(_.getMs2Query.spectrumId)
	          val spectra = spectrumProvider.getSpectra(spectrumIds)
	          val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
	          val psmMatcher = new PeptideSpectrumMatcher(spectraById, msmsSearchSettings.ms2ErrorTol, msmsSearchSettings.ms2ErrorTolUnit)
	          logger.info("Storing spectrum matches...")
	          for (peptideMatch <- peptideMatches) {
	            val spectrumMatch = psmMatcher.getSpectrumMatch(peptideMatch)
	            SQLRsWriter.insertSpectrumMatch(peptideMatch, spectrumMatch, msiDbCtx)
	          }
	        }
        } else { logger.debug("Peptide-Spectrum Matching is restricted to Mascot result files") }
      } else {
        storerContext = StorerContext(executionContext)

        val (resultSet, rsm) = {
          if (!resultSummaryId.isDefined) {
            (SpectrumMatchesGenerator._loadResultSet(resultSetId, executionContext), null)
          } else {
            val rsm = SpectrumMatchesGenerator._loadResultSummary(resultSummaryId.get, executionContext)
            (rsm.resultSet.get, rsm)
          }
        }
        //TODO : load resultSet Spectrum, build a map (spectrum.id -> spectrum) then creates a PeptideSpectrumMatcher
//        if (resultSet.msiSearch.isDefined && resultSet.msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
        if (resultSet.msiSearch.isDefined && resultSet.msiSearch.get.resultFileName.endsWith("dat")) {
	        if (resultSet.msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
	          val msmsSearchSettings = resultSet.msiSearch.get.searchSettings.msmsSearchSettings.get
	
	          val spectra = {
	            if (resultSummaryId.isDefined) {
	              spectrumProvider.getSpectra(SpectrumMatchesGenerator._getSpectraIds(rsm.resultSet.get))
	            } else {
	              spectrumProvider.getSpectra(SpectrumMatchesGenerator._getSpectraIds(resultSet))
	            }
	          }
	
	          val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
	          val psmMatcher = new PeptideSpectrumMatcher(spectraById, msmsSearchSettings.ms2ErrorTol, msmsSearchSettings.ms2ErrorTolUnit)
	          logger.info("Storing spectrum matches...")
	          rsStorer.insertSpectrumMatches(resultSet, new ResultSetWrapper(resultSet, psmMatcher), storerContext)
	        } else {
	          logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
	          throw new RuntimeException("ResultSet " + resultSet.id + " Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
	        }
        } else { logger.debug("Peptide-Spectrum Matching is restricted to Mascot result files") }
      }
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

}

class ResultSetWrapper(resultSet: ResultSet, psmMatcher: PeptideSpectrumMatcher) extends IRsContainer {

  def getResultSet(wantDecoy: Boolean): ResultSet = {
    resultSet
  }

  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {

  }

  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {
    for (peptideMatch <- resultSet.peptideMatches) {
      if (psmMatcher.spectraByIds.contains(peptideMatch.getMs2Query.spectrumId)) {
        onEachSpectrumMatch(psmMatcher.getSpectrumMatch(peptideMatch))
      }
    }
  }

}