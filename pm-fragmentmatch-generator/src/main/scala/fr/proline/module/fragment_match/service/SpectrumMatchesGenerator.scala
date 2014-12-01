package fr.proline.module.fragment_match.service

import java.sql.Connection
import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.api.service.IService
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.om.model.msi.IRsContainer
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.IResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideMatchProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import fr.proline.core.om.storer.msi.RsStorer
import fr.proline.core.om.storer.msi.impl.SQLRsWriter
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.module.fragment_match.PeptideSpectrumMatcher
import fr.proline.module.fragment_match.PeptideSpectrumMatcherMascot
import fr.proline.module.fragment_match.PeptideSpectrumMatcherOmssa
import fr.proline.repository.util.JDBCWork
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.om.model.msi.SearchSettings

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
  peptideMatchIds: Option[Array[Long]] = None,
  forceInsert : Boolean = false ) extends IService with Logging {

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

        val msiSearch = getAndTestMSISearch(resultSetId, msiDbCtx)
        val ms2ErrorTol = msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTol
	    val ms2ErrorTolUnit = msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
	    val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, executionContext.getPSDbConnectionContext)
        
        generateSpectrumMatchesFor(peptideMatchIds.get, forceInsert , ms2ErrorTol, ms2ErrorTolUnit, msiSearch.searchSettings, peptideMatchProvider,spectrumProvider,  msiDbCtx )
        
      } else {
        storerContext = StorerContext(executionContext)
        val allPepMatchIds = Seq.newBuilder[Long]
        
    	if (!resultSummaryId.isDefined) {
    		val jdbcWork = new JDBCWork() {
	            override def execute(con: Connection) {
		
		              val pStmt = con.prepareStatement("SELECT id from peptide_match WHERE result_set_id = ?")
		              pStmt.setLong(1, resultSetId)
		              val sqlResultSet = pStmt.executeQuery()
		              while(sqlResultSet.next){
		                allPepMatchIds += sqlResultSet.getLong("id")
		              }		                
		              pStmt.close()
	            }
		
	          } // End of jdbcWork anonymous inner class    	 
		
	          executionContext.getMSIDbConnectionContext().doWork(jdbcWork, false)
    	} else {
	          val jdbcWork = new JDBCWork() {
	        	  override def execute(con: Connection) {
		
		              val pStmt = con.prepareStatement("SELECT peptide_match.id FROM peptide_match, peptide_instance_peptide_match_map  pipm "+ 
		            		  	" WHERE pipm.peptide_match_id = peptide_match.id AND pipm.result_summary_id = ? ")
		              pStmt.setLong(1, resultSummaryId.get)
		              val sqlResultSet = pStmt.executeQuery()
		              while(sqlResultSet.next){
		                allPepMatchIds += sqlResultSet.getLong("id")
		              }		                
		              pStmt.close()
		            }
		
	          } // End of jdbcWork anonymous inner class    	 
		
	          executionContext.getMSIDbConnectionContext().doWork(jdbcWork, false)        	  
    	}
        	        
    	val msiSearch = getAndTestMSISearch(resultSetId, msiDbCtx )       	
    	val ms2ErrorTol = msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTol
		val ms2ErrorTolUnit = msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
		val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, executionContext.getPSDbConnectionContext)
        	
		val pepMatchIDsIterator = allPepMatchIds.result.sliding(1000,1000)
		while(pepMatchIDsIterator.hasNext){
		   val pepMatchIds = pepMatchIDsIterator.next
			   
		   generateSpectrumMatchesFor(pepMatchIds, forceInsert , ms2ErrorTol, ms2ErrorTolUnit, msiSearch.searchSettings, peptideMatchProvider,spectrumProvider , msiDbCtx )
		} //End go through slinding windows
                     
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
    val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext(),
      executionContext.getMSIDbConnectionContext(),
      executionContext.getPSDbConnectionContext())

    //TODO Get ResultSetId for case only RSM specified !
    val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(resultSetId))(0))
    if (!msiSearch.isDefined || !msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
      logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
      throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
    }
    msiSearch.get
  }
  
   private def generateSpectrumMatchesFor(pepMatchIds : Seq[Long], forceInsert : Boolean,  ms2ErrorTol: Double,  
		  		ms2ErrorTolUnitStr: String,  searchSettings: SearchSettings, peptideMatchProvider :SQLPeptideMatchProvider,spectrumProvider : SQLSpectrumProvider, 
		  		msiDbCtx : DatabaseConnectionContext ) {
  

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
    executionContext.getMSIDbConnectionContext().doWork(getExistingMatchesWork, false)

    val existingMatchPepMatchIds = existingMatchPepMatchIdsBuilder.result
    if (existingMatchPepMatchIds.length > 0)
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
      executionContext.getMSIDbConnectionContext().doWork(removeExistingMatchesWork, false)

    } else {
      // just Ignore existing spectrum matches
      generatePepMatchesId = pepMatchIds.diff(existingMatchPepMatchIds)
    }

    //Generate and save Spectrum Matches 
    val peptideMatches = peptideMatchProvider.getPeptideMatches(generatePepMatchesId)
    val spectrumIds = peptideMatches.map(_.getMs2Query.spectrumId)
    val spectra = spectrumProvider.getSpectra(spectrumIds)
    val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
    val psmMatcher = {
      searchSettings.softwareName.toLowerCase() match {
        case "mascot" => new PeptideSpectrumMatcherMascot(spectraById, ms2ErrorTol, ms2ErrorTolUnitStr, searchSettings.instrumentConfig)
        case "omssa"  => new PeptideSpectrumMatcherOmssa(spectraById, ms2ErrorTol, ms2ErrorTolUnitStr)
        case _ =>
          logger.error("Spectrum matches cannot be generated for this search engine")
          throw new RuntimeException("Spectrum matches cannot be generated for this search engine")
      }
    }
    logger.info("Storing spectrum matches...")
    for (peptideMatch <- peptideMatches) {
      val spectrumMatch = psmMatcher.getSpectrumMatch(peptideMatch)
      SQLRsWriter.insertSpectrumMatch(peptideMatch, spectrumMatch, msiDbCtx)
    }
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
