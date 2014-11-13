package fr.proline.module.quality.msdiag.service

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.api.service.IService
import fr.proline.context.IExecutionContext
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.IResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.storer.msi.RsStorer
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.module.quality.msdiag.MSDiag
import fr.profi.util.serialization.ProfiJson
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import java.util.HashMap


object MSDiagReportGenerator {

  
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

class MSDiagReportGenerator(
  executionContext: IExecutionContext,
  resultSetId: Long,
  resultSummaryId: Option[Long] = None,
  peptideMatchIds: Option[Array[Long]] = None/*,
  memorySave : Boolean = false */) extends IService with Logging {
  
  //var resultString : String = "" // this is where the message is taken back from the service
   var resultString_MassesPerCharge : String = null
   var resultString_AssignementRepartition : String = null
   var resultString_MassesPerScore :String  = null
   var resultString_MatchesPerChargeAndScore : String = null 
   var resultString_MatchesPerMinuteAndScore : String  = null
   var resultString_MatchesPerResultSetAndScore : String = null
   var resultString_MatchesPerScanAndScore : String = null
   var resultHashMap : HashMap [String,String] = null
   var resultHashMapJson : String = ""
  
  def runService(): Boolean = {
    logger.info("Run service MSDiagDataGenerator on ResultSet.id=" + resultSetId)

    val msiDbCtx = executionContext.getMSIDbConnectionContext
    var storerContext: StorerContext = null
    val rsStorer = RsStorer(msiDbCtx)
    var localMSITransaction: Boolean = false
    var msiTransacOk: Boolean = false

    
   
    val parserContext = ProviderDecoratedExecutionContext(executionContext)
    //val id = loadFile("SGI_F154964.dat")
    val id = resultSetId
    val msdiag = new MSDiag(id, parserContext)
    
    // here serialize (and json) the resulting object to be sent to prolinestudio...
    
    var  msOutputMSDiagMassesPerCharge : MSDiagOutput = msdiag.getMSDiagMassesPerCharge
    var  msOutputMSDiagAssignementRepartition : MSDiagOutput = msdiag.getMSDiagAssignementRepartition
    var  msOutputMSDiagMassesPerScore : MSDiagOutput = msdiag.getMSDiagMassesPerScore
    var  msOutputMSDiagMatchesPerChargeAndScore : MSDiagOutput = msdiag.getMSDiagMatchesPerChargeAndScore
    // next one needs parsing rules. TODO
    var  msOutputMSDiagMatchesPerMinuteAndScore : MSDiagOutput = msdiag.getMSDiagMatchesPerMinuteAndScore
    var  msOutputMSDiagMatchesPerResultSetAndScore : MSDiagOutput = msdiag.getMSDiagMatchesPerResultSetAndScore
    var  msOutputMSDiagMatchesPerScanAndScore : MSDiagOutput = msdiag.getMSDiagMatchesPerScanAndScore
    
    

    resultString_MassesPerCharge = ProfiJson.serialize(msOutputMSDiagMassesPerCharge)
    resultString_AssignementRepartition = ProfiJson.serialize(msOutputMSDiagAssignementRepartition)
    resultString_MassesPerScore = ProfiJson.serialize(msOutputMSDiagMassesPerScore)
    resultString_MatchesPerChargeAndScore = ProfiJson.serialize(msOutputMSDiagMatchesPerChargeAndScore)
    resultString_MatchesPerMinuteAndScore = ProfiJson.serialize(msOutputMSDiagMatchesPerMinuteAndScore)
    resultString_MatchesPerResultSetAndScore = ProfiJson.serialize(msOutputMSDiagMatchesPerResultSetAndScore)
    resultString_MatchesPerScanAndScore = ProfiJson.serialize(msOutputMSDiagMatchesPerScanAndScore)
    
    resultHashMap = new HashMap [String,String](0)
    resultHashMap.put("MassesPerCharge" , resultString_MassesPerCharge)
    resultHashMap.put("AssignementRepartition" , resultString_AssignementRepartition)
    resultHashMap.put("MassesPerScore" , resultString_MassesPerScore) // TODO: seems empty??
    resultHashMap.put("MatchesPerChargeAndScore" , resultString_MatchesPerChargeAndScore)
    resultHashMap.put("MatchesPerMinuteAndScore" , resultString_MatchesPerMinuteAndScore)
    resultHashMap.put("MatchesPerResultSetAndScore" , resultString_MatchesPerResultSetAndScore)
    resultHashMap.put("MatchesPerScanAndScore" , resultString_MatchesPerScanAndScore)
    
    resultHashMapJson = ProfiJson.serialize(resultHashMap) // this message will transit from service to webcore to prolinestudio...

    
    //resultString = gson
        
    
    //msdiag.setParsingRules(super.getParsingRules(8))
   // MSDiagViewer.load(msdiag)
   // return "message:MSDiagReportGenerator.scala replying...messageString"
    
    
//    try {
//
//      // Check if a transaction is already initiated
//      if (!msiDbCtx.isInTransaction) {
//        msiDbCtx.beginTransaction()
//        localMSITransaction = true
//        msiTransacOk = false
//      }
//
//     // val spectrumProvider = new SQLSpectrumProvider(msiDbCtx)
//
//      if (peptideMatchIds.isDefined) {
//
//        val msiDbHelper = new MsiDbHelper(msiDbCtx)
//        val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext(),
//          executionContext.getMSIDbConnectionContext(),
//          executionContext.getPSDbConnectionContext())
//        val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(resultSetId))(0))
//        if (msiSearch.isDefined && msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
//	      val ms2ErrorTol = msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTol
//	      val ms2ErrorTolUnit = msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
//	      val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, executionContext.getPSDbConnectionContext)
//          val peptideMatches = peptideMatchProvider.getPeptideMatches(peptideMatchIds.get)
//          val spectrumIds = peptideMatches.map(_.getMs2Query.spectrumId)
//          val spectra = spectrumProvider.getSpectra(spectrumIds)
//          val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
//	      val psmMatcher = {
//	        msiSearch.get.searchSettings.softwareName.toLowerCase() match {
//	          case "mascot" => new PeptideSpectrumMatcherMascot(spectraById, ms2ErrorTol, ms2ErrorTolUnit)
//	          case "omssa" => new PeptideSpectrumMatcherOmssa(spectraById, ms2ErrorTol, ms2ErrorTolUnit)
//	          case _ => 
//	            logger.error("Spectrum matches cannot be generated for this search engine")
//	            throw new RuntimeException("Spectrum matches cannot be generated for this search engine")
//	        }
//	      }
//	      logger.info("Storing spectrum matches...")
//          for (peptideMatch <- peptideMatches) {
//            val spectrumMatch = psmMatcher.getSpectrumMatch(peptideMatch)
//            SQLRsWriter.insertSpectrumMatch(peptideMatch, spectrumMatch, msiDbCtx)
//          }
//        } else {
//          logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
//	      throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
//        }
//      } else {
//        storerContext = StorerContext(executionContext)
//
//        if(memorySave){
//        	
//        	val pepMatchIdsBuilder = Array.newBuilder[Long]
//        
//        	if (!resultSummaryId.isDefined) {
//		          val jdbcWork = new JDBCWork() {
//		            override def execute(con: Connection) {
//		
//		              val pStmt = con.prepareStatement("SELECT id from peptide_match WHERE result_set_id = ?")
//		              pStmt.setLong(1, resultSetId)
//		              val sqlResultSet = pStmt.executeQuery()
//		              while(sqlResultSet.next){
//		                pepMatchIdsBuilder += sqlResultSet.getLong("id")
//		              }		                
//		              pStmt.close()
//		            }
//		
//		          } // End of jdbcWork anonymous inner class    	 
//		
//		          executionContext.getMSIDbConnectionContext().doWork(jdbcWork, false)
//        	} else {
//		          val jdbcWork = new JDBCWork() {
//		            override def execute(con: Connection) {
//		
//		              val pStmt = con.prepareStatement("SELECT peptide_match.id FROM peptide_match, peptide_instance_peptide_match_map  pipm "+ 
//		            		  	" WHERE pipm.peptide_match_id = peptide_match.id AND pipm.result_summary_id = ? ")
//		              pStmt.setLong(1, resultSummaryId.get)
//		              val sqlResultSet = pStmt.executeQuery()
//		              while(sqlResultSet.next){
//		                pepMatchIdsBuilder += sqlResultSet.getLong("id")
//		              }		                
//		              pStmt.close()
//		            }
//		
//		          } // End of jdbcWork anonymous inner class    	 
//		
//		          executionContext.getMSIDbConnectionContext().doWork(jdbcWork, false)        	  
//        	}
//        	        
//        	val msiDbHelper = new MsiDbHelper(msiDbCtx)
//        	val msiSearchProvider = new SQLMsiSearchProvider(executionContext.getUDSDbConnectionContext(),
//			executionContext.getMSIDbConnectionContext(),
//			executionContext.getPSDbConnectionContext())
//        	
//        	//TODO Get ResultSetId for case only RSM specified !
//        	val msiSearch = msiSearchProvider.getMSISearch(msiDbHelper.getResultSetsMsiSearchIds(Array(resultSetId))(0))
//        	if (!msiSearch.isDefined || !msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
//        		logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
//        		throw new RuntimeException("PeptideMatch Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
//        	}
//        	
//        	val ms2ErrorTol = msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTol
//			val ms2ErrorTolUnit = msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
//			val peptideMatchProvider = new SQLPeptideMatchProvider(msiDbCtx, executionContext.getPSDbConnectionContext)
//        	
//			val pepMatchIDsIterator = pepMatchIdsBuilder.result.sliding(1000,1000)
//			while(pepMatchIDsIterator.hasNext){
//			   val pepMatchIds = pepMatchIDsIterator.next
//			   
//			   val peptideMatches = peptideMatchProvider.getPeptideMatches(pepMatchIds)
//			   val spectrumIds = peptideMatches.map(_.getMs2Query.spectrumId)
//			   val spectra = spectrumProvider.getSpectra(spectrumIds)
//			   val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
//			   val psmMatcher = {
//		        msiSearch.get.searchSettings.softwareName.toLowerCase() match {
//		          case "mascot" => new PeptideSpectrumMatcherMascot(spectraById, ms2ErrorTol, ms2ErrorTolUnit)
//		          case "omssa" => new PeptideSpectrumMatcherOmssa(spectraById, ms2ErrorTol, ms2ErrorTolUnit)
//		          case _ => 
//		            logger.error("Spectrum matches cannot be generated for this search engine")
//		            throw new RuntimeException("Spectrum matches cannot be generated for this search engine")
//		        }
//			   }
//			   logger.info("Storing spectrum matches...")
//		          for (peptideMatch <- peptideMatches) {
//		            val spectrumMatch = psmMatcher.getSpectrumMatch(peptideMatch)
//		            SQLRsWriter.insertSpectrumMatch(peptideMatch, spectrumMatch, msiDbCtx)
//		          }
//			} //End go through slinding windows
//            
//        } else { // End if memory Save
//	        
//	        val (resultSet, rsm) = {
//	          if (!resultSummaryId.isDefined) {
//	            (MSDiagDataGenerator._loadResultSet(resultSetId, executionContext), null)
//	          } else {
//	            val rsm = MSDiagDataGenerator._loadResultSummary(resultSummaryId.get, executionContext)
//	            (rsm.resultSet.get, rsm)
//	          }
//	        }
//	        
//	        val msiSearch = resultSet.msiSearch
//	        if (msiSearch.isDefined && msiSearch.get.searchSettings.msmsSearchSettings.isDefined) {
//		      val ms2ErrorTol = msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTol
//		      val ms2ErrorTolUnit = msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
//		      val spectra = {
//	            if (resultSummaryId.isDefined) {
//	              spectrumProvider.getSpectra(MSDiagDataGenerator._getSpectraIds(rsm.resultSet.get))
//	            } else {
//	              spectrumProvider.getSpectra(MSDiagDataGenerator._getSpectraIds(resultSet))
//	            }
//	          }
//	
//	          val spectraById = Map() ++ spectra.map { sp => (sp.id -> sp) }
//	          val psmMatcher = {
//		        msiSearch.get.searchSettings.softwareName.toLowerCase() match {
//		          case "mascot" => new PeptideSpectrumMatcherMascot(spectraById, ms2ErrorTol, ms2ErrorTolUnit)
//		          case "omssa" => new PeptideSpectrumMatcherOmssa(spectraById, ms2ErrorTol, ms2ErrorTolUnit)
//		          case _ => 
//		            logger.error("Spectrum matches cannot be generated for this search engine")
//		            throw new RuntimeException("Spectrum matches cannot be generated for this search engine")
//		        }
//		      }
//	          logger.info("Storing spectrum matches...")
//	          rsStorer.insertSpectrumMatches(resultSet, new ResultSetWrapper(resultSet, psmMatcher), storerContext)
//	        } else {
//	          logger.error("Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
//		      throw new RuntimeException("ResultSet " + resultSet.id + " Peptide-Spectrum Matching cannot be done because searchSettings ms2 error tolerance is undefined")
//	        }
//        }      
//      } // End peptideMatchIds Not Defined
//      
//      // Commit transaction if it was initiated locally
//        if (localMSITransaction) {
//          msiDbCtx.commitTransaction()
//        }
//
//        msiTransacOk = true
//    } finally {

//      if (storerContext != null) {
//        storerContext.clearContext()
//      }
//
//      if (localMSITransaction && !msiTransacOk) {
//        logger.info("Rollbacking MSI Db Transaction")
//
//        try {
//          msiDbCtx.rollbackTransaction()
//        } catch {
//          case ex: Exception => logger.error("Error rollbacking MSI Db Transaction", ex)
//        }
//
//      }

      // Execution context should be closed by the service caller
      executionContext.closeAll()

   // }

   // this.beforeInterruption()
   // logger.debug("End of MSDiag result importer service")


    msiTransacOk
  }

}

//class ResultSetWrapper(resultSet: ResultSet, psmMatcher: PeptideSpectrumMatcher) extends IRsContainer {
//
//  def getResultSet(wantDecoy: Boolean): ResultSet = {
//    resultSet
//  }
//
//  def eachSpectrum(onEachSpectrum: Spectrum => Unit): Unit = {
//
//  }
//
//  def eachSpectrumMatch(wantDecoy: Boolean, onEachSpectrumMatch: SpectrumMatch => Unit): Unit = {
//    for (peptideMatch <- resultSet.peptideMatches) {
//      if (psmMatcher.spectraByIds.contains(peptideMatch.getMs2Query.spectrumId)) {
//        onEachSpectrumMatch(psmMatcher.getSpectrumMatch(peptideMatch))
//      }
//    }
//  }
//
//}
