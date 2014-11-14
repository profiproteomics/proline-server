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

    
   

      // Execution context should be closed by the service caller
      executionContext.closeAll()

 
    msiTransacOk
  }

}

