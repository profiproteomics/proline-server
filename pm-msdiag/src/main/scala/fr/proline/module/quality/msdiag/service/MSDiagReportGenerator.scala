package fr.proline.module.quality.msdiag.service

import scala.Array.canBuildFrom
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.util.serialization.ProfiJson
import fr.proline.api.service.IService
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.storer.msi.RsStorer
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.module.quality.msdiag.MSDiag

class MSDiagReportGenerator(
  executionContext: IExecutionContext,
  resultSetId: Long,
  resultSummaryId: Option[Long] = None,
  peptideMatchIds: Option[Array[Long]] = None/*,
  memorySave : Boolean = false */) extends IService with Logging {

   var resultHashMapJson : String = ""
  
  def runService(): Boolean = {
    logger.info("Run service MSDiagDataGenerator on ResultSet.id=" + resultSetId)

    val msiDbCtx = executionContext.getMSIDbConnectionContext
    var storerContext: StorerContext = null
    val rsStorer = RsStorer(msiDbCtx)
    var localMSITransaction: Boolean = false
    var msiTransacOk: Boolean = false

    val parserContext = ProviderDecoratedExecutionContext(executionContext)
    val id = resultSetId
    val msdiag = new MSDiag(id, parserContext)
    
    // here serialize (and json) the resulting object to be sent to prolinestudio...
    val reports = msdiag.getAvailableReports
    logger.info("MSDiag returned " + reports.size + " reports")
    resultHashMapJson = ProfiJson.serialize(reports.map(msd => msd.description -> ProfiJson.serialize(msd)).toMap)
    logger.info("Json serialization takes " + resultHashMapJson.length() + " bytes")   

    // Execution context should be closed by the service caller
    executionContext.closeAll()

    msiTransacOk
  }

}

