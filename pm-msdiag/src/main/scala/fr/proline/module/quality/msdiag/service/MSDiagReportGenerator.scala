package fr.proline.module.quality.msdiag.service

import scala.Array.canBuildFrom
import fr.profi.util.serialization.ProfiJson
import fr.profi.api.service.IService
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.storer.msi.RsStorer
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.module.quality.msdiag.MSDiag
import com.typesafe.scalalogging.LazyLogging

class MSDiagReportGenerator(
  executionContext: IExecutionContext,
  resultSetId: Long,
  //settings: Option[java.util.Map[String,Object]]
  settings: Option[Map[String,Any]] // msdiag settings 
  ) extends IService with LazyLogging {

   var resultHashMapJson : String = ""
  
  def runService(): Boolean = {
    logger.info("Run service MSDiagDataGenerator on ResultSet.id=" + resultSetId)

    val msiDbCtx = executionContext.getMSIDbConnectionContext
    var storerContext: StorerContext = null
    val rsStorer = RsStorer(msiDbCtx, useJPA = false)
    var localMSITransaction: Boolean = false
    var msiTransacOk: Boolean = false

    val parserContext = ProviderDecoratedExecutionContext(executionContext)
    val id = resultSetId
    val msdiag = new MSDiag(id, parserContext)
    // apply settings to MSDiag object
    if(settings.isDefined) {
      logger.info("MSDiag received settings " + settings.get.toString())
      msdiag.setSettings(settings.get);
    } 
    else
    {       
      logger.info("MSDiag received settings EMPTY")
    }
    
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

