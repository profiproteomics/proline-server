package fr.proline.module.exporter.pridexml

import com.typesafe.scalalogging.LazyLogging
import fr.proline.api.service.IService
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.msi.IResultSummaryProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider


class PrideExporterService (
  execCtx: IExecutionContext,
  resultSummaryId: Long,
  filePath: String ,
  extraDataMap: Map[String, Object]) extends IService with LazyLogging {

  def runService(): Boolean = {

    require(!execCtx.isJPA, "SQL connextion should be provided")
    val udsSQLCtx = execCtx.getUDSDbConnectionContext
    val msiSQLCtx = execCtx.getMSIDbConnectionContext

    val rsmProvider = getResultSummaryProvider(execCtx)
    val rsm = rsmProvider.getResultSummary(resultSummaryId, loadResultSet = true).get

    val msiDbHelper = new MsiDbHelper(msiSQLCtx)
    val unimodIdByPtmId = msiDbHelper.getUnimodIdByPtmId()



    //VDS : Not used !
    // TODO: use peaklist_relation instead ?
//    val msiIds = msiDbHelper.getResultSetsMsiSearchIds(Array(rsm.getResultSetId))
    //    val pklIds = DoJDBCReturningWork.withEzDBC(msiSQLCtx, { msiEzDBC =>
//      msiEzDBC.selectLongs("SELECT peaklist_id FROM msi_search WHERE id IN (" + msiIds.mkString(",") + ")")
//    })
    
    val exporter = new PrideExporter(rsm, unimodIdByPtmId, execCtx)
    exporter.exportResultSummary(filePath, extraDataMap)

    true
  }
  
    // TODO Retrieve a ResultSetProvider from a decorated ExecutionContext ?
  private def getResultSummaryProvider(execContext: IExecutionContext): IResultSummaryProvider = {
		  
    new SQLResultSummaryProvider(PeptideCacheExecutionContext(execContext))
  }

}