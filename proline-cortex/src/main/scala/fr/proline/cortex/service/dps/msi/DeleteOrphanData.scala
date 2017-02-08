package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.service.msi.OrphanDataDeleter
import fr.proline.cortex.api.service.dps.msi.IDeleteOrphanDataService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService


/**
 * Define JMS Service to remove orphan data into the MSI database: rsm and rs data
 * 
 *  Input params :
 *  project_id : The id of the project the rsm/rs will be searched in
 *  result_set_ids : The list of the result set to be removed
 *  result_summary_ids : The list of the result summary  to be removed
 *
 * Output params :
 *  Boolean for service run status
 *  
 * @author MB243701
 */
class DeleteOrphanData extends AbstractRemoteProcessingService with IDeleteOrphanDataService with LazyLogging  {
  
  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    require(paramsRetriever != null, "No parameters specified")
    
    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSummaryIds = paramsRetriever.getList(PROCESS_METHOD.RESULT_SUMMARY_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val resultSetIds = paramsRetriever.getList(PROCESS_METHOD.RESULT_SET_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)  // Use JPA context
    var msiDbConnectionContext: DatabaseConnectionContext = null
    
    try {
      // Begin transaction
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction()
      
      val orphanDataDeleter = new OrphanDataDeleter(execCtx, projectId, resultSummaryIds, resultSetIds)
      orphanDataDeleter.runService()
      
      //Commit transaction
      msiDbConnectionContext.commitTransaction()
    } catch {
      case t: Throwable=> {
        DbConnectionHelper.tryToRollbackDbTransaction(msiDbConnectionContext)
        
        throw ExceptionUtils.wrapThrowable("Error while deleting orphan data in MSI", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}