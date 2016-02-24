package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.service.msi.OrphanDataDeleter
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessService

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
class DeleteOrphanData extends AbstractRemoteProcessService with LazyLogging  {
  /* JMS Service identification */
  val serviceName = "proline/dps/msi/DeleteOrphanData"
  val serviceVersion = "1.0"
  override val defaultVersion = true
  
  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require((paramsRetriever != null), "No Parameters specified")
    
    var deleteResult: java.lang.Boolean = false
    
    val projectId = paramsRetriever.getLong("project_id")
    val resultSummaryIds = paramsRetriever.getList("result_summary_ids").toArray.map { rf => deserialize[Long](serialize(rf)) }
    val resultSetIds = paramsRetriever.getList("result_set_ids").toArray.map { rf => deserialize[Long](serialize(rf)) }
    
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)  // Use JPA context
    var msiDbConnectionContext: DatabaseConnectionContext = null;
    
    try {
      // Begin transaction
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext;
      msiDbConnectionContext.beginTransaction();
      
      val orphanDataDeleter = new OrphanDataDeleter(execCtx, projectId, resultSummaryIds, resultSetIds);
      orphanDataDeleter.runService()
      
      //Commit transaction
      msiDbConnectionContext.commitTransaction();
    }catch {
        case ex: Exception => {
          deleteResult = false;
          msiDbConnectionContext.rollbackTransaction();
          logger.error("Error while deleting orphan data in MSI", ex);
          val msg = if (ex.getCause() != null) { "Error while deleting orphan data in MSI " + ex.getCause().getMessage() } else { "Error  while deleting orphan data in MSI " + ex.getMessage() };
          throw new Exception(msg)
        }
      } finally {
        try {
          execCtx.closeAll();
        } catch {
          case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
        }
      }

    deleteResult
  }
}