package fr.proline.cortex.service.dps.msi

import scala.Array.canBuildFrom
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.service.msi.ResultSetMerger
import fr.proline.core.service.msi.ResultSummaryMerger
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.IRemoteService
import fr.proline.jms.util.jsonrpc.JSONRPC2Utils
import fr.proline.jms.util.jsonrpc.ProfiJSONRPC2Response

/**
 * Merge specified result sets (or result summaries) into one new result set (or new result summary).
 * In case or merge result summaries,  only validated data will be taken into account for new result summary and associated result set.
 * This service will return the merged result set Id or the merged result Summary Id and associated result set id (targetResultSummaryId/targetResultSetId).
 *
 * Input params
 *  project_id : the id of the project to which data is linked.
 *     result_set_ids : The list of the result set to merged.
 *  OR
 *  result_summary_ids : The list of the result summaries to merged
 *
 *  Output param
 *    for merge RS :  the merged result set Id
 *    for merge RSM : the merged result Summary Id and associated result set id (as RSMMergeResult object)
 */
class MergeResultSets extends IRemoteService with LazyLogging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/MergeResults"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {

    require((req != null), "Req is null")

    val requestId = req.getID
    val methodName = req.getMethod

    /* Method dispatch */
    methodName match {

      case "merge_result_sets" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)
        val result = doMergeResultSetProcess(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }

      case "merge_result_summaries" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)
        val result = doMergeResultSummariesProcess(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }

      // Method name not supported
      case _ => return new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
    }

    new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
  }

  case class RSMMergeResult(var targetResultSummaryId: Long = -1L, var targetResultSetId: Long = -1L)

  /* Define the doMergeResultSummariesProcess method */
  def doMergeResultSummariesProcess(paramsRetriever: NamedParamsRetriever): Object = {
    
    require((paramsRetriever != null), "no parameter specified")
    
    val projectId = paramsRetriever.getLong("project_id")
    val resultSummaryIds = paramsRetriever.getList("result_summary_ids").toArray.map { rf => deserialize[Long](serialize(rf)) }

    var result: RSMMergeResult = new RSMMergeResult()
    var msiDbConnectionContext: DatabaseConnectionContext = null
    val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), projectId, false)

    try {
      logger.info("ResultSummary merger service will start")

      val rsmMerger = new ResultSummaryMerger(
        execCtx = execCtx,
        resultSummaryIds = Some(resultSummaryIds),
        resultSummaries = None
      )
      rsmMerger.run()

      logger.info("ResultSet merger done")

      result.targetResultSummaryId = rsmMerger.mergedResultSummary.id
      result.targetResultSetId = rsmMerger.mergedResultSummary.getResultSetId

    } finally {
      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    result
  }

  /* Define the doMergeResultSetProcess method */
  def doMergeResultSetProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "no parameter specified")
    
    val projectId = paramsRetriever.getLong("project_id")
    val resultSetIds = paramsRetriever.getList("result_set_ids").toArray.map { rf => deserialize[Long](serialize(rf)) }

    var result: java.lang.Long = -1L
    var msiDbConnectionContext: DatabaseConnectionContext = null
    val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), projectId, false)

    try {
      logger.info("ResultSet merger service will start")

      val rsMerger = new ResultSetMerger(
        execCtx = execCtx,
        resultSetIds = Some(resultSetIds),
        resultSets = None
      )

      rsMerger.run

      logger.info("ResultSet merger done")

      result = rsMerger.mergedResultSet.id

    } finally {
      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    result
  }

}