package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.jsonrpc.BuildJSONRPC2Response
import fr.profi.util.jsonrpc.JSONRPC2Utils
import fr.profi.util.jsonrpc.ProfiJSONRPC2Response
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.algo.msi.AdditionMode
import fr.proline.core.service.msi.ResultSetMerger
import fr.proline.core.service.msi.ResultSummaryMerger
import fr.proline.cortex.api.service.dps.msi.IMergeResultSetsService
import fr.proline.cortex.api.service.dps.msi.IMergeResultSetsServiceV1_0
import fr.proline.cortex.api.service.dps.msi.IMergeResultSetsServiceV2_0
import fr.proline.cortex.api.service.dps.msi.MergeResultSetsServiceV1_0
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.IRemoteJsonRPC2Service

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

abstract class AbstractMergeDatases extends IMergeResultSetsService with IRemoteJsonRPC2Service with LazyLogging {
  
  def doMergeResultSetProcess(paramsRetriever: NamedParamsRetriever): Object
  def doMergeResultSummariesProcess(paramsRetriever: NamedParamsRetriever): Object
  
  override  def runService(jsonRequest: JSONRPC2Request, jmsMessageContext: Map[String, Any]): JSONRPC2Response = {
    require((jsonRequest != null), "Req is null")
    
    val requestId = jsonRequest.getID()
    val methodName = jsonRequest.getMethod()

    /* Method dispatch */
    methodName match {

      case MergeResultSetsServiceV1_0.MERGE_RESULT_SETS_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doMergeResultSetProcess(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }

      case MergeResultSetsServiceV1_0.MERGE_RESULT_SUMMARIES_METHOD.name => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(jsonRequest)
        val result = doMergeResultSummariesProcess(paramsRetriever) // Call service
        return new ProfiJSONRPC2Response(result, requestId)
      }

      // Method name not supported
      case _ => return BuildJSONRPC2Response.forMethodNotFound(requestId)
    }

    return BuildJSONRPC2Response.forMethodNotFound(requestId)
  }
}

// TODO: rename this file and the service to MergeDatasets
class MergeDatasetsV1_0 extends AbstractMergeDatases with IMergeResultSetsServiceV1_0 {

  def doMergeResultSetProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(MERGE_RESULT_SETS_METHOD.PROJECT_ID_PARAM)
    val resultSetIds = paramsRetriever.getList(MERGE_RESULT_SETS_METHOD.RESULT_SET_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }

    var result: java.lang.Long = -1L
    var msiDbConnectionContext: DatabaseConnectionContext = null
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      logger.info("ResultSet merger service will start")

      val rsMerger = new ResultSetMerger(
        execCtx = execCtx,
        resultSetIds = Some(resultSetIds),
        resultSets = None,
        aggregationMode = None
      )

      rsMerger.run

      logger.info("ResultSet merger done")

      result = rsMerger.mergedResultSet.id

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }

  def doMergeResultSummariesProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(MERGE_RESULT_SUMMARIES_METHOD.PROJECT_ID_PARAM)
    val resultSummaryIds = paramsRetriever.getList(MERGE_RESULT_SUMMARIES_METHOD.RESULT_SUMMARY_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }

    var result: RSMMergeResult = new RSMMergeResult()
    var msiDbConnectionContext: DatabaseConnectionContext = null
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      logger.info("ResultSummary merger service will start")

      val rsmMerger = new ResultSummaryMerger(
        execCtx = execCtx,
        resultSummaryIds = Some(resultSummaryIds),
        resultSummaries = None,
        aggregationMode = None
      )
      rsmMerger.run()

      logger.info("ResultSummary merger done")

      result.targetResultSummaryId = rsmMerger.mergedResultSummary.id
      result.targetResultSetId = rsmMerger.mergedResultSummary.getResultSetId

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }
  

}

class MergeDatasetsV2_0 extends AbstractMergeDatases with IMergeResultSetsServiceV2_0 {

  /* Define the MergeResultSets method */
  def doMergeResultSetProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(MERGE_RESULT_SETS_METHOD.PROJECT_ID_PARAM)
    val resultSetIds = paramsRetriever.getList(MERGE_RESULT_SETS_METHOD.RESULT_SET_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val aggregationMode = Option(paramsRetriever.getOptString(MERGE_RESULT_SETS_METHOD.AGGREGATION_MODE_PARAM, true, null)).map(AdditionMode.withName(_))

    var result: java.lang.Long = -1L
    var msiDbConnectionContext: DatabaseConnectionContext = null
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      logger.info("ResultSet merger service will start")

      val rsMerger = new ResultSetMerger(
        execCtx = execCtx,
        resultSetIds = Some(resultSetIds),
        resultSets = None,
        aggregationMode = aggregationMode
      )

      rsMerger.run

      logger.info("ResultSet merger done")

      result = rsMerger.mergedResultSet.id

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }

  def doMergeResultSummariesProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(MERGE_RESULT_SUMMARIES_METHOD.PROJECT_ID_PARAM)
    val resultSummaryIds = paramsRetriever.getList(MERGE_RESULT_SUMMARIES_METHOD.RESULT_SUMMARY_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val aggregationMode = Option(paramsRetriever.getOptString(MERGE_RESULT_SUMMARIES_METHOD.AGGREGATION_MODE_PARAM, true, null)).map(AdditionMode.withName(_))

    var result: RSMMergeResult = new RSMMergeResult()
    var msiDbConnectionContext: DatabaseConnectionContext = null
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      logger.info("ResultSummary merger service will start")

      val rsmMerger = new ResultSummaryMerger(
        execCtx = execCtx,
        resultSummaryIds = Some(resultSummaryIds),
        resultSummaries = None,
        aggregationMode = aggregationMode
      )
      rsmMerger.run()

      logger.info("ResultSummary merger done")

      result.targetResultSummaryId = rsmMerger.mergedResultSummary.id
      result.targetResultSetId = rsmMerger.mergedResultSummary.getResultSetId

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }
  

}