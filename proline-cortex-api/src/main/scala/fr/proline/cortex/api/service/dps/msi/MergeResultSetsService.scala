package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

//object MergeResultSetsService extends IMergeResultSetsService

trait IMergeResultSetsService extends IRemoteServiceIdentity {
  
  /* JMS Service identification */
  val serviceNamespace = "proline/dps/msi"
  // TODO: rename to MergeDataSetsService
  val serviceLabel = "MergeResults"
  this.serviceDescription = Some(
    "Merge specified data sets  (result sets or result summaries) into one new data set (new result set or new result summary)." +
    "In case or merge result summaries, only validated data will be taken into account for new result summary and associated result set." +
    "This service will return the merged result set id or the merged result summary id and associated result set id (targetResultSummaryId/targetResultSetId)."
  )
  
  // Description of the merge_result_sets service method
  trait IMergeResultSetsMethod extends JSONRPC2DefaultMethod { // Emulate an enumeration of objects
    
    // Method description
    val name = "merge_result_sets"
    val description =  "Merge specified result sets into one new result set."
    val parameters = List(PROJECT_ID_PARAM,RESULT_SET_IDS_PARAM)
    
    val returns = JSONRPC2MethodResult(
      typeOf[Long],
      description = "The ID of the merged result set."
    )
    
    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The id of the project containing these result sets."
      val scalaType = typeOf[Long]
    }
    object RESULT_SET_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "result_set_ids"
      val description = "A list of ids corresponding to the result sets to merge."
      val scalaType = typeOf[Array[Long]]
    }
    
  }
  
  case class RSMMergeResult(var targetResultSummaryId: Long = -1L, var targetResultSetId: Long = -1L)
  
  // Description of the merge_result_sets service method
  trait IMergeResultSummariesMethod extends JSONRPC2DefaultMethod { // Emulate an enumeration of objects
    
    // Method description
    val name = "merge_result_summaries"
    val description = "Merge specified result summaries into one new result summary. " +
    "Only validated data will be taken into account for new result summary and associated result set."
    
    // Configure method interface
    val parameters = List(PROJECT_ID_PARAM,RESULT_SUMMARY_IDS_PARAM)
    val returns = JSONRPC2MethodResult(
      typeOf[RSMMergeResult],
      description = "The ID of the merged result summary and associated result set ID (targetResultSummaryId/targetResultSetId)."
    )
    
    object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "project_id"
      val description = "The id of the project containing these result summaries."
      val scalaType = typeOf[Long]
    }
    object RESULT_SUMMARY_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "result_summary_ids"
      val description = "A list of ids corresponding to the result summaries to merge."
      val scalaType = typeOf[Array[Long]]
    }
  }
}

object MergeResultSetsServiceV1_0 extends IMergeResultSetsServiceV1_0

trait IMergeResultSetsServiceV1_0 extends IMergeResultSetsService with IDefaultServiceVersion {
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(MERGE_RESULT_SETS_METHOD,MERGE_RESULT_SUMMARIES_METHOD)
  
  object MERGE_RESULT_SETS_METHOD extends IMergeResultSetsMethod
  object MERGE_RESULT_SUMMARIES_METHOD extends IMergeResultSummariesMethod
}

object MergeResultSetsServiceV2_0 extends IMergeResultSetsServiceV2_0

trait IMergeResultSetsServiceV2_0 extends IMergeResultSetsService {
  
  val serviceVersion = "2.0"
  
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(MERGE_RESULT_SETS_METHOD,MERGE_RESULT_SUMMARIES_METHOD)
  
  trait HasAggregationModeParam {
    object AGGREGATION_MODE_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "aggregation_mode" // TODO: rename to addition_mode ?
      val description = "The mode of PSM aggregation to use (union or aggregation)."
      val scalaType = typeOf[String]
      optional = true
    }
  }
  
  // Description of the merge_result_sets service method
  object MERGE_RESULT_SETS_METHOD extends IMergeResultSetsMethod with HasAggregationModeParam {
    override val parameters = List(PROJECT_ID_PARAM, RESULT_SET_IDS_PARAM, AGGREGATION_MODE_PARAM)
  }
  
  // Description of the merge_result_sets service method
  object MERGE_RESULT_SUMMARIES_METHOD extends IMergeResultSummariesMethod with HasAggregationModeParam {
    override val parameters = List(PROJECT_ID_PARAM, RESULT_SUMMARY_IDS_PARAM, AGGREGATION_MODE_PARAM)
  }
  
  
}

