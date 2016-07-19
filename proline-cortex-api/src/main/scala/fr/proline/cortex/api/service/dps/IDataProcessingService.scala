package fr.proline.cortex.api.service.dps

import scala.reflect.runtime.universe.typeOf

import com.thetransactioncompany.jsonrpc2.{JSONRPC2Error, JSONRPC2Request, JSONRPC2Response}
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.cortex.api.JSONRPC2Service
import fr.proline.cortex.util.json.JSONType
import fr.proline.cortex.util.jsonrpc._
import fr.proline.cortex.util.reflect._

trait IDataProcessingService extends JSONRPC2Service with Logging {

  /* Constants */  
  //val RUN_JOB_METHOD = "run_job"
  //val GET_JOB_STATUS_METHOD = "get_job_status"
  //val CANCEL_JOB_METHOD = "cancel_job"
  //val JOB_ID_KEY = "job_id"
  //val INVALID_PARAMS_ERROR_CODE = JSONRPC2Error.INVALID_PARAMS.getCode()

  val serviceParams: Seq[IJSONRPC2MethodParameter]
  // TODO: require
  var serviceResultType: reflect.runtime.universe.Type = scala.reflect.runtime.universe.typeOf[Object]

  // Specify the method definitions of the handled requests
  lazy val handledMethods = List(
    JSONRPC2Method(
      name = "process",
      description = "Runs a Job using a specified Map of parameters",
      parameters = serviceParams.map(_.toSerializableJSONRPC2Parameter()),
      returns = JSONRPC2MethodResult(typeOf[Int])
    )
    // TODO: re-enable other methods when we can hav
    /*JSONRPC2Method(
      name = RUN_JOB_METHOD,
      description = "Runs a Job using a specified Map of parameters",
      parameters = wsParams.map(_.toSerializableJSONRPC2Parameter()),
      returns = JSONRPC2MethodResult(typeOf[Int])
    ),
    JSONRPC2Method(
      name = GET_JOB_STATUS_METHOD,
      description = "Returns the Job Status of the specified job",
      parameters = Array(JSONRPC2MethodParameter(JOB_ID_KEY, null, typeOf[Int])),
      returns = JSONRPC2MethodResult(
        typeOf[JobStatus],
        description = s"Scala type of result is '${wsResultType.typeSymbol.name }'"
      )
    ),
    JSONRPC2Method(
      name = CANCEL_JOB_METHOD,
      description = "Tries to cancel the specified job and returns the success of the cancellation.",
      parameters = Array(JSONRPC2MethodParameter(JOB_ID_KEY, null, typeOf[Int])),
      returns = JSONRPC2MethodResult( typeOf[Boolean] )
    )*/
  )
  
  /*private def _newUnknownJobIdError( jobId: Int ) = {
    new JSONRPC2Error(INVALID_PARAMS_ERROR_CODE, "Unknown Job Id " + jobId )
  }*/

}

