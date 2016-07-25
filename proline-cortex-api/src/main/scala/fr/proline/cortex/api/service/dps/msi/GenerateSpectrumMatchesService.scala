package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object GenerateSpectrumMatchesService extends IGenerateSpectrumMatchesService

trait IGenerateSpectrumMatchesService extends IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "GenerateSpectrumMatches"
  this.serviceDescription = Some(
    "Generates and store the spectrum matches."
  )
  
  /* Configure the service interface */
  val serviceParams = List(
    PROJECT_ID_PARAM,
    RESULT_SET_ID_PARAM,
    RESULT_SUMMARY_ID_PARAM,
    PEPTIDE_MATCH_IDS_PARAM,
    FORCE_INSERT_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Boolean],
    "True if the service ran successfully, false otherwise."
  )
  
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project."
    val scalaType = typeOf[Long]
  }
  object RESULT_SET_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_set_id"
    val description = "The id of the Result Set containing the peptide matches and spectrum."
    val scalaType = typeOf[Long]
  }
  object RESULT_SUMMARY_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_summary_id"
    val description = "The id of the Result Summary containing the set of peptide matches for which spectrum matches will be generated."
    val scalaType = typeOf[Long]
    optional = true
  }
  object PEPTIDE_MATCH_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "peptide_match_ids"
    val description = "The array of peptide match ids to consider."
    val scalaType = typeOf[Array[Long]]
    optional = true
  }
  object FORCE_INSERT_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "force_insert"
    val description = "Specify if existing spectrum matches should be replaced."
    val scalaType = typeOf[Boolean]
    optional = true
  }
  
}

