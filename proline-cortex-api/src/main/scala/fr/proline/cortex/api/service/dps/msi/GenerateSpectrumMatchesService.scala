package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

object GenerateSpectrumMatchesService extends IGenerateSpectrumMatchesService

trait IGenerateSpectrumMatchesServiceParams {
  object FORCE_INSERT_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "force_insert"
    val description = "Specify if existing spectrum matches should be replaced."
    val scalaType = typeOf[Boolean]
    optional = true
  }
}

trait IGenerateSpectrumMatchesService extends IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "GenerateSpectrumMatches"
  this.serviceDescription = Some("Generates and store the spectrum matches.")

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IGenerateSpectrumMatchesServiceParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get
    
    // Configure method interface
    val parameters = List(
      PROJECT_ID_PARAM,
      RESULT_SET_ID_PARAM,
      RESULT_SUMMARY_ID_PARAM,
      PEPTIDE_MATCH_IDS_PARAM,
      FORCE_INSERT_PARAM
    )
    val returns = JSONRPC2MethodResult(
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
  }
}

