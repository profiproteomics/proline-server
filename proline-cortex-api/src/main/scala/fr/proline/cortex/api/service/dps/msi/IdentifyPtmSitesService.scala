package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

trait IIdentifyPtmSitesService extends IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "IdentifyPtmSites"
  
  this.serviceDescription = Some(
    "Identify PTM sites located on validated proteins of a specified Result Summary " +
    "The extracted information is stored in the database in an object tree linked to the Result Summary."
  )

  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IIdentifyPtmSitesParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get
    
    val returns = JSONRPC2MethodResult(
       typeOf[Boolean],
      "True if the Ptm site service succeed."
    )

    // Configure method interface
    val parameters = List(PROJECT_ID_PARAM, RESULT_SUMMARY_ID_PARAM, FORCE_PARAM)
    
  }

}

trait IIdentifyPtmSitesServiceV2 extends IMsiService {

  /* JMS Service identification */
  val serviceLabel = "IdentifyPtmSites"
  val serviceVersion= "2.0"

  this.serviceDescription = Some(
    "Identify PTM sites located on validated proteins of a specified Result Summary " +
      "The extracted information is stored in the database in an object tree linked to the Result Summary."
  )

  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod with IIdentifyPtmSitesParams {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get

    val returns = JSONRPC2MethodResult(
      typeOf[Boolean],
      "True if the Ptm site service succeed."
    )

    // Configure method interface
    val parameters = List(PROJECT_ID_PARAM, RESULT_SUMMARY_ID_PARAM, PTM_IDS_PARAM, CLUSTERING_CONFIG_PARAM, FORCE_PARAM)

  }

}

trait IIdentifyPtmSitesParams {

  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The ID of the project the result summary belongs to."
    val scalaType = typeOf[Long]
  }

  object RESULT_SUMMARY_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "result_summary_id"
    val description = "The id of the result summary to process."
    val scalaType = typeOf[Long]
  }

  object FORCE_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "force"
    val description = "True to force re-identification of ptm sites."
    val scalaType = typeOf[Boolean]
    optional = true
  }

  object PTM_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "ptm_ids"
    val description = "A list of ids corresponding to the ptms of interest."
    val scalaType = typeOf[Array[Long]]
    optional = true
  }

  object CLUSTERING_CONFIG_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "clustering_config"
    val description = "The parameters used to create clusters of identified PTMs (see documentation)."
    val scalaType = typeOf[Map[String, Any]]
    optional = true
  }

}

