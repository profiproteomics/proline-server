package fr.proline.cortex.api.service.dps.msq

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc._

object QuantifySCService extends IQuantifySCService

trait IQuantifySCService extends IMsqService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "QuantifySC"
  this.serviceDescription = Some(
    "Computes spectral count for proteins of result summaries associated to experimental design's QuantChannel."+
    "This service return the id of the created dataset (dataset_quanti_id) and the JSON result containing spectral count values (spectral_count_result)."
  )
  
  /* Configure the service interface */
  val serviceParams = List(
    NAME_PARAM,
    DESCRIPTION_PARAM,
    PROJECT_ID_PARAM,
    REF_RSM_ID_PARAM,
    REF_DS_ID_PARAM,
    EXPERIMENTAL_DESIGN_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Map[String,Any]],
    "A Map containging the quant_dataset_id and the spectral_count_result as a JSON string."
  )
  
  object NAME_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "name"
    val description = "Name of the quantitation dataset that will be created for this quantitation."
    val scalaType = typeOf[String]
  }
  object DESCRIPTION_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "description"
    val description = "Description of the quantitation dataset that will be created for this quantitation."
    val scalaType = typeOf[String]
  }
  object PROJECT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "project_id"
    val description = "The id of the project the quantitation will be created in."
    val scalaType = typeOf[Long]
  }
  object REF_RSM_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "ref_rsm_id"
    val description = "The id of the reference result summary used for this spectral counting computation."
    val scalaType = typeOf[Long]
  }
  object REF_DS_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "ref_ds_id"
    val description = "The id of the reference dataset used for this spectral counting computation."
    val scalaType = typeOf[Long]
  }
  object PEPTIDE_REF_RSM_IDS_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "peptide_ref_rsm_ids"
    val description = "List of result summary ID where the spectral count specificity and weight should be calculated."
    val scalaType = typeOf[Array[Long]]
  }
  object EXPERIMENTAL_DESIGN_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "experimental_design"
    val description = "The experimental design related to this quantitation."
    val scalaType = typeOf[Object] // SimplifiedExperimentalDesign
  }
  
}

