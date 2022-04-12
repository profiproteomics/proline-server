package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult


trait IImportValidateGenerateSMService extends IImportResultFilesServiceParams with IValidateResultSetServiceParams with IGenerateSpectrumMatchesServiceParams with IMsiService  {
  /* JMS Service identification */
  val serviceLabel = "ImportValidateGenerateSM"
  this.serviceDescription = Some(
    "Import a result file in the MSIdb corresponding to the provided project id. " +
      "Validate the imported result and optionnaly generate Spectrum Matches for the validated PSMs."
  )

  /* Import Specific parameters */
  object USE_DECOY_REGEX_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "use_decoy_regexp"
    val description = "true if result file is formated with decoy strategy RegExp, false if it is formated with the id of the rule to be used."
    val scalaType = typeOf[Boolean]
  }

  /* Generate Spectrum Matches Specific parameters */
  object GENERATE_SPECTRUM_MATCHES_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "generate_spectrum_matches"
    val description = " If true, generate fragment matches of MS/MS spectra for validated PSMs."
    val scalaType = typeOf[Boolean]
  }
}

object ImportValidateGenerateSMServiceV1 extends IImportValidateGenerateSMServiceV1

trait IImportValidateGenerateSMServiceV1 extends IImportValidateGenerateSMService  with IDefaultServiceVersion {

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Import a result file, validate data and optionnaly generate Spectrum Matches for the validated PSMs." 

    val returns = JSONRPC2MethodResult(
      typeOf[Array[ImportedResultFile]],
      "List of ImportedResultFile: path of imported file and id of created target RS."
    )

    /* Configure the service interface */
    override val parameters = List(
      IMPORT_PROJECT_ID_PARAM,
      USE_DECOY_REGEX_PARAM,
      RESULT_FILES_PARAM,
      INSTRUMENT_CONFIG_ID_PARAM,
      PEAKLIST_SOFTWARE_ID_PARAM,
      IMPORTER_PROPERTIES_PARAM,
      PEP_MATCH_FILTERS_PARAM,
      PEP_MATCH_VALIDATOR_CONFIG_PARAM,
      PEP_SET_SCORE_TYPE_PARAM,
      PROT_SET_FILTERS_PARAM,
      PROT_SET_VALIDATOR_CONFIG_PARAM,
      GENERATE_SPECTRUM_MATCHES_PARAM,
      FORCE_INSERT_PARAM
    )

  }

}

object ImportValidateGenerateSMServiceV2 extends IImportValidateGenerateSMServiceV2

trait IImportValidateGenerateSMServiceV2 extends IImportValidateGenerateSMService {

  val serviceVersion = "2.0"
  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Import a result file, validate data and optionnaly generate Spectrum Matches for the validated PSMs."

    val returns = JSONRPC2MethodResult(
      typeOf[Array[ImportedResultFile]],
      "List of ImportedResultFile: path of imported file and id of created target RS."
    )

    /* Configure the service interface */
    override val parameters = List(
      IMPORT_PROJECT_ID_PARAM,
      USE_DECOY_REGEX_PARAM,
      RESULT_FILES_PARAM,
      INSTRUMENT_CONFIG_ID_PARAM,
      PEAKLIST_SOFTWARE_ID_PARAM,
      IMPORTER_PROPERTIES_PARAM,
      PEP_MATCH_FILTERS_PARAM,
      PEP_MATCH_VALIDATOR_CONFIG_PARAM,
      PEP_SET_SCORE_TYPE_PARAM,
      PROT_SET_FILTERS_PARAM,
      PROT_SET_VALIDATOR_CONFIG_PARAM,
      GENERATE_SPECTRUM_MATCHES_PARAM,
      FORCE_INSERT_PARAM
    )

    object VALIDATE_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "validate_result_set"
      val description = "If true, imported resultsets will be validated with specified param."
      val scalaType = typeOf[Boolean]
    }
  }

  }



