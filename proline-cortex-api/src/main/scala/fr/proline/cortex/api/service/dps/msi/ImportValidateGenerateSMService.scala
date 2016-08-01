package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.typeOf

import fr.proline.jms.service.api.IDefaultServiceVersion
import fr.proline.jms.service.api.RemoteServiceIdentity
import fr.profi.util.jsonrpc.IJSONRPC2Method
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethod
import fr.profi.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.profi.util.jsonrpc.JSONRPC2MethodResult

object ImportValidateGenerateSMService extends IImportValidateGenerateSMService

trait IImportValidateGenerateSMService extends IImportResultFilesServiceParams with IValidateResultSetServiceParams with IGenerateSpectrumMatchesServiceParams with IMsiService with IDefaultServiceVersion {

  /* JMS Service identification */
  val serviceLabel = "ImportValidateGenerateSM"
  this.serviceDescription = Some(
    "Import a result file in the MSIdb corresponding to the provided project id. " +
    "Validate the imported result and optionnaly generate Spectrum Matches for the validated PSMs."
  )

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = "Import a result file, validate data and optionnaly generate Spectrum Matches for the validated PSMs." 

    val returns = JSONRPC2MethodResult(
      // TODO: create a case class for these parameters
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
}



