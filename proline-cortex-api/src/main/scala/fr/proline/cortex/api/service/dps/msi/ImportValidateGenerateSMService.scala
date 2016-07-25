package fr.proline.cortex.api.service.dps.msi

import scala.reflect.runtime.universe.typeOf
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import fr.proline.cortex.util.jsonrpc._
import fr.proline.cortex.util.reflect.FieldDescription
import fr.proline.jms.service.api._

object ImportValidateGenerateSMService extends IImportValidateGenerateSMService

trait IImportValidateGenerateSMService extends IImportResultFilesServiceParams with IValidateResultSetServiceParams with IMsiService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "ImportValidateGenerateSM"
  this.serviceDescription = Some(
    "Import a result file in the MSIdb corresponding to the provided project id. "+
    "Validate the imported result and optionnaly generate Spectrum Matches for the validated PSMs."
  )
  val serviceResult = JSONRPC2MethodResult(
    // TODO: create a case class for these parameters
    typeOf[Array[ImportResultFilesService.ImportedResultFile]],
    "List of ImportedResultFile: path of imported file and id of created target RS."
  )
  
  /* Configure the service interface */
  override val serviceParams = List(
    PROJECT_ID_PARAM,
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
  
  val PROJECT_ID_PARAM = ImportResultFilesService.PROJECT_ID_PARAM
  
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
  val FORCE_INSERT_PARAM = GenerateSpectrumMatchesService.FORCE_INSERT_PARAM
  
}



