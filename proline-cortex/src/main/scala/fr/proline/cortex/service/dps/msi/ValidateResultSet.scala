package fr.proline.cortex.service.dps.msi

import scala.Array.canBuildFrom

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.algo.msi.InferenceMethod
import fr.proline.core.algo.msi.filtering.IPeptideMatchFilter
import fr.proline.core.algo.msi.filtering.IPeptideMatchSorter
import fr.proline.core.algo.msi.filtering.IProteinSetFilter
import fr.proline.core.algo.msi.filtering.pepmatch.ScorePSMFilter
import fr.proline.core.algo.msi.scoring.PepSetScoring
import fr.proline.core.algo.msi.validation.BasicTDAnalyzer
import fr.proline.core.algo.msi.validation.BuildOptimizablePeptideMatchFilter
import fr.proline.core.algo.msi.validation.BuildPeptideMatchFilter
import fr.proline.core.algo.msi.validation.BuildPeptideMatchValidator
import fr.proline.core.algo.msi.validation.BuildProteinSetFilter
import fr.proline.core.algo.msi.validation.BuildProteinSetValidator
import fr.proline.core.algo.msi.validation.IPeptideMatchValidator
import fr.proline.core.algo.msi.validation.IProteinSetValidator
import fr.proline.core.algo.msi.validation.ITargetDecoyAnalyzer
import fr.proline.core.algo.msi.validation.ProtSetValidationMethods
import fr.proline.core.algo.msi.validation.TargetDecoyModes
import fr.proline.core.service.msi.ResultSetValidator
import fr.proline.core.service.msi.ValidationConfig
import fr.proline.cortex.api.service.dps.msi.FilterConfig
import fr.proline.cortex.api.service.dps.msi.IValidateResultSetService
import fr.proline.cortex.api.service.dps.msi.PepMatchValidatorConfig
import fr.proline.cortex.api.service.dps.msi.ProtSetValidatorConfig
import fr.proline.cortex.api.service.dps.msi.ValidateResultSetService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  Define JMS Service which validates ResultSet and creates appropriates ResultSummaries.
 *  Specified PSMs, Proteins filters and validations are applied.
 *
 * Input params :
 *  project_id : The id of the project result set to validate belongs to.
 *  result_set_id : The ResultSet id to validate.
 *  // description : Description of the generated ResultSummary // NOT USED
 *  // use_td_competition : specify if TargetDecoy Competition Based should be used // DEPRECATED
 *  pep_match_filters : List of PSM filters to use (parameter, threshold and post_validation)
 *  pep_match_validator_config : PSM validation configuration (as PepMatchValidatorConfig : parameter, threshold, expectedFdr)
 *  pep_set_score_type : PeptideSet Scoring to use, one of PepSetScoring (mascot:standard score, mascot:modified mudpit score)
 *  prot_set_filters : List of ProteinSet filters to use (parameter, threshold)
 *  prot_set_validator_config : ProteinSet validation configuration  (as ProtSetValidatorConfig : validation_method, parameter, thresholds, expectedFdr)
 *
 * Output params :
 *   generated ResultSummary ID
 */
object ValidateResultSet {
  
  import ValidateResultSetService.PROCESS_METHOD._
  
  def parseValidationConfig(paramsRetriever: NamedParamsRetriever): ValidationConfig = {

    // val pepMatchFilters =  .parsePepMatchFilters(params,targetRS )
    val pepMatchFilters = parsePepMatchFilters(paramsRetriever)

    // Create a default TD analyzer 
    val tdAnalyzer: Option[ITargetDecoyAnalyzer] = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED))
    val pepMatchValidator = parsePepMatchValidator(paramsRetriever, tdAnalyzer)
    val pepSetScoring = Option(paramsRetriever.getOptString(PEP_SET_SCORE_TYPE_PARAM, true, null)).map(PepSetScoring.withName(_))
    val protSetFilters = parseProtSetFilters(paramsRetriever)
    val protSetValidator = parseProtSetValidator(paramsRetriever)

    ValidationConfig(
      tdAnalyzer = None,
      pepMatchPreFilters = pepMatchFilters,
      pepMatchValidator = pepMatchValidator,
      pepSetScoring = pepSetScoring,
      protSetFilters = protSetFilters,
      protSetValidator = protSetValidator
    )
  }

  def parseFilterConfig(paramsMap: Object): FilterConfig = {
    val configAsMap = deserialize[Map[String, AnyRef]](serialize(paramsMap))
    if (configAsMap.contains(ValidateResultSetService.POST_VALIDATION_PARAM_NAME))
      new FilterConfig(configAsMap(ValidateResultSetService.PARAMETER_PARAM_NAME).asInstanceOf[String], configAsMap(ValidateResultSetService.THRESHOLD_PARAM_NAME).asInstanceOf[AnyVal], configAsMap(ValidateResultSetService.POST_VALIDATION_PARAM_NAME).asInstanceOf[Boolean])
    else
      new FilterConfig(configAsMap(ValidateResultSetService.PARAMETER_PARAM_NAME).asInstanceOf[String], configAsMap(ValidateResultSetService.THRESHOLD_PARAM_NAME).asInstanceOf[AnyVal])
  }

  def parsePepMatchFilters(params: NamedParamsRetriever): Option[Seq[IPeptideMatchFilter]] = {
    if (params.hasParam(PEP_MATCH_FILTERS_PARAM)) {
      val pepMatchFiltersConfigs = params.getList(PEP_MATCH_FILTERS_PARAM).toArray.map(parseFilterConfig(_))

      Some(pepMatchFiltersConfigs.map(fc => {
        val nextFilter = BuildPeptideMatchFilter(fc.parameter, fc.threshold.asInstanceOf[AnyVal])
        if (fc.postValidation)
          nextFilter.setAsPostValidationFilter(true)
        nextFilter
      }).toSeq)

    } else None
  }

  def parsePepMatchValidatorConfig(paramsMap: java.util.Map[String, Object]): PepMatchValidatorConfig = {
    deserialize[PepMatchValidatorConfig](serialize(paramsMap))
  }

  // def parsePepMatchValidator(params: NamedParamsRetriever, tdAnalyzer: Option[ITargetDecoyAnalyzer], targetRS: ResultSet): Option[IPeptideMatchValidator] = {
  def parsePepMatchValidator(params: NamedParamsRetriever, tdAnalyzer: Option[ITargetDecoyAnalyzer]): Option[IPeptideMatchValidator] = {
    if (params.hasParam(PEP_MATCH_VALIDATOR_CONFIG_PARAM)) {

      val pepMatchValidatorConfig = parsePepMatchValidatorConfig(params.getMap(PEP_MATCH_VALIDATOR_CONFIG_PARAM))

      val pepMatchValidationFilter = if (pepMatchValidatorConfig.expectedFdr.isDefined) {
        BuildOptimizablePeptideMatchFilter(pepMatchValidatorConfig.parameter)
      } else {
        val currentFilter = BuildPeptideMatchFilter(pepMatchValidatorConfig.parameter, pepMatchValidatorConfig.threshold.get.asInstanceOf[AnyVal])
        currentFilter
      }

      // Build PeptideMatchValidator
      Some(
        BuildPeptideMatchValidator(
          pepMatchValidationFilter,
          pepMatchValidatorConfig.expectedFdr,
          tdAnalyzer
        )
      )

    } else None
  }

  def parseProtSetFilters(params: NamedParamsRetriever): Option[Seq[IProteinSetFilter]] = {
    if (params.hasParam(PROT_SET_FILTERS_PARAM)) {
      val protSetFiltersConfigs = params.getList(PROT_SET_FILTERS_PARAM).toArray.map(parseFilterConfig(_))
      Some(protSetFiltersConfigs.map(fc => BuildProteinSetFilter(fc.parameter, fc.threshold.asInstanceOf[AnyVal])).toSeq)
    } else None
  }

  def parseProtSetValidatorConfig(paramsMap: java.util.Map[String, Object]): ProtSetValidatorConfig = {
    deserialize[ProtSetValidatorConfig](serialize(paramsMap))
  }

  def parseProtSetValidator(params: NamedParamsRetriever): Option[IProteinSetValidator] = {
    if (params.hasParam(PROT_SET_VALIDATOR_CONFIG_PARAM)) {

      val protSetValidatorConfig = parseProtSetValidatorConfig(params.getMap(PROT_SET_VALIDATOR_CONFIG_PARAM))
      val thresholds = protSetValidatorConfig.thresholds.map(_.map(e => e._1 -> e._2.asInstanceOf[AnyVal]))

      // Retrieve protein set validation method
      val protSetValMethod = ProtSetValidationMethods.withName(protSetValidatorConfig.validationMethod)

      // Build ProteinSetValidator
      Some(
        BuildProteinSetValidator(
          protSetValMethod,
          protSetValidatorConfig.parameter,
          thresholds,
          protSetValidatorConfig.expectedFdr
        )
      )

    } else None
  }

}

class ValidateResultSet extends AbstractRemoteProcessingService with IValidateResultSetService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    require(paramsRetriever != null, "No Parameters specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSetId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SET_ID_PARAM)

    var result = -1L

    var msiDbConnectionContext: DatabaseConnectionContext = null
    var msiDbTransacOk: Boolean = false

    val execCtx =  DbConnectionHelper.createSQLExecutionContext(projectId) 

    try {
      val validationConfig = ValidateResultSet.parseValidationConfig(paramsRetriever)

      // Use peptide match validator as sorter if provided, else use default ScorePSM         
      val sorter: IPeptideMatchSorter =
        if (validationConfig.pepMatchValidator.isDefined
          && validationConfig.pepMatchValidator.get.validationFilter.isInstanceOf[IPeptideMatchSorter])
          validationConfig.pepMatchValidator.get.validationFilter.asInstanceOf[IPeptideMatchSorter]
        else if (validationConfig.pepMatchPreFilters.isDefined) {
          var foundSorter: IPeptideMatchSorter = null
          val index = 0
          while (foundSorter == null && index < validationConfig.pepMatchPreFilters.get.size) {
            if (validationConfig.pepMatchPreFilters.get(index).isInstanceOf[IPeptideMatchSorter]) {
              foundSorter = validationConfig.pepMatchPreFilters.get(index).asInstanceOf[IPeptideMatchSorter]
            }
          }
          if (foundSorter == null)
            foundSorter = new ScorePSMFilter()
          foundSorter
        } else {
          new ScorePSMFilter()
        }

      // Begin transaction
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction() // Start a transaction on MSI Db
      msiDbTransacOk = false

      // Instantiate a result set validator
      val rsValidator = ResultSetValidator(
        execContext = execCtx,
        targetRsId = resultSetId,
        tdAnalyzer = validationConfig.tdAnalyzer,
        pepMatchPreFilters = validationConfig.pepMatchPreFilters,
        pepMatchValidator = validationConfig.pepMatchValidator,
        protSetFilters = validationConfig.protSetFilters,
        protSetValidator = validationConfig.protSetValidator,
        inferenceMethod = Some(InferenceMethod.PARSIMONIOUS),
        peptideSetScoring = Some(validationConfig.pepSetScoring.getOrElse(PepSetScoring.MASCOT_STANDARD_SCORE))
      )

      rsValidator.run()

      // Commit transaction
      msiDbConnectionContext.commitTransaction()
      msiDbTransacOk = true

      result = rsValidator.validatedTargetRsm.id
    } finally {

      if ((msiDbConnectionContext != null) && !msiDbTransacOk) {
        try {
          msiDbConnectionContext.rollbackTransaction()
        } catch {
          case ex: Exception => logger.error("Error rollbacking MSI Db Transaction", ex)
        }
      }

      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }

    }

    result
  }

}
