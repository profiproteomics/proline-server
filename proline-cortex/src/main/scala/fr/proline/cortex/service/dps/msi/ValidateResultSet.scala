package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.algo.msi.InferenceMethod
import fr.proline.core.algo.msi.filtering.{IOptimizablePeptideMatchFilter, IPeptideInstanceFilter, IPeptideMatchFilter, IProteinSetFilter, PepMatchFilterParams, ProtSetFilterParams}
import fr.proline.core.algo.msi.scoring.PepSetScoring
import fr.proline.core.algo.msi.validation._
import fr.proline.core.algo.msi.validation.pepinstance.BHPeptideInstanceValidator
import fr.proline.core.algo.msi.validation.pepmatch.{BHPepMatchValidator, TDPepMatchValidator, TDPepMatchValidatorWithFDROptimization}
import fr.proline.core.algo.msi.validation.proteinset.BHProtSetValidator
import fr.proline.core.service.msi.ResultSetValidator
import fr.proline.core.service.msi.ValidationConfig
import fr.proline.cortex.api.service.dps.msi._
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

import scala.Array.canBuildFrom
import scala.collection.immutable.HashMap


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
    val fdrAnalyzerConfig = parseFDRAnalyzerConfig(paramsRetriever)
    val pepMatchValidator = parsePepMatchValidator(paramsRetriever, fdrAnalyzerConfig)
    val peptideBuilder = BuildPeptideInstanceBuilder(PeptideInstanceBuilders.withName(paramsRetriever.getOptString(PEPTIDE_BUILDER_CONFIG_PARAM, true, "STANDARD")))
    val peptideFilters = parsePeptideFilters(paramsRetriever)
    val peptideValidator = parsePeptideValidator(paramsRetriever, fdrAnalyzerConfig)
    val pepSetScoring = Option(paramsRetriever.getOptString(PEP_SET_SCORE_TYPE_PARAM, true, null)).map(PepSetScoring.withName(_))
    val protSetFilters = parseProtSetFilters(paramsRetriever)
    val protSetValidator = parseProtSetValidator(paramsRetriever, fdrAnalyzerConfig)

    val tdAnalyzerBuilder = if (fdrAnalyzerConfig.isDefined && fdrAnalyzerConfig.get.methodName == FDRAnalyzerMethods.TARGET_DECOY.toString) {
      val tdAnalyzerConfig = fdrAnalyzerConfig.get.tdAnalyzerConfig
      require(tdAnalyzerConfig.isDefined, "When fdr control method is set to target/decoy, a td analyzer config must be provided")
      val tdAnalyzer = TargetDecoyAnalyzers.values.find(_.toString.toLowerCase == tdAnalyzerConfig.get.methodName.toLowerCase()).getOrElse(TargetDecoyAnalyzers.BASIC)
      val tdEstimator = TargetDecoyEstimators.values.find(_.toString.toLowerCase == tdAnalyzerConfig.get.estimatorName.getOrElse("").toLowerCase())

       Some(new TDAnalyzerBuilder(
        analyzer = tdAnalyzer,
        estimator = tdEstimator,
        params = tdAnalyzerConfig.get.params))
      } else {
      None
    }

    ValidationConfig(
      tdAnalyzerBuilder = tdAnalyzerBuilder,
      pepMatchPreFilters = pepMatchFilters,
      pepMatchValidator = pepMatchValidator,
      peptideBuilder = peptideBuilder,
      peptideFilters = peptideFilters,
      peptideValidator = peptideValidator,
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

      Some(pepMatchFiltersConfigs.map(fc => {BuildPeptideMatchFilter(fc.parameter, fc.threshold.asInstanceOf[AnyVal])} ).toSeq)

    } else None
  }

  def parsePepMatchValidatorConfig(paramsMap: java.util.Map[String, Object]): PepMatchValidatorConfig = {
    deserialize[PepMatchValidatorConfig](serialize(paramsMap))
  }

  def parsePepMatchValidator(params: NamedParamsRetriever, fdrAnalyzer: Option[FDRAnalyzerConfig]): Option[IPeptideMatchValidator] = {

    if (params.hasParam(PEP_MATCH_VALIDATOR_CONFIG_PARAM)) {

      val pepMatchValidatorConfig = parsePepMatchValidatorConfig(params.getMap(PEP_MATCH_VALIDATOR_CONFIG_PARAM))
      if (fdrAnalyzer.isDefined && fdrAnalyzer.get.methodName == FDRAnalyzerMethods.TARGET_DECOY.toString) {
        val pepMatchValidator = if (pepMatchValidatorConfig.expectedFdr.isDefined) {

          val valFilter = BuildOptimizablePeptideMatchFilter(pepMatchValidatorConfig.parameter)
          new TDPepMatchValidatorWithFDROptimization(valFilter, pepMatchValidatorConfig.expectedFdr)
        } else {

          val valFilter = BuildPeptideMatchFilter(pepMatchValidatorConfig.parameter, pepMatchValidatorConfig.threshold.get)
          new TDPepMatchValidator(valFilter)
        }

        Some(pepMatchValidator)
      } else if (fdrAnalyzer.isDefined && fdrAnalyzer.get.methodName == FDRAnalyzerMethods.BH.toString) {
        Some(new BHPepMatchValidator(Some(pepMatchValidatorConfig.expectedFdr.get)))
      } else {
        None
      }
    } else {
      None
    }
  }

  def parsePeptideFilters(params: NamedParamsRetriever): Option[Seq[IPeptideInstanceFilter]] = {
    if (params.hasParam(PEPTIDE_FILTERS_PARAM)) {
      val pepFiltersConfigs = params.getList(PEPTIDE_FILTERS_PARAM).toArray.map(parseFilterConfig(_))
      Some(pepFiltersConfigs.map(fc => {
        BuildPeptideInstanceFilter(fc.parameter, fc.threshold.asInstanceOf[AnyVal])
      }).toSeq)
    } else None
  }

  def parsePeptideValidator(params: NamedParamsRetriever, fdrAnalyzer: Option[FDRAnalyzerConfig]): Option[IPeptideInstanceValidator] = {

   if (params.hasParam(PEPTIDE_VALIDATOR_CONFIG_PARAM)) {

      val peptideValidatorConfig = deserialize[PeptideValidatorConfig](serialize(params.getMap(PEPTIDE_VALIDATOR_CONFIG_PARAM)))
      val peptideValidator = if (fdrAnalyzer.isDefined && fdrAnalyzer.get.methodName == FDRAnalyzerMethods.BH.toString) {
        val valFilter = BuildPeptideInstanceFilter(PepMatchFilterParams.BH_AJUSTED_PVALUE.toString, peptideValidatorConfig.expectedFdr.get/100.0f)
        Some(new BHPeptideInstanceValidator(valFilter, Some(peptideValidatorConfig.expectedFdr.get)))
      } else {

        //TODO implements some target decoy methods similar to PSM validation methods

        None

      }

      peptideValidator

    } else {

      None

    }
  }


  def parseFDRAnalyzerConfig(params: NamedParamsRetriever): Option[FDRAnalyzerConfig] = {
    if (params.hasParam(FDR_ANALYZER_PARAM)) {
      Some(deserialize[FDRAnalyzerConfig](serialize(params.getMap(FDR_ANALYZER_PARAM))))
    } else {
      // service V2.0 compatibility: if a PSM or Protein validator using an expected FDR is defined, create an basic
      // target/decoy FDR analyzer
      if (params.hasParam(PEP_MATCH_VALIDATOR_CONFIG_PARAM)) {
        val pepMatchValidatorConfig = parsePepMatchValidatorConfig(params.getMap(PEP_MATCH_VALIDATOR_CONFIG_PARAM))
        if (pepMatchValidatorConfig.expectedFdr.isDefined) {
          Some(FDRAnalyzerConfig(FDRAnalyzerMethods.TARGET_DECOY.toString, Some(new TDAnalyzerConfig("BASIC"))))
        } else {
        None
        }
      } else if (params.hasParam(PROT_SET_VALIDATOR_CONFIG_PARAM)) {
        val protSetValidatorConfig = parseProtSetValidatorConfig(params.getMap(PROT_SET_VALIDATOR_CONFIG_PARAM))
        if (protSetValidatorConfig.expectedFdr.isDefined) {
          Some(FDRAnalyzerConfig(FDRAnalyzerMethods.TARGET_DECOY.toString, Some(new TDAnalyzerConfig("BASIC"))))
        } else {
          None
        }
      } else {
        None
      }
    }
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

  def parseProtSetValidator(params: NamedParamsRetriever, fdrAnalyzer: Option[FDRAnalyzerConfig]): Option[IProteinSetValidator] = {
    if (params.hasParam(PROT_SET_VALIDATOR_CONFIG_PARAM)) {

      val protSetValidatorConfig = parseProtSetValidatorConfig(params.getMap(PROT_SET_VALIDATOR_CONFIG_PARAM))

      if (! fdrAnalyzer.isDefined || fdrAnalyzer.get.methodName != FDRAnalyzerMethods.BH.toString) {
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
      } else {
        val filter = BuildProteinSetFilter(ProtSetFilterParams.BH_ADJUSTED_PVALUE.toString, protSetValidatorConfig.expectedFdr.get/100.0f)
        Some(new BHProtSetValidator(filter, Some(protSetValidatorConfig.expectedFdr.get)))
      }
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

      // Begin transaction
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction() // Start a transaction on MSI Db
      msiDbTransacOk = false

      // Instantiate a result set validator
      val rsValidator = ResultSetValidator(
        execContext = execCtx,
        targetRsId = resultSetId,
        validationConfig = validationConfig,
        inferenceMethod = Some(InferenceMethod.PARSIMONIOUS)
      )

      rsValidator.run()

      // Commit transaction
      msiDbConnectionContext.commitTransaction()
      msiDbTransacOk = true

      result = rsValidator.validatedTargetRsm.id
    } finally {

      if (!msiDbTransacOk) {
        DbConnectionHelper.tryToRollbackDbTransaction(msiDbConnectionContext)
      }

      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }

}

abstract class AbstractValidateResultSet extends AbstractRemoteProcessingService {

  def process(paramsRetriever: NamedParamsRetriever, serviceParams: IValidateResultSetServiceParams): Any = {

    require(paramsRetriever != null, "No Parameters specified")

    val projectId = paramsRetriever.getLong(serviceParams.PROJECT_ID_PARAM)
    val resultSetId = paramsRetriever.getLong(serviceParams.RESULT_SET_ID_PARAM)
    val propagatePSMFilters = if(paramsRetriever.hasParam(serviceParams.PROPAGATE_PEP_MATCH_VALIDATION_PARAM)) paramsRetriever.getBoolean(serviceParams.PROPAGATE_PEP_MATCH_VALIDATION_PARAM) else false
    val propagateProtSetFilters = if(paramsRetriever.hasParam(serviceParams.PROPAGATE_PROT_SET_VALIDATION_PARAM)) paramsRetriever.getBoolean(serviceParams.PROPAGATE_PROT_SET_VALIDATION_PARAM) else false

    var result: HashMap[Long, Long] = null

    var msiDbConnectionContext: DatabaseConnectionContext = null
    var msiDbTransacOk: Boolean = false

    val execCtx =  DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      val validationConfig = ValidateResultSet.parseValidationConfig(paramsRetriever)

      // Begin transaction
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction() // Start a transaction on MSI Db
      msiDbTransacOk = false

      // Instantiate a result set validator
      val rsValidator = ResultSetValidator(
        execContext = execCtx,
        targetRsId = resultSetId,
        validationConfig = validationConfig,
        inferenceMethod = Some(InferenceMethod.PARSIMONIOUS),
        storeResultSummary = true,
        propagatePepMatchValidation= propagatePSMFilters,
        propagateProtSetValidation= propagateProtSetFilters
      )

      rsValidator.run()

      // Commit transaction
      msiDbConnectionContext.commitTransaction()
      msiDbTransacOk = true

      result = rsValidator.targetRSMIdPerRsId
    } finally {

      if (!msiDbTransacOk) {
        DbConnectionHelper.tryToRollbackDbTransaction(msiDbConnectionContext)
      }

      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }
}

class ValidateResultSetV2 extends AbstractValidateResultSet with IValidateResultSetServiceV2 {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    process(paramsRetriever, PROCESS_METHOD)
  }
}

class ValidateResultSetV3 extends AbstractValidateResultSet with IValidateResultSetServiceV3 {

    def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
      process(paramsRetriever, PROCESS_METHOD)
    }

  }