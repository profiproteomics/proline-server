package fr.proline.cortex.service.dps.msi

import scala.Array.canBuildFrom

import com.fasterxml.jackson.databind.annotation.JsonDeserialize

import com.thetransactioncompany.jsonrpc2.JSONRPC2Error
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.algo.msi.InferenceMethods
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
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.orm.util.DataStoreConnectorFactory
import fr.proline.core.service.msi.ResultSetValidator
import fr.proline.core.service.msi.ValidationConfig
import fr.proline.cortex.service.IRemoteService
import fr.proline.cortex.util.jsonrpc.JSONRPC2Utils
import fr.proline.cortex.util.jsonrpc.ProfiJSONRPC2Response

case class FilterConfig(
  parameter: String,
  threshold: AnyVal,
  postValidation: Boolean = false)

case class PepMatchValidatorConfig(
  parameter: String,
  threshold: Option[AnyVal] = None,
  @JsonDeserialize(contentAs = classOf[java.lang.Float]) expectedFdr: Option[Float] = None)

case class ProtSetValidatorConfig(
  validationMethod: String,
  parameter: String,
  thresholds: Option[Map[String, AnyVal]] = None,
  @JsonDeserialize(contentAs = classOf[java.lang.Float]) expectedFdr: Option[Float] = None)

class ValidateResultSet extends IRemoteService with Logging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/ValidateResultSet"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  /* Define the concrete service method */
  override def service(jmsMessageContext: Map[String, Any], req: JSONRPC2Request): JSONRPC2Response = {
    require((req != null), "Req is null")

    val requestId = req.getID
    val methodName = req.getMethod

    /* Method dispatch */
    methodName match {

      case "process" => {
        val paramsRetriever = JSONRPC2Utils.buildParamsRetriever(req)

        val result = doProcess(paramsRetriever) // Call service

        new ProfiJSONRPC2Response(java.lang.Long.valueOf(result), requestId)
      }

      // Method name not supported
      case _ => new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
    }

  }

  private def doProcess(paramsRetriever: NamedParamsRetriever): Long = {
    require((paramsRetriever != null), "ParamsRetriever is null")

    val projectId = paramsRetriever.getLong("project_id")
    val resultSetId = paramsRetriever.getLong("result_set_id")
    val description = paramsRetriever.getString("description")
    val useTdCompet = if (paramsRetriever.hasParam("use_td_competition")) paramsRetriever.getBoolean("use_td_competition") else false

    var result: Long = -1L;

    var msiDbConnectionContext: DatabaseConnectionContext = null
    var msiDbTransacOk: Boolean = false

    val execCtx = BuildExecutionContext(DataStoreConnectorFactory.getInstance(), projectId, false)

    try {
      val validationConfig = parseValidationConfig(paramsRetriever)

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
        inferenceMethod = Some(InferenceMethods.communist),
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

  def parseValidationConfig(paramsRetriever: NamedParamsRetriever): ValidationConfig = {

    // val pepMatchFilters =  .parsePepMatchFilters(params,targetRS )
    val pepMatchFilters = parsePepMatchFilters(paramsRetriever)

    // Create a default TD analyzer 
    val tdAnalyzer: Option[ITargetDecoyAnalyzer] = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED))
    val pepMatchValidator = parsePepMatchValidator(paramsRetriever, tdAnalyzer)
    val pepSetScoring = Option(paramsRetriever.getOptString("pep_set_score_type", true, null)).map(PepSetScoring.withName(_))
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
    if (configAsMap.contains("post_validation"))
      new FilterConfig(configAsMap("parameter").asInstanceOf[String], configAsMap("threshold").asInstanceOf[AnyVal], configAsMap.contains("post_validation"))
    else
      new FilterConfig(configAsMap("parameter").asInstanceOf[String], configAsMap("threshold").asInstanceOf[AnyVal])
  }

  def parsePepMatchFilters(params: NamedParamsRetriever): Option[Seq[IPeptideMatchFilter]] = {
    if (params.hasParam("pep_match_filters")) {
      val pepMatchFiltersConfigs = params.getList("pep_match_filters").toArray.map(parseFilterConfig(_))

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
    if (params.hasParam("pep_match_validator_config")) {

      val pepMatchValidatorConfig = parsePepMatchValidatorConfig(params.getMap("pep_match_validator_config"))

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
    if (params.hasParam("prot_set_filters")) {
      val protSetFiltersConfigs = params.getList("prot_set_filters").toArray.map(parseFilterConfig(_))
      Some(protSetFiltersConfigs.map(fc => BuildProteinSetFilter(fc.parameter, fc.threshold.asInstanceOf[AnyVal])).toSeq)
    } else None
  }

  def parseProtSetValidatorConfig(paramsMap: java.util.Map[String, Object]): ProtSetValidatorConfig = {
    deserialize[ProtSetValidatorConfig](serialize(paramsMap))
  }

  def parseProtSetValidator(params: NamedParamsRetriever): Option[IProteinSetValidator] = {
    if (params.hasParam("prot_set_validator_config")) {

      val protSetValidatorConfig = parseProtSetValidatorConfig(params.getMap("prot_set_validator_config"))
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