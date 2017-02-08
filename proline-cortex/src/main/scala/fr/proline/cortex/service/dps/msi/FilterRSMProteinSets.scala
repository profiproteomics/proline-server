package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.algo.msi.filtering.IProteinSetFilter
import fr.proline.core.algo.msi.validation.BuildProteinSetFilter
import fr.proline.core.service.msi.RSMProteinSetFilterer
import fr.proline.cortex.api.service.dps.msi.FilterConfig
import fr.proline.cortex.api.service.dps.msi.IFilterRSMProteinSetsService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  Define JMS Service which Filters ProteinSets of a given Result Summary
 *
 *  Input params :
 *    project_id : The id of the project the result summary belongs to.
 *    result_summary_id: The id of the result summary to filter.
 *    prot_set_filters: List of Proteins set filters to apply (name, threshold).
 *
 *  Output params :
 *    Boolean for service run status
 */

class FilterRSMProteinSets extends AbstractRemoteProcessingService with IFilterRSMProteinSetsService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSummaryId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)

    var msiDbConnectionContext: DatabaseConnectionContext = null

    try {
      val filterConfigs = this.parseProtSetFilters(paramsRetriever)

      if (filterConfigs.isDefined) {

        // Begin transaction
        msiDbConnectionContext = execCtx.getMSIDbConnectionContext
        msiDbConnectionContext.beginTransaction()

        // Instantiate a result set validator
        val rsmFilterer = RSMProteinSetFilterer(
          execCtx = execCtx,
          targetRsmId = resultSummaryId,
          protSetFilters = filterConfigs.get
        )

        rsmFilterer.run

        //Commit transaction
        msiDbConnectionContext.commitTransaction()
      }
    } catch {
      case t: Throwable => {
        throw ExceptionUtils.wrapThrowable("Error while filtering the Protein Sets", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }

  def parseProtSetFilters(params: NamedParamsRetriever): Option[Seq[IProteinSetFilter]] = {
    if (params.hasParam(PROCESS_METHOD.PROT_SET_FILTERS_PARAM) == false) None
    else {
      val protSetFiltersConfigs = params.getList(PROCESS_METHOD.PROT_SET_FILTERS_PARAM).toArray.map(parseFilterConfig(_))

      val filters = protSetFiltersConfigs.map { fc =>
        logger.debug(" ---- TRY to BuildProteinSetFilter for " + fc.parameter + " threshold " + fc.threshold)
        BuildProteinSetFilter(fc.parameter, fc.threshold)
      }

      Some(filters)
    }
  }

  def parseFilterConfig(paramsMap: Object): FilterConfig = {
    val configAsMap = deserialize[Map[String, AnyRef]](serialize(paramsMap))
    if (configAsMap.contains("post_validation"))
      new FilterConfig(configAsMap("parameter").asInstanceOf[String], configAsMap("threshold").asInstanceOf[AnyVal], configAsMap("post_validation").asInstanceOf[Boolean])
    else
      new FilterConfig(configAsMap("parameter").asInstanceOf[String], configAsMap("threshold").asInstanceOf[AnyVal])
  }

}