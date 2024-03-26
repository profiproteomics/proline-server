package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.{deserialize, serialize}
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.service.msi.{RsmPtmSitesIdentifier, RsmPtmSitesIdentifierV2, RsmPtmSitesIdentifierV3}
import fr.proline.cortex.api.service.dps.msi.{IIdentifyPtmSitesService, IIdentifyPtmSitesServiceV2, IIdentifyPtmSitesServiceV3}
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  A JMS Service to identify Ptm Sites of a given Result Summary
 *
 *  Input params :
 *    project_id : The id of the project the result summary belongs to.
 *    result_summary_id: The id of the result summary to filter.
 *    prot_set_filters: List of Proteins set filters to apply (name, threshold).
 *
 *  Output params :
 *    Boolean for service run status
 */
@deprecated (message = "Use IdentifyPtmSitesV3_0", since = "Version 2.2")
class IdentifyPtmSites extends AbstractRemoteProcessingService with IIdentifyPtmSitesService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSummaryId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)
    val force = if (paramsRetriever.hasParam(PROCESS_METHOD.FORCE_PARAM)) { paramsRetriever.getBoolean(PROCESS_METHOD.FORCE_PARAM) } else { false }
    
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)

    var msiDbConnectionContext: DatabaseConnectionContext = null

    try {
        msiDbConnectionContext = execCtx.getMSIDbConnectionContext
        msiDbConnectionContext.beginTransaction()

        // Instantiate a Ptm sites identifier
        val ptmSitesIdentifier = new RsmPtmSitesIdentifier(
          execContext = execCtx,
          resultSummaryId = resultSummaryId,
          force = force
        )

        ptmSitesIdentifier.run()

        //Commit transaction
        msiDbConnectionContext.commitTransaction()
    } catch {
      case t: Throwable =>
        throw ExceptionUtils.wrapThrowable("Error while identifying Ptm sites", t, appendCause = true)
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}

@deprecated (message = "Use IdentifyPtmSitesV3_0", since = "Version 2.2")
class IdentifyPtmSitesV2_0 extends AbstractRemoteProcessingService with IIdentifyPtmSitesServiceV2 with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSummaryId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)
    val ptmIds = paramsRetriever.getList(PROCESS_METHOD.PTM_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val clusteringConfigAsMap = deserialize[Map[String,Any]](serialize(paramsRetriever.getMap(PROCESS_METHOD.CLUSTERING_CONFIG_PARAM)))


    val force = if (paramsRetriever.hasParam(PROCESS_METHOD.FORCE_PARAM)) { paramsRetriever.getBoolean(PROCESS_METHOD.FORCE_PARAM) } else { false }

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)

    var msiDbConnectionContext: DatabaseConnectionContext = null

    try {
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction()

      // Instantiate a Ptm sites identifier
      val ptmSitesIdentifier = new RsmPtmSitesIdentifierV2(
        execContext = execCtx,
        resultSummaryId = resultSummaryId,
        ptmIds,
        clusteringConfigAsMap,
        force = force
      )

      ptmSitesIdentifier.run()

      //Commit transaction
      msiDbConnectionContext.commitTransaction()
    } catch {
      case t: Throwable =>
        throw ExceptionUtils.wrapThrowable("Error while identifying Ptm sites", t, appendCause = true)
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}

class IdentifyPtmSitesV3_0 extends AbstractRemoteProcessingService with IIdentifyPtmSitesServiceV3 with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSummaryId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)
    val ptmIds = paramsRetriever.getList(PROCESS_METHOD.PTM_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val clusteringConfigAsMap = deserialize[Map[String,Any]](serialize(paramsRetriever.getMap(PROCESS_METHOD.CLUSTERING_CONFIG_PARAM)))


    val force = if (paramsRetriever.hasParam(PROCESS_METHOD.FORCE_PARAM)) { paramsRetriever.getBoolean(PROCESS_METHOD.FORCE_PARAM) } else { false }

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)

    var msiDbConnectionContext: DatabaseConnectionContext = null

    try {
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction()

      // Instantiate a Ptm sites identifier
      val ptmSitesIdentifier = new RsmPtmSitesIdentifierV3(
        execContext = execCtx,
        resultSummaryId = resultSummaryId,
        ptmIds,
        clusteringConfigAsMap,
        force = force
      )

      ptmSitesIdentifier.run()

      //Commit transaction
      msiDbConnectionContext.commitTransaction()
    } catch {
      case t: Throwable =>
        throw ExceptionUtils.wrapThrowable("Error while identifying Ptm sites", t, appendCause = true)
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    true
  }
}
