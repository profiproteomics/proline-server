package fr.proline.cortex.service.dps.msi

import java.sql.Connection

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.service.msi.SpectraParamsUpdater
import fr.proline.cortex.api.service.dps.msi.IUpdateSpectraParamsServiceV1_0
import fr.proline.cortex.api.service.dps.msi.IUpdateSpectraParamsServiceV2_0
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.repository.util.JDBCWork


/**
 *  Define JMS Service which allows to update scan, cycle and time information of spectra belonging to specified peaklists.
 *  Information are extracted from the spectrum title string by applying the regexes corresponding to the given parsing rule.
 *
 * Input params :
 *  project_id : The id of the project the peaklists will be searched in
 *  peaklist_ids : The ids of the peaklists to update.
 *  spec_title_rule_id : The id of the spectrum title parsing rule to use.
 *
 * Output params :
 *  new project Id.
 *
 */
class UpdateSpectraParamsV1_0 extends AbstractRemoteProcessingService with IUpdateSpectraParamsServiceV1_0 with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    require(paramsRetriever != null, "No Parameters specified")

    val projectId = paramsRetriever.getLong(PROJECT_ID_PARAM)
    val peaklistIds = paramsRetriever.getList(PROCESS_METHOD.PEAKLIST_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val specTitleRuleId = paramsRetriever.getLong(PROCESS_METHOD.SPEC_TITLE_RULE_ID_PARAM)

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) 

    var updatedSpectraCount = 0
    try {
      logger.info("UpdateSpectraParams WebService is starting...")

      updatedSpectraCount = SpectraParamsUpdater.updateSpectraParams(execCtx, projectId, peaklistIds, specTitleRuleId)

      logger.info(updatedSpectraCount + " spectra updated !")
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    updatedSpectraCount
  }
}


/**
 *  Define JMS Service which allows to update scan, cycle and time information of spectra belonging to specified ResultSsets.
 *  Information are extracted from the spectrum title string by applying the regexes corresponding to the parsing rule associated to specific peaklist_software
 *
 * Input params :
 *  project_id : The id of the project the peaklists will be searched in
 *  resultset_ids : The ids of the resultsets to update. These RS should be SEARCH ResultSet
 *  peaklist_software_id : id in datastore of the new software to use to update spectra params
 *
 * Output params :
 *  nbr updated spectra
 *
 */
class UpdateSpectraParamsV2_0 extends AbstractRemoteProcessingService with IUpdateSpectraParamsServiceV2_0 with LazyLogging {
  
  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {
    require(paramsRetriever != null, "No Parameters specified")

    val projectId = paramsRetriever.getLong(PROJECT_ID_PARAM)
    val rsIds = paramsRetriever.getList(PROCESS_METHOD.RESULT_SET_IDS_PARAM).toArray.map { rf => deserialize[Long](serialize(rf)) }
    val peaklistSoftwareId = paramsRetriever.getLong(PROCESS_METHOD.PEAKLIST_SOFTWARE_ID_PARAM)
    val peaklistIdsBuilder = Seq.newBuilder[Long]
    var specTitleParsingRuleId : Long = -1l
    val execCtx =  DbConnectionHelper.createJPAExecutionContext(projectId) 

    var updatedSpectraCount: Integer = 0
    try {
      
      //Get peaklistIds for specified ResultSets
       val getPeaklistIDsWork = new JDBCWork() {
  
        override def execute(con: Connection) {
  
          val queryStr = "select msis.peaklist_id from result_set rs, msi_search msis where rs.msi_search_id = msis.id and rs.id IN ("+ rsIds.mkString(",") + ")"
          val stmt = con.createStatement()
          stmt.execute(queryStr)
          val sqlResultSet = stmt.getResultSet
          while (sqlResultSet.next) {
            peaklistIdsBuilder += sqlResultSet.getLong(1)
          }
          stmt.close()
        } // End of jdbcWork anonymous inner class
      }
      execCtx.getMSIDbConnectionContext.doWork(getPeaklistIDsWork, false)

       //Get specTitleParsing Rules for specified peaklistSoftware
      val getSpecTitleIdsWork = new JDBCWork() {
  
        override def execute(con: Connection) {
  
          val pStmt = con.prepareStatement("select ps.spec_title_parsing_rule_id from peaklist_software ps where ps.id = ? ")
          pStmt.setLong(1, peaklistSoftwareId)
          val result = pStmt.executeQuery()
          if (result.next) {
            specTitleParsingRuleId = result.getLong(1)
          }
          pStmt.close()
        } // End of jdbcWork anonymous inner class
      }
      execCtx.getUDSDbConnectionContext.doWork(getSpecTitleIdsWork, false)

      require(specTitleParsingRuleId >0,"No spectrum parsing rule found for peaklist software # "+peaklistSoftwareId)
      
      logger.info("UpdateSpectraParams WebService is starting...")

      updatedSpectraCount = SpectraParamsUpdater.updateSpectraParams(execCtx, projectId, peaklistIdsBuilder.result().toArray,specTitleParsingRuleId )

      logger.info(updatedSpectraCount + " spectra updated !")
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    updatedSpectraCount
  }
}