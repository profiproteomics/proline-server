/*
 * Copyright (C)  2026.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the CeCILL FREE SOFTWARE LICENSE AGREEMENT
 * ; either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * CeCILL License V2.1 for more details.
 *
 * You should have received a copy of the CeCILL License
 * along with this program;
 * If not, see <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html>.
 *
 */

package fr.proline.module.paser.diann.builder;

import fr.proline.context.IExecutionContext;
import fr.proline.context.UdsDbConnectionContext;
import fr.proline.core.algo.msi.AdditionMode;
import fr.proline.core.algo.msi.InferenceMethod;
import fr.proline.core.algo.msi.scoring.PepSetScoring;
import fr.proline.core.algo.msi.validation.BuildPeptideInstanceBuilder;
import fr.proline.core.dal.tables.uds.*;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.ResultSummary;
import fr.proline.core.om.model.msq.MasterQuantChannel;
import fr.proline.core.om.model.msq.QuantChannel;
import fr.proline.core.service.msi.ResultSetValidator;
import fr.proline.core.service.msi.ResultSummaryMerger;
import fr.proline.core.service.msi.ValidationConfig;
import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.quantify.DiaNNQuantifier;
import fr.proline.repository.util.JDBCWork;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.JavaConverters;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DiaNNProcessData {

  private static final Logger logger = LoggerFactory.getLogger(DiaNNProcessData.class);
  DiaNNResult m_diannResult;
  IExecutionContext m_executionContext;
  Map<String, ResultSet> m_allResultSetsByRun;
  Map<Long, ResultSummary> m_allResultSummaryByRsId;
  Map<String, Set<String>> m_precIdByPepKey ;
  Long m_createdQuantDatasetId;
  boolean m_serviceRun = false;

  public DiaNNProcessData(IExecutionContext context, DiaNNResult diannResult, Map<String, ResultSet> identResultSetsByRun, Map<String, Set<String>> precIdByPepKey) {
    this.m_diannResult = diannResult;
    this.m_allResultSetsByRun = identResultSetsByRun;
    this.m_executionContext = context;
    this.m_precIdByPepKey = precIdByPepKey;
  }


  /**
   * This method should be used in specific cases where resultSaummary was alerady created for diaNN run resultset
   * @param context ExecutionConetxt to get access to datastore provider and storer et conection
   * @param diannResult DiaNNResult to create Proline Quantitation for
   * @param identResultSetsByRun proline ResultSet corresponding to each run in DiaNN result
   * @param rsmByRsId ResultSummary for resultSet corresponding to each run in DiaNN result
   */
  public DiaNNProcessData(IExecutionContext context, DiaNNResult diannResult, Map<String, ResultSet> identResultSetsByRun,   Map<String, Set<String>> precIdByPepKey , Map<Long, ResultSummary> rsmByRsId) {
    this.m_diannResult = diannResult;
    this.m_allResultSetsByRun = identResultSetsByRun;
    this.m_executionContext = context;
    this.m_precIdByPepKey = precIdByPepKey;
    this.m_allResultSummaryByRsId = rsmByRsId;
  }

  public void runService(){

    logger.info("--- Starting DiaNN Process data Service ---");
    logger.debug(" -- Run Validator ");
    // Validate and merge RSM if needed
    if(m_allResultSummaryByRsId == null)
      m_allResultSummaryByRsId = validateResultSets();

    logger.debug(" -- Run RSM Merger");
    ResultSummary mergedRSM = mergeResultSummaries();

    logger.debug("-- Merge RSM created with id {}", mergedRSM.id());

    // Create Quantitation DS
    try {
      logger.debug("-- Run Create Exp Design");
      MasterQuantChannel mqChannel  = createQuantitationExpDesign(mergedRSM);

//      EntityManager em = m_executionContext.getUDSDbConnectionContext().getEntityManager();
//      TypedQuery<MasterQuantitationChannel> mqcQuery = em.createQuery("Select mqc from fr.proline.core.orm.uds.MasterQuantitationChannel mqc WHERE id = "+mqcId, MasterQuantitationChannel.class);
//      MasterQuantitationChannel mqc = mqcQuery.getSingleResult();
//      MasterQuantChannelEntityCache cacheEntities = new MasterQuantChannelEntityCache(m_executionContext, mqc);


      logger.debug("-- Run Quantifier");
      DiaNNQuantifier quantifier = new DiaNNQuantifier(mqChannel, m_allResultSetsByRun, mergedRSM, m_diannResult, m_precIdByPepKey, m_executionContext);
      quantifier.quantify();
      m_serviceRun  = true;
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }

    // Compute Quant Entities

    //store quant mergedRSM
  }

  private @NonNull ResultSummary mergeResultSummaries() {
//    ResultSummaryAdder rsmBuilder = new ResultSummaryAdder(
//            ResultSummary.generateNewId(),
//            false,
//            PeptideSetScoreUpdater.apply(PepSetScoring.MASCOT_STANDARD_SCORE()),
//            AdditionMode.UNION()
//    );
//
//    for (ResultSummary identRsm : m_allResultSummaryByRsId.values()) {
//      rsmBuilder.addResultSummary(identRsm);
//    }
//
//    ResultSummary mergedRSM = rsmBuilder.toResultSummary();
//    mergedRSM.isQuantified_$eq(true);

//    logger.debug("  - store merged result summary");

    List<ResultSummary> rsmIds = new ArrayList<>(m_allResultSummaryByRsId.values());

    ResultSummaryMerger merger = new ResultSummaryMerger(m_executionContext,Option.empty(),
            Option.apply(JavaConverters.asScalaBuffer(rsmIds).toSeq()), Option.apply(AdditionMode.UNION()),false);
    merger.runService();
    return merger.mergedResultSummary();
  }

  public Map<Long,Long> getRSMIdsByRSIds() {
    if(!m_serviceRun)
      throw new IllegalStateException("Service has not been executed ");
    Map<Long,Long> rsmIdByRsIs = new  HashMap<>();
    m_allResultSummaryByRsId.keySet().forEach(e -> {
      rsmIdByRsIs.put(e, m_allResultSummaryByRsId.get(e).id());
    });
    return rsmIdByRsIs;
  }

  public Long getCreatedQuantDatasetId() {
    if(!m_serviceRun)
      throw new IllegalStateException("Service has not been executed ");

    return m_createdQuantDatasetId;
  }


  /**
   * Create all datastore objects for experimental design associated o this quantitation
   * result: Biological_group, Biological_Sample, Quant_chanel ...
   *
   * @return CreatedMasterQuanChannel of the masterquantchannel for this quantittaion
   * @throws SQLException if an error occurs during data creation
   */
  protected MasterQuantChannel createQuantitationExpDesign(ResultSummary quantRSM) throws SQLException {

    long pId = m_executionContext.getProjectId();
    UdsDbConnectionContext udsDbCtx = m_executionContext.getUDSDbConnectionContext();
    boolean localUDSTransaction= false;
    boolean udsTransacOk= false;
    try {
      if (!udsDbCtx.isInTransaction()) {
        udsDbCtx.beginTransaction();
        localUDSTransaction = true;
      }

      final int[] qDatasetNbr = {0};
      JDBCWork jdbcGetDSNumber = connection -> {
        String sql = "Select max(number) from data_set where project_id = ? and type = 'QUANTITATION' and (parent_dataset_id is null or parent_dataset_id =0 )";
        PreparedStatement pStmt = connection.prepareStatement(sql);
        pStmt.setLong(1, pId);
        java.sql.ResultSet result = pStmt.executeQuery();
        if(result.next()){
          qDatasetNbr[0] = result.getInt(1)+1;
        }
      };

      udsDbCtx.doWork(jdbcGetDSNumber, false);

      final Long[] qMQChId = new Long[1];
      final List<QuantChannel> qChannels = new ArrayList<>();
      final Map<String, Long> splAnalysisPerRun = new HashMap<>();
      JDBCWork jdbcCreateDS = connection -> {

        logger.debug(" * Creating quantitation dataset ... ");
        long qDSId = insertIntoDataset(connection, qDatasetNbr[0], pId );
        logger.debug("     ... done with ID "+qDSId);
        m_createdQuantDatasetId = qDSId;

        logger.debug(" * Creating quantitation group");
        long grpSetupId = insertIntoGrpSetupId(connection, qDSId);

        logger.debug(" * Creating Biological group");
        long bioGrpId = insertIntoBioGrp(connection, qDSId, grpSetupId);

        logger.debug(" * Creating Biological Sample");
        long bioSplId = insertIntoBioSpl(connection, qDSId, bioGrpId);

        logger.debug(" * Creating MasterQuantChannel ");
        qMQChId[0] = insertIntoMasterQChannel(connection, qDSId, quantRSM.id());
        logger.debug("     ... done with ID "+qMQChId[0]);

        //Create one SampleAnalysis per run
        int splNbr=1;
        logger.debug(" * Creating quantChannels ");
        qChannels.addAll( insertIntoQChannels(connection, qDSId, splAnalysisPerRun, bioSplId, splNbr, qMQChId[0]));
      };

      udsDbCtx.doWork(jdbcCreateDS, false);
      logger.debug(" * Quantitation Dataset created with MasterQuant Channel id {}, created {} sample analysis",  qMQChId[0], splAnalysisPerRun.size());

      //Get Back created MasterQuantitationChannel
      MasterQuantChannel mqCh = new MasterQuantChannel(qMQChId[0],1,Option.apply("Quant " + m_diannResult.getName()),
              Option.empty(), Option.empty(), Option.empty(), Option.empty(), qChannels.toArray(new QuantChannel[0]));

      if (localUDSTransaction) {
        udsDbCtx.commitTransaction();
      }
      udsTransacOk = true;
      return mqCh;
    } finally {
      if (localUDSTransaction && !udsTransacOk) {
        logger.info("Roll backing MSI Db Transaction");

        try {
          udsDbCtx.rollbackTransaction();
        } catch (Exception ex){
          logger.error("Error roll backing MSI Db Transaction", ex);
        }

      }
    }
  }

  private List<QuantChannel> insertIntoQChannels(Connection connection, long qDSId, Map<String, Long> splAnalysisPerRun, long bioSplId, int splNbr, Long qMQChId) throws SQLException {
    List<QuantChannel> qChannels = new ArrayList<>();
    String sqlQuery = "INSERT INTO " + UdsDbSampleAnalysisTable$.MODULE$.name() +
            " (" + UdsDbSampleAnalysisColumns.QUANTITATION_ID()
            + ") VALUES (?) ";
    String sqlQuery2 = "INSERT INTO " + UdsDbBiologicalSampleSampleAnalysisMapTable$.MODULE$.name() +
            " (" + UdsDbBiologicalSampleSampleAnalysisMapColumns.SAMPLE_ANALYSIS_ID() + "," + UdsDbBiologicalSampleSampleAnalysisMapColumns.BIOLOGICAL_SAMPLE_ID()
            + "," + UdsDbBiologicalSampleSampleAnalysisMapColumns.SAMPLE_ANALYSIS_NUMBER() + ") VALUES (?,?,?) ";
    String  sqlQuery3 = "INSERT INTO " + UdsDbQuantChannelTable$.MODULE$.name() +
            " (" + UdsDbQuantChannelColumns.QUANTITATION_ID() + "," + UdsDbQuantChannelColumns.NAME()+ ", "+ UdsDbQuantChannelColumns.NUMBER()
            +", "+ UdsDbQuantChannelColumns.CONTEXT_KEY() + "," + UdsDbQuantChannelColumns.SAMPLE_ANALYSIS_ID()  + "," + UdsDbQuantChannelColumns.BIOLOGICAL_SAMPLE_ID()
            + "," + UdsDbQuantChannelColumns.MASTER_QUANT_CHANNEL_ID()  + "," + UdsDbQuantChannelColumns.IDENT_RESULT_SUMMARY_ID()
            + ") VALUES (?,?,?,?,?,?,?,?) ";
    for (String run : m_diannResult.getRuns()){
      try(PreparedStatement pStmt = connection.prepareStatement(sqlQuery,  new String[] { "id" });
          PreparedStatement pStmt2 = connection.prepareStatement(sqlQuery2);
          PreparedStatement pStmt3 = connection.prepareStatement(sqlQuery3,  new String[] { "id" })) {

        pStmt.setLong(1, qDSId);
        pStmt.executeUpdate();
        java.sql.ResultSet keyRS = pStmt.getGeneratedKeys();
        long spAnaId;
        if (keyRS.next()) {
          spAnaId = keyRS.getLong(1);
          splAnalysisPerRun.put(run, spAnaId);
        } else {
          throw new SQLException("Quantitation SampleA Analysis Id not found");
        }

        pStmt2.setLong(1, spAnaId);
        pStmt2.setLong(2, bioSplId);
        pStmt2.setInt(3, splNbr);
        pStmt2.executeUpdate();

        Long rsId = this.m_allResultSetsByRun.get(run).id();
        long rsmId = m_allResultSummaryByRsId.get(rsId).id();

        pStmt3.setLong(1, qDSId);
        pStmt3.setString(2, run);
        pStmt3.setInt(3, splNbr);
        pStmt3.setString(4, "1." + splNbr);
        pStmt3.setLong(5, spAnaId);
        pStmt3.setLong(6, bioSplId);
        pStmt3.setLong(7, qMQChId);
        pStmt3.setLong(8, rsmId);
        pStmt3.executeUpdate();
        keyRS = pStmt3.getGeneratedKeys();
        long qChId =0;
        if (keyRS.next()) {
          qChId = keyRS.getLong(1);
        } else {
          throw new SQLException("Quantitation Channel Id not found");
        }
        splNbr++;

        qChannels.add(new QuantChannel(qChId,splNbr,run,splNbr,rsmId, Option.empty(), Option.empty(), Option.empty()));
      }
    }
    return qChannels;
  }

  private long insertIntoMasterQChannel(Connection connection, long qDSId, long quantRsmId) throws SQLException {

    String sqlQuery;
    long qMQChId;
    sqlQuery = "INSERT INTO " + UdsDbMasterQuantChannelTable$.MODULE$.name() +
            " (" + UdsDbMasterQuantChannelColumns.QUANTITATION_ID() + "," + UdsDbMasterQuantChannelColumns.NAME() + "," + UdsDbMasterQuantChannelColumns.NUMBER() +
            ", "+UdsDbMasterQuantChannelColumns.QUANT_RESULT_SUMMARY_ID()
             + ") VALUES (?,?,?, ?) ";
    try(PreparedStatement pStmt = connection.prepareStatement(sqlQuery,  new String[] { "id" })) {
      pStmt.setLong(1, qDSId);
      pStmt.setString(2, "Quant " + m_diannResult.getName());
      pStmt.setInt(3, 1);
      pStmt.setLong(4, quantRsmId);
      pStmt.executeUpdate();
      java.sql.ResultSet keyRS = pStmt.getGeneratedKeys();
      if (keyRS.next()) {
        qMQChId = keyRS.getLong(1);
      } else {
        throw new SQLException("Quantitation masterQuantChannel Id not found");
      }
      return qMQChId;
    }
  }

  private long insertIntoBioSpl(Connection connection, long qDSId, long bioGrpId) throws SQLException {
    long bioSplId;
    String sqlQuery;
    int nbRuns = m_diannResult.getRuns().size();
    sqlQuery = "INSERT INTO " + UdsDbBiologicalSampleTable$.MODULE$.name() +
            " (" + UdsDbBiologicalSampleColumns.QUANTITATION_ID() + "," + UdsDbBiologicalSampleColumns.NAME() + "," + UdsDbBiologicalSampleColumns.NUMBER()
            + ") VALUES (?,?,?) ";
    try(PreparedStatement pStmt =  connection.prepareStatement(sqlQuery,  new String[] { "id" })) {
      pStmt.setLong(1, qDSId);
      pStmt.setString(2, "Group " + m_diannResult.getName() + nbRuns+" Runs");
      pStmt.setInt(3, 1);
      pStmt.executeUpdate();
      java.sql.ResultSet keyRS = pStmt.getGeneratedKeys();
      if (keyRS.next()) {
        bioSplId = keyRS.getLong(1);
      } else {
        throw new SQLException("Quantitation Biological Sample Id not found");
      }
    }

    sqlQuery = "INSERT INTO " + UdsDbBiologicalGroupBiologicalSampleItemTable$.MODULE$.name() +
            " (" + UdsDbBiologicalGroupBiologicalSampleItemColumns.BIOLOGICAL_GROUP_ID() + "," + UdsDbBiologicalGroupBiologicalSampleItemColumns.BIOLOGICAL_SAMPLE_ID()
            + ") VALUES (?,?) ";

    try(PreparedStatement pStmt = connection.prepareStatement(sqlQuery)) {
      pStmt.setLong(1, bioGrpId);
      pStmt.setLong(2, bioSplId);
      pStmt.executeUpdate();
      return bioSplId;
    }
  }

  private long insertIntoBioGrp(Connection connection, long qDSId, long grpSetupId) throws SQLException {

    long bioGrpId;
    String sqlQuery = "INSERT INTO " + UdsDbBiologicalGroupTable$.MODULE$.name() +
            " (" + UdsDbBiologicalGroupColumns.QUANTITATION_ID() + "," + UdsDbGroupSetupColumns.NAME() + "," + UdsDbBiologicalGroupColumns.NUMBER()
            + ") VALUES (?,?,?) ";
    try (PreparedStatement pStmt = connection.prepareStatement(sqlQuery,  new String[] { "id" })) {
      pStmt.setLong(1, qDSId);
      pStmt.setString(2, "Group " + m_diannResult.getName());
      pStmt.setInt(3, 1);
      pStmt.executeUpdate();
      java.sql.ResultSet keyRS = pStmt.getGeneratedKeys();
      if (keyRS.next()) {
        bioGrpId = keyRS.getLong(1);
      } else {
        throw new SQLException("Quantitation Biological Id not found");
      }
    }

    sqlQuery = "INSERT INTO " + UdsDbGroupSetupBiologicalGroupMapTable$.MODULE$.name() +
            " (" + UdsDbGroupSetupBiologicalGroupMapColumns.GROUP_SETUP_ID() + "," + UdsDbGroupSetupBiologicalGroupMapColumns.BIOLOGICAL_GROUP_ID()
            + ") VALUES (?,?) ";
    try(PreparedStatement pStmt = connection.prepareStatement(sqlQuery)) {
      pStmt.setLong(1, grpSetupId);
      pStmt.setLong(2, bioGrpId);
      pStmt.executeUpdate();
      return bioGrpId;
    }
  }

  private long insertIntoGrpSetupId(Connection connection, long qDSId) throws SQLException {
    long grpSetupId;
    String sqlQuery = "INSERT INTO " + UdsDbGroupSetupTable$.MODULE$.name() +
            " (" + UdsDbGroupSetupColumns.QUANTITATION_ID() + "," + UdsDbGroupSetupColumns.NAME() + "," + UdsDbGroupSetupColumns.NUMBER()
            + ") VALUES (?,?,?) ";
    try(PreparedStatement  pStmt = connection.prepareStatement(sqlQuery,  new String[] { "id" })) {
      pStmt.setLong(1, qDSId);
      pStmt.setString(2, m_diannResult.getName());
      pStmt.setInt(3, 1);
      pStmt.executeUpdate();
      java.sql.ResultSet keyRS = pStmt.getGeneratedKeys();
      if (keyRS.next()) {
        grpSetupId = keyRS.getLong(1);
      } else {
        throw new SQLException("Quantitation GroupSetup Id not found");
      }
      return grpSetupId;
    }
  }

  private long insertIntoDataset(Connection connection, Integer dsNbr, Long projectId) throws SQLException {
    try (Statement stmt = connection.createStatement();) {
      String methodSql = "Select id from " + UdsDbQuantMethodTable$.MODULE$.name() + " WHERE " + UdsDbQuantMethodColumns.TYPE() + " = 'label_free' and " + UdsDbQuantMethodColumns.ABUNDANCE_UNIT() + " = 'feature_intensity';";
      java.sql.ResultSet methodRs = stmt.executeQuery(methodSql);
      long methodId = 1; //VDS TODO exception if not found !
      if (methodRs.next()) {
        methodId = methodRs.getLong(1);
      } else {
        logger.warn("!!!! No quantitation method found for quantitation - USE ID 1 !!! ");
      }


      long qDSId = 0;
      String sqlQuery = "INSERT INTO " + UdsDbDataSetTable$.MODULE$.name() +
              " (" + UdsDbDataSetColumns.NUMBER() + "," + UdsDbDataSetColumns.NAME() + "," + UdsDbDataSetColumns.TYPE()
              + "," + UdsDbDataSetColumns.CREATION_TIMESTAMP() + "," + UdsDbDataSetColumns.PROJECT_ID() + "," + UdsDbDataSetColumns.QUANT_METHOD_ID()
              + ") VALUES (?,?,?,?,?,?) ";
      try (PreparedStatement pStmt = connection.prepareStatement(sqlQuery, new String[]{"id"})) {

        pStmt.setInt(1, dsNbr);
        pStmt.setString(2, m_diannResult.getName());
        pStmt.setString(3, "QUANTITATION");
        pStmt.setTimestamp(4, new Timestamp(new Date().getTime()));
        pStmt.setLong(5, projectId);
        pStmt.setLong(6, methodId);
        pStmt.executeUpdate();
        java.sql.ResultSet keyRS = pStmt.getGeneratedKeys();
        if (keyRS.next()) {
          qDSId = keyRS.getLong(1);
        } else {
          throw new SQLException("Quantitation dataset Id not found");
        }
        return qDSId;
      }
    }
  }


  private Map<Long, ResultSummary> validateResultSets() {
    Map<Long, ResultSummary> rsmByRsId =  new HashMap<>();
    for(ResultSet nextRs : m_allResultSetsByRun.values()) {
      logger.debug(" Will validate RS {} with {} peptides matches & {} proteins ", nextRs.id(), nextRs.peptideMatches().length, nextRs.proteinMatches().length);
      ValidationConfig config = new ValidationConfig(Option.empty(),Option.empty(),
              Option.empty(), BuildPeptideInstanceBuilder.apply("STANDARD"),
              Option.empty(), Option.empty(), Option.apply(PepSetScoring.MASCOT_STANDARD_SCORE()),
              Option.empty(),Option.empty());
      ResultSetValidator validator = new ResultSetValidator(m_executionContext, nextRs,Option.empty(),
              config, Option.apply(InferenceMethod.PARSIMONIOUS()), true, false, false);
      validator.runService();
      ResultSummary rsm = validator.validatedTargetRsm();
      logger.debug("Created RSM {} for RS {} with {} pepInst {} protSet  {} pepset ", rsm.id(), nextRs.id(), rsm.peptideInstances().length, rsm.proteinSets().length, rsm.peptideSets().length);

      rsmByRsId.put(nextRs.id(), rsm);
    }
    return rsmByRsId;
  }

}
