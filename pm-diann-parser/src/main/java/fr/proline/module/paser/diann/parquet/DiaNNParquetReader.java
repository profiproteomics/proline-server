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

package fr.proline.module.paser.diann.parquet;

import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.model.Fragment;
import fr.proline.module.paser.diann.model.Precursor;
import fr.proline.module.paser.diann.model.QuantPrecursor;
import fr.proline.module.paser.diann.model.QuantProteinGroup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DiaNNParquetReader {

  static Logger logger = LoggerFactory.getLogger(DiaNNParquetReader.class);
  private final File m_reportFile;
  private final DiaNNResult.FilterMode m_resultFilteringMode;
  private String m_cutValue;
  private final Set<String> cleavagePairs = new HashSet<>();
  private final Set<String> blockedPairs = new HashSet<>();
  private static float m_pgQValThreshold = 0.01f;
  public DiaNNParquetReader(File reportFile) {
    this(reportFile, DiaNNResult.FilterMode.NONE, null);
  }

  public DiaNNParquetReader(File reportFile, DiaNNResult.FilterMode filterResultMode, String cutValue ) {
    m_reportFile = reportFile;
    m_resultFilteringMode = filterResultMode;
    m_cutValue = cutValue;
    extractMissCleavedRules(m_cutValue);
  }

  /*
  For test purpose only
   */
  protected void setCutValue(String cutValue) {
    m_cutValue = cutValue;
    extractMissCleavedRules(m_cutValue);
  }

  public  void  readRunsInfo() throws SQLException {

    String sql = "SELECT Run, COUNT(*) as cnt FROM '"+m_reportFile.getAbsoluteFile()+"' GROUP BY Run";

    try (
         Connection conn = DriverManager.getConnection("jdbc:duckdb:");
         Statement stmt = conn.createStatement();
         ResultSet rs = stmt.executeQuery(sql)
        ) {

        logger.info(" - Processing file: {}", m_reportFile.getName());
        while (rs.next()) {
          String run = rs.getString("Run");
          int count = rs.getInt("cnt");
          logger.info(" Run: {} contains {} entries", run, count);
        }

    }
  }

  public DiaNNResult readData() throws SQLException {
    Map<String, Integer> runs = new HashMap<>();
    String sql = "SELECT Run, COUNT(*) as countPrec FROM '"+m_reportFile.getAbsoluteFile()+"' GROUP BY Run";
    DiaNNResult diaNNResult = new DiaNNResult(m_reportFile.getParentFile().getName());
    Map<String, Map<String, String>> statByRuns = new HashMap<>();
    try (
         Connection conn = DriverManager.getConnection("jdbc:duckdb:");
         Statement stmt = conn.createStatement()
        ) {

      try (ResultSet rs = stmt.executeQuery(sql)) {
        logger.info(" Reading data from diaNN report  : {}", m_reportFile.getParent());
        while (rs.next()) {
          String run = rs.getString("Run");
          int count = rs.getInt("countPrec");
          runs.put(run, count);
          logger.debug("Run: {} contains {} precursors ", run, count);
        }
      }

      diaNNResult.setRuns(runs.keySet().stream().toList());

      for (String run : runs.keySet()) {
        //Read and Create data for 1 Run :  Proline RS/RSM
        sql = "select * from '" + m_reportFile.getAbsoluteFile() + "' where Run = '" + run + "'";
        switch (m_resultFilteringMode) {
          case MBR: {
            sql = sql + " and \"Lib.PG.Q.Value\" < " + m_pgQValThreshold + " and \"Q.Value\" < " + m_pgQValThreshold + " and \"Lib.Q.Value\" < " + m_pgQValThreshold;
            break;
          }
          case NONE: {
            break;
          }
          case NOMBR: {
            sql = sql + " and \"Global.PG.Q.Value\" < " + m_pgQValThreshold + " and \"Q.Value\" < " + m_pgQValThreshold + " and \"Global.Q.Value\" < " + m_pgQValThreshold;
            break;
          }
        }

        logger.info("use SQL " + sql);
        try (ResultSet rs = stmt.executeQuery(sql)) {
          statByRuns.put(run, readRunData(run, rs, diaNNResult));
        }
      } // End for each Run
    } // End try for connection & Statement

    // for tests only TODO remove it
    for(Map.Entry<String, Map<String, String>> runStat : statByRuns.entrySet()){
      logger.debug(" Run {}", runStat.getKey());
      for(Map.Entry<String, String> stat : runStat.getValue().entrySet()){
        logger.debug(" Stat {} = {}", stat.getKey(), stat.getValue());
      }
    }
//    diaNNResult.printStat();
    return diaNNResult;
  }

  private Map<String,String> readRunData(String run, ResultSet rs, DiaNNResult diaNNResult) throws SQLException {
    logger.debug(" -------- Read Data for {}", run);
    List<QuantPrecursor> precursors = new ArrayList<>();
    Map<String,QuantProteinGroup> proteinGroupByName = new HashMap<>();
    boolean hasFragments = containsColumn("Fr.0.Index", rs);
    int nbCreatedPrec = 0;
    int nbExistingPrec = 0;
    int nbPGAddedProt = 0;
    int nbPGPropDifferent = 0;
    int nbDiffIdAndName = 0;
    int nbDiffIdAndNameNotConta = 0;
    while (rs.next()) {
      String precursor = rs.getString("Precursor.Id");
      String modifSeq = rs.getString("Modified.Sequence");
      String sequence = rs.getString("Stripped.Sequence");
      Integer missCleaved = computeMissCleaved(sequence);
      Integer charge = rs.getInt("Precursor.Charge");
      Float precMoz = rs.getFloat("Precursor.Mz");
      Float predictedRT = rs.getFloat("Predicted.RT");
      Float precRT = rs.getFloat("RT");
      Float precRTStart = rs.getFloat("RT.Start");
      Float precRTStop = rs.getFloat("RT.Stop");
      Double precQuant = rs.getDouble("Precursor.Quantity");
      String geneName = rs.getString("Genes");

      String proteinGroupAsStr = rs.getString("Protein.Group");
      List<String> allProteinsInGroup = new ArrayList<>();
      if (proteinGroupAsStr != null && !proteinGroupAsStr.isEmpty()) {
        allProteinsInGroup.addAll(List.of(proteinGroupAsStr.split(";")));
      }

      String proteinIdsAsStr = rs.getString("Protein.Ids");
      List<String> allProteinIds = new ArrayList<>();
      if (proteinIdsAsStr != null && !proteinIdsAsStr.isEmpty()) {
        allProteinIds.addAll(List.of(proteinIdsAsStr.split(";")));
      }

      String proteinNamesAsStr = rs.getString("Protein.Names");
      List<String> allProteinsNames = new ArrayList<>();
      if (proteinNamesAsStr != null && !proteinNamesAsStr.isEmpty()) {
        allProteinsNames.addAll(List.of(proteinNamesAsStr.split(";")));
      }

      if (allProteinsNames.size() != allProteinsInGroup.size()) {
        nbDiffIdAndName++;
        if(!allProteinsInGroup.get(0).startsWith("#C#")) {
          nbDiffIdAndNameNotConta++;
        }
//        logger.warn("Not same number of entries in protein groups and protein names ! No names will be saved for {}", allProteinsInGroup);
        allProteinsNames = new ArrayList<>();
      }

      Integer libIndex = rs.getInt("Precursor.Lib.Index");
      Double evidence = rs.getDouble("Evidence");
      Double massEvidence = rs.getDouble("Mass.Evidence");
      Boolean proteotypique = rs.getInt("Proteotypic") == 1;
      Double quantQuality = rs.getDouble("Quantity.Quality");
      Double qValue = rs.getDouble("Q.Value");

      Double pgQuant = 0.0d;
      if(containsColumn("PG.Normalised", rs))
        pgQuant = rs.getDouble("PG.Normalised");
      else if(containsColumn("PG.TopN", rs)  )
        pgQuant = rs.getDouble("PG.Normalised");
      Double pgMaxLFQ = rs.getDouble("PG.MaxLFQ");
      Double pgQValue = rs.getDouble("PG.Q.Value");

      List<Fragment> fragments = new ArrayList<>();
      if (hasFragments) {
        for (int i = 0; i < 12; i++) {
          String fragFullId = rs.getString("Fr." + i + ".Id");
          String[] fragIdpart = fragFullId.split("/");
          String fragId = fragFullId;
          double fragMoz = 0.0f;
          if (fragIdpart.length == 2) {
            fragId = fragIdpart[0];
            fragMoz = Double.parseDouble(fragIdpart[1]);
          }
          Float fragQ = rs.getFloat("Fr." + i + ".Quantity");
          Double fragSc = rs.getDouble("Fr." + i + ".Score");
          fragments.add(new Fragment(fragId, fragMoz, fragQ, fragSc));
        }
      }

      //Create read Precursor and associated QuantPrecursor
      String protGroupKey = allProteinsInGroup.stream().min(String::compareTo).orElse(null);
      QuantPrecursor qPrec = new QuantPrecursor(precursor,run,precRT, precRTStart, precRTStop, predictedRT,
              precQuant, fragments, evidence, massEvidence, quantQuality, qValue, missCleaved);
      Precursor currentPrec = diaNNResult.getPrecursorForId(precursor);
      if(currentPrec == null) {
        currentPrec = new Precursor(precursor,sequence, modifSeq, charge, precMoz, protGroupKey, allProteinIds, libIndex, proteotypique);
        diaNNResult.addPrecursors(currentPrec);
        nbCreatedPrec++;
      } else
        nbExistingPrec++;

      //Create QValues Map
      Map<String, Double> qValuesMap = new HashMap<>();
      if(geneName != null && !geneName.isEmpty())
        qValuesMap.put("GG.Q.Value_"+geneName, rs.getDouble("GG.Q.Value"));
      qValuesMap.put("Global.PG.Q.Value_"+protGroupKey, rs.getDouble("Global.PG.Q.Value"));
      qValuesMap.put("Lib.PG.Q.Value_"+protGroupKey, rs.getDouble("Lib.PG.Q.Value"));
      qValuesMap.put("PG.Q.Value_"+protGroupKey, pgQValue);
      qValuesMap.put("Protein.Q.Value_"+protGroupKey, rs.getDouble("Protein.Q.Value"));
      qValuesMap.put("Global.Q.Value", rs.getDouble("Global.Q.Value"));
      qValuesMap.put("Lib.Q.Value", rs.getDouble("Lib.Q.Value"));
      qValuesMap.put("Q.Value", qValue);
      qPrec.setQValues(qValuesMap);

      currentPrec.addQuantPrecursors(qPrec);
      precursors.add(qPrec);

      //Create read QuantProteinGroup
      if(!proteinGroupByName.containsKey(protGroupKey)) {
        QuantProteinGroup quantProteinGroup = new QuantProteinGroup(protGroupKey, run, allProteinIds, allProteinsInGroup, allProteinsNames, pgQuant, pgMaxLFQ, pgQValue);
        proteinGroupByName.put(protGroupKey, quantProteinGroup);
      } else {
        // Already found. Just complete and verify same values !
        QuantProteinGroup quantProteinGroup = proteinGroupByName.get(protGroupKey);
        int nbAdded = quantProteinGroup.addProteinIds(allProteinIds);
        QuantProteinGroup fakeProteinGroup = new QuantProteinGroup(protGroupKey, run, allProteinIds, allProteinsInGroup, allProteinsNames, pgQuant, pgMaxLFQ, pgQValue);
        String isEqual = quantProteinGroup.compare(fakeProteinGroup);
        if(!isEqual.equals("OK") || nbAdded != 0) {
          if(nbAdded>0)
            nbPGAddedProt++;
          else
            nbPGPropDifferent++;
            //logger.info(" Found DIFFERENT {} - Added {} prot Id  - Are NOT Eq {} ", protGroupKey, nbAdded, isEqual);
        }
      }

      /* // Test only
         if (run.equals("Asc_004865"))
           System.out.println(precursor + "\t" + modifSeq + "\t" + charge + "\t" + libIndex + "\t" + precMoz + "\t" + precRT + "\t" + precQuant);
      */

    } // End go through precursors

//    logger.debug(" -- Found {} protein group - needed to just add protId {} times ", proteinGroupByName.size(), nbPGAddedProt);
//    diaNNResult.setPrecursorForRun(precursors, run);
    diaNNResult.setProteinGroupsForRun(proteinGroupByName.values().stream().toList(), run);

    Map<String,String> result = new HashMap<>();
    result.put("nb Created precursor for this run ", String.valueOf(nbCreatedPrec));
    result.put("nb added quantPrecursor in existing precursor for this run ", String.valueOf(nbExistingPrec));
    result.put("nb Added ProtId in ProtGrp ", String.valueOf(nbPGAddedProt));
    result.put("nb PG with different Properties ", String.valueOf(nbPGPropDifferent));
    result.put("nb PG with Diff in ProtId and ProtName ", String.valueOf(nbDiffIdAndName));
    result.put("nb PG with Diff in ProtId and ProtName & not conta ", String.valueOf(nbDiffIdAndNameNotConta));
    return result;
  }

  private boolean containsColumn(String columnName, ResultSet rs) throws SQLException {
    ResultSetMetaData rsmd  =rs.getMetaData();
    int nbCol = rsmd.getColumnCount();
    for(int i=1;i<=nbCol;i++){
      if(rsmd.getCatalogName(i).equals(columnName))
        return true;
    }
    return false;
  }

  private void extractMissCleavedRules(String cutValue){
    if ( cutValue == null || cutValue.isBlank()) {
      return;
    }

    String[] rules = cutValue.split(",");
    for (String rule : rules) {
      if (rule == null) {
        continue;
      }

      String cleanedRule = rule.trim();
      if (cleanedRule.isEmpty()) {
        continue;
      }

      boolean isBlocked = cleanedRule.startsWith("!");
      if (isBlocked) {
        cleanedRule = cleanedRule.substring(1).trim();
      }

      if (cleanedRule.length() != 2) {
        continue;
      }

      if (isBlocked) {
        blockedPairs.add(cleanedRule);
      } else {
        cleavagePairs.add(cleanedRule);
      }
    }
  }

  protected Integer computeMissCleaved(String sequence) {

    if (sequence == null || sequence.length() < 2 || (blockedPairs.isEmpty() &&  cleavagePairs.isEmpty()) ) {
      return 0;
    }

    int cleavageSites = 0;
    for (int i = 0; i < sequence.length() - 1; i++) {
      String pair = sequence.substring(i, i + 2);
      if (matchesPair(cleavagePairs, pair) && !matchesPair(blockedPairs, pair)) {
        cleavageSites++;
      }
    }

    return cleavageSites;
  }

  private static boolean matchesPair(Set<String> patterns, String pair) {
    for (String pattern : patterns) {
      if (pattern.length() != 2) {
        continue;
      }

      char left = pattern.charAt(0);
      char right = pattern.charAt(1);
      boolean leftMatch = left == '*' || left == pair.charAt(0);
      boolean rightMatch = right == '*' || right == pair.charAt(1);
      if (leftMatch && rightMatch) {
        return true;
      }
    }
    return false;
  }
}
