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

package fr.proline.module.paser.diann.model;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DiaNNResult {

  private static final Logger logger = LoggerFactory.getLogger(DiaNNResult.class);

  Map<String, List<Precursor>> m_precursorByRun;
  Map<String, Precursor> m_allPrecursorsById;
  List<String> m_runs;
  Map<String, List<QuantProteinGroup>> m_protgroupsByRun;
  String m_name;

  public DiaNNResult(String name) {
    m_precursorByRun = new HashMap<>();
    m_allPrecursorsById = new HashMap<>();
    m_protgroupsByRun = new HashMap<>();
    m_runs = new ArrayList<>();
    m_name = name;
  }

  public  void setRuns(List<String> runs) {
    this.m_runs = runs;
  }

  public List<Precursor> getAllPrecursor()  {
    return m_allPrecursorsById.values().stream().toList();
  }

  public Precursor getPrecursorForId(String precId) {
    return m_allPrecursorsById.get(precId);
  }

  public List<Precursor> getPrecursorForRun(String run) {
    if(m_precursorByRun.isEmpty())
      initPrecursorsBuyRun();
    return m_precursorByRun.get(run);
  }

  public List<QuantProteinGroup> getProteinGroupsForRun(String run) {
    return m_protgroupsByRun.get(run);
  }

  public List<String> getRuns() {
    return m_runs;
  }

  public String getName() {
    return m_name;
  }

  public void addPrecursors(Precursor precursor) {
    m_allPrecursorsById.put(precursor.m_precursorId, precursor);
  }

  public void setPrecursors(Map<String, Precursor> precursors) {
    m_allPrecursorsById = precursors;
    m_precursorByRun.clear();
  }

  private void initPrecursorsBuyRun(){
    for(Precursor precursor : m_allPrecursorsById.values()){
      List<QuantPrecursor> qprec = precursor.getQuantPrecursors();
      if(qprec == null || qprec.isEmpty())
        continue;

      for(QuantPrecursor qprecursor : qprec){
        String runName = qprecursor.getRunName();
        m_precursorByRun.computeIfAbsent(runName, k -> new ArrayList<>()).add(precursor);
      }
    }
  }

//  public void setPrecursorForRun(List<Precursor> precursor, String run) {
//    m_precursorByRun.put(run, precursor);
//  }

  public void setProteinGroupsForRun(List<QuantProteinGroup> proteinGroups, String run) {
    m_protgroupsByRun.put(run, proteinGroups);
  }

  public void printStatForRun(String run) {
    List<Precursor> precursors = getPrecursorForRun(run);
    if (precursors == null) {
      logger.info("No precursors found for run: {}", run);
      return;
    }
    logger.info("Run: {}, Precursor count: {}", run, precursors.size());

    List<QuantProteinGroup> pgs = m_protgroupsByRun.get(run);
    if(pgs == null) {
      logger.info("No protein groups found for run: {}", run);
      return;
    }
    logger.info("Run: {}, Protein group count: {}", run, pgs.size());
  }

  public void printStat(){
    List<String> runs = getRuns();
    for(String run : runs) {
      logger.info("--- Get stat information for  run: {}", run);
      printStatForRun(run);
    }
  }

}
