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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class QuantProteinGroup {
  static Logger logger = LoggerFactory.getLogger(QuantProteinGroup.class);
  private static final double EPSILON = 1e-10;

  String m_groupId;
  String m_runName;

  List<String> m_allProteinsInGroup;
  List<String> m_allProteinsNames;
  Set<String> m_proteinIds;

  Double m_pgAbundance;
  Double m_pgMaxLFQ;
  Double m_qVlaue;

  public QuantProteinGroup(String groupId, String runName, List<String> proteinIds, List<String> allProteinInGroup, List<String> allProteinNames, Double protGroupAbundance, Double protGroupMaxLFQ, Double protGroupQValue) {
    this.m_runName = runName;
    this.m_qVlaue = protGroupQValue;
    this.m_pgAbundance = protGroupAbundance;
    this.m_pgMaxLFQ = protGroupMaxLFQ;
    this.m_proteinIds = new HashSet<>();
    m_proteinIds.addAll(proteinIds);
    this.m_groupId = groupId;
    this.m_allProteinsNames = allProteinNames;
    this.m_allProteinsInGroup = allProteinInGroup;
  }

  public int addProteinIds(List<String> proteinIds) {
    if(m_proteinIds == null) {
      m_proteinIds = new HashSet<>();
    }
    int sizeBefore = m_proteinIds.size();
    m_proteinIds.addAll(proteinIds);
    return m_proteinIds.size() - sizeBefore;
  }

  public String compare(QuantProteinGroup group) {
    if (group == null) {
      return "null";
    }

    // Compare run name
    if (m_runName == null ? group.m_runName != null : !m_runName.equals(group.m_runName)) {
      logger.warn(" NOT SAME Run {} vs {}", m_runName, group.m_runName);
      return "runName";
    }

    // Compare Double values using epsilon
    if (!compareDoubles(m_pgAbundance, group.m_pgAbundance)) {
      logger.warn(" NOT SAME Abundance for {}", m_groupId);
      return "pgAbundance";
    }
    if (!compareDoubles(m_pgMaxLFQ, group.m_pgMaxLFQ)) {
      logger.warn(" NOT SAME MaxLFQ for {}", m_groupId);
      return "MaxLFQ";
    }
    if (!compareDoubles(m_qVlaue, group.m_qVlaue)) {
      logger.warn(" NOT SAME Vlaue for {}", m_groupId);
      return "qValue";
    }

    // Compare Lists
    if (!compareLists(m_allProteinsInGroup, group.m_allProteinsInGroup)) {
      logger.warn(" NOT SAME ProteinsInGroup for {}", m_groupId);
      return "AllProtInGrp";
    }

    if (!compareLists(m_allProteinsNames, group.m_allProteinsNames)) {
      logger.warn(" NOT SAME ProteinNames for {}", m_groupId);
      return "ProtNames";
    }
//    if (!compareLists(m_proteinIds, group.m_proteinIds)) {
//      logger.warn(" NOT SAME ProteinIds for {}", m_groupId);
//      return false;
//    }

    return "OK";
  }

  private boolean compareDoubles(Double d1, Double d2) {
    if (d1 == null && d2 == null) {
      return true;
    }
    if (d1 == null || d2 == null) {
      return false;
    }
    return Math.abs(d1 - d2) < EPSILON;
  }

  private boolean compareLists(List<String> list1, List<String> list2) {
    if (list1 == null && list2 == null) {
      return true;
    }
    if (list1 == null || list2 == null) {
      return false;
    }
    return list1.equals(list2);
  }


}
