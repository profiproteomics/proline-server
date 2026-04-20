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

import java.util.ArrayList;
import java.util.List;

/**
 * Represent a Precursor, Ion, which has been identified (and quantified) in at least one Run
 *
 */
public class Precursor {

  // Ions properties
  String m_precursorId; // Sequence+Modif+Charge
  String m_modifiedSequence;
  String m_sequence;
  Integer m_charge;
  Integer m_libIndex;
  Float m_moz;
  Boolean m_proteotypique;

  // Matching proteins
  String m_proteinGroup;
  List<String> m_proteinIds;

  List<DiaNNPepModification> m_ptmDescription;
  List<QuantPrecursor> m_quantPrecursors;
  List<String> m_quantifiedRuns;

  public Precursor(String precId, String sequence, String modifiedSequence, Integer charge, Float moz, String proteinGroup, List<String> proteinIds, Integer libIndex, Boolean proteotypique) {
    this.m_sequence = sequence;
    this.m_charge = charge;
    this.m_libIndex = libIndex;
    this.m_modifiedSequence = modifiedSequence;
    this.m_moz = moz;
    this.m_precursorId = precId;
    this.m_proteinGroup = proteinGroup;
    this.m_proteinIds = proteinIds;
    this.m_proteotypique = proteotypique;
  }

  public String getPrecursorId() {
    return m_precursorId;
  }

  public String getModifiedSequence() {
    return m_modifiedSequence;
  }

  public boolean isModified(){
    return !m_sequence.equals(m_modifiedSequence);
  }

  public List<DiaNNPepModification> getModifications(){
    if(m_ptmDescription == null){
      m_ptmDescription= new ArrayList<DiaNNPepModification>();
      if(isModified()){
        m_ptmDescription = DiaNNPepModification.parseModification(m_modifiedSequence);
      }
    }
    return m_ptmDescription;
  }

  public String getSequence() {
    return m_sequence;
  }

  public Integer getCharge() {
    return m_charge;
  }

  public Float getMoz() {
    return m_moz;
  }

  public String getProteinGroup() {
    return m_proteinGroup;
  }

  public List<String> getProteinIds() {
    return m_proteinIds;
  }

  public List<DiaNNPepModification> getPtmDescription() {
    return m_ptmDescription;
  }

  public List<QuantPrecursor> getQuantPrecursors() {
    return m_quantPrecursors;
  }

  public void addQuantPrecursors(QuantPrecursor quantPrecursor) {
    if(this.m_quantPrecursors == null)
      m_quantPrecursors = new ArrayList<>();
    if(m_quantifiedRuns == null)
      m_quantifiedRuns = new ArrayList<>();
    m_quantPrecursors.add(quantPrecursor);
    if(!m_quantifiedRuns.contains(quantPrecursor.m_runName))
      m_quantifiedRuns.add(quantPrecursor.m_runName);
  }

  public List<String> getQuantifiedRuns() {
    return m_quantifiedRuns;
  }

  public QuantPrecursor getQuantitationForRun(String run){
    QuantPrecursor quantPrecursor = null;
    if(m_quantPrecursors!=null) {
      List<QuantPrecursor> runQuantPrecursors = m_quantPrecursors.stream().filter(qp -> qp.getRunName().equals(run)).toList();
      if(!runQuantPrecursors.isEmpty()){
        quantPrecursor = runQuantPrecursors.get(0);
      }
    }
    return quantPrecursor;
  }

  public Boolean isProteotypique() {
    return m_proteotypique;
  }

  public Integer getLibIndex() {
      return m_libIndex;
  }

}
