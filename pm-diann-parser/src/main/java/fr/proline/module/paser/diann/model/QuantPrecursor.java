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

import fr.proline.module.paser.diann.parquet.DiaNNParquetReader;

import java.util.ArrayList;
import java.util.List;

public class QuantPrecursor {
  String m_precursorId;

  String m_runName;

  Float m_rt;
  Float m_rtStart;
  Float m_rtStop;
  Float m_predictedRT;


  // Quantitation - quality properties
  Double m_abundance;
  Double m_quantQuality;
  Double m_qValue;
  Double m_evidence;
  Double m_massEvidence;

  List<Fragment> m_fragments;

  public QuantPrecursor(String precId, String runName, Float rt, Float rtStart, Float rtStop, Float predictedRT, Double quantiAb,
                        List<Fragment> fragments, Double evidence, Double massEvidence, Double quality, Double qValue) {
    this.m_evidence = evidence;
    this.m_fragments = fragments;
    this.m_massEvidence = massEvidence;
    this.m_precursorId = precId;
    this.m_predictedRT = predictedRT;
    this.m_quantQuality = quality;
    this.m_abundance = quantiAb;
    this.m_qValue = qValue;
    this.m_rt = rt;
    this.m_rtStart = rtStart;
    this.m_rtStop = rtStop;
    this.m_runName = runName;
  }

  public Double getAbundance() {
    return m_abundance;
  }

  public void setAbundance(Double abundance) {
    this.m_abundance = abundance;
  }

  public Double getEvidence() {
    return m_evidence;
  }

  public void setEvidence(Double evidence) {
    this.m_evidence = evidence;
  }

  public List<Fragment> getFragments() {
    return m_fragments;
  }

  public void setFragments(List<Fragment> fragments) {
    this.m_fragments = fragments;
  }

  public Double getMassEvidence() {
    return m_massEvidence;
  }

  public void setMassEvidence(Double massEvidence) {
    this.m_massEvidence = massEvidence;
  }

  public String getPrecursorId() {
    return m_precursorId;
  }

  public void setPrecursorId(String precursorId) {
    this.m_precursorId = precursorId;
  }

  public Float getPredictedRT() {
    return m_predictedRT;
  }

  public void setPredictedRT(Float predictedRT) {
    this.m_predictedRT = predictedRT;
  }

  public Double getQuantQuality() {
    return m_quantQuality;
  }

  public void setQuantQuality(Double quantQuality) {
    this.m_quantQuality = quantQuality;
  }

  public Double getQValue() {
    return m_qValue;
  }

  public void setQValue(Double qValue) {
    this.m_qValue = qValue;
  }

  public Float getRt() {
    return m_rt;
  }

  public Float getRtStart() {
    return m_rtStart;
  }

  public Float getRtStop() {
    return m_rtStop;
  }

  public void setRt(Float rt) {
    this.m_rt = rt;
  }

  public String getRunName() {
    return m_runName;
  }

  public void setRunName(String runName) {
    this.m_runName = runName;
  }

}
