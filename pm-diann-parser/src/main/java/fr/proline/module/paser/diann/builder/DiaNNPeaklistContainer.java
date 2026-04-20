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

import fr.proline.core.om.model.msi.IPeaklistContainer;
import fr.proline.core.om.model.msi.Ms2Query;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.Spectrum;
import scala.Function1;
import scala.runtime.BoxedUnit;

import java.util.Map;

public class DiaNNPeaklistContainer implements IPeaklistContainer {

  ResultSet m_resultSet;
  Map<Long,Spectrum> m_spectraById;

  public DiaNNPeaklistContainer(ResultSet rs, Map<Long,Spectrum> spectraById) {
    m_resultSet = rs;
    m_spectraById = spectraById;
  }

  @Override
  public void eachSpectrum(Function1<Spectrum, BoxedUnit> onEachSpectrum) {
    PeptideMatch[] allpepMatches = m_resultSet.peptideMatches();
    for(PeptideMatch peptideMatch : allpepMatches) {
      Ms2Query q =peptideMatch.getMs2Query();
      onEachSpectrum.apply(m_spectraById.get(q.spectrumId()));
    }
  }
}
