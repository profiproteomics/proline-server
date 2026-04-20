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

public class Fragment {
  String id;
  Double moz;

  Float quantAb;
  Double score;

  public Fragment(String id, Double moz, Float quantAb, Double score) {
    this.id = id;
    this.moz = moz;
    this.quantAb = quantAb;
    this.score = score;
  }

  public String getId() {
    return id;
  }

  public Double getMoz() {
    return moz;
  }

  public Float getQuantAb() {
    return quantAb;
  }

  public Double getScore() {
    return score;
  }
}
