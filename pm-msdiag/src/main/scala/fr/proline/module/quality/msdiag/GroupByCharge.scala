package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi._
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet

object GroupByCharge extends Logging {

  def sortByScoreAndCharge(rs: MSDiagResultSetManager, scores: Array[Float]): Array[Array[MSDiagChargeMatch]] = {
    if (scores.length == 0) throw new Exception("Score window is empty")
    val charges: Array[Int] = getCharges(rs)
    val MSDiagMatches = Array.ofDim[MSDiagChargeMatch](charges.length, scores.length + 1)
    // look for each charge
    for(c <- 0 until charges.length) {
      // for each score with this charge
      for (i <- 0 to scores.length) {
        val _minScore = if (i == 0) Float.NaN else scores(i - 1)
        val _maxScore = if (i == scores.length) Float.NaN else scores(i)
        MSDiagMatches(c)(i) = new MSDiagChargeMatch(charge = charges(c),
          minScore = _minScore,
          maxScore = _maxScore,
          targetMatches = filterByChargeAndScore(rs.getPeptideMatches(false), charges(c), _minScore, _maxScore),
          decoyMatches = filterByChargeAndScore(rs.getPeptideMatches(true), charges(c), _minScore, _maxScore))
      }
      // also look for unassigned spectra with this charge
      // TODO 
    }
    MSDiagMatches
  }
  
  def getCharges(rs: MSDiagResultSetManager): Array[Int] = rs.getAllPeptideMatches.groupBy(pm => pm.msQuery.charge).keys.toList.sorted.toArray

  private def filterByChargeAndScore(peptideMatches: Array[PeptideMatch], charge: Int, minScore: Float, maxScore: Float): Array[PeptideMatch] = {
    if (peptideMatches == null) null // may happen when there is no decoy RS
    else if (minScore == Float.NaN && maxScore != Float.NaN) peptideMatches.filter(pm => pm.msQuery.charge == charge && pm.score <= maxScore)
    else if (minScore != Float.NaN && maxScore != Float.NaN) peptideMatches.filter(pm => pm.msQuery.charge == charge && minScore < pm.score && pm.score <= maxScore)
    else if (minScore != Float.NaN && maxScore == Float.NaN) peptideMatches.filter(pm => pm.msQuery.charge == charge && pm.score > minScore)
    else peptideMatches.filter(pm => pm.msQuery.charge == charge) // should not happen (means that both scores are NaN)
  }
}

