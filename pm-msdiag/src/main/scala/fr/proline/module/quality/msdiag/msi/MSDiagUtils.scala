package fr.proline.module.quality.msdiag.msi

import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.PeptideMatch

object MSDiagUtils {

  def getScoreLabel(minScore: Float, maxScore: Float): String = {
    if (minScore == Float.NaN && maxScore == Float.NaN) "Unassigned spectra"
    else if (minScore.isNaN && !maxScore.isNaN) "Score <= " + maxScore
    else if (minScore.isNaN && !maxScore.isNaN) "Score <= " + maxScore
    else if (!minScore.isNaN && !maxScore.isNaN) minScore + " < score <= " + maxScore
    else "Score > " + minScore
  }

  def getCharges(rs: MSDiagResultSetManager): Array[Int] = {
    val charges = new ArrayBuffer[Int]()
    val query = rs.getAllMsQueries.maxBy(_.charge)
    if (query != null) { // create an array from 1 to higher charge
      for (c <- 1 to query.charge) charges += c
    }
    if (charges.length == 0) throw new Exception("No charges found")
    charges.toArray
  }
  
  def countMatchesPerScore(peptideMatches: Array[PeptideMatch], minScore: Float, maxScore: Float): Int = {
    if (peptideMatches == null || peptideMatches.size == 0) 0
    else if (minScore == Float.NaN && maxScore != Float.NaN) peptideMatches.count(pm => pm.score <= maxScore)
    else if (minScore != Float.NaN && maxScore != Float.NaN) peptideMatches.count(pm => minScore < pm.score && pm.score <= maxScore)
    else if (minScore != Float.NaN && maxScore == Float.NaN) peptideMatches.count(pm => pm.score > minScore)
    else peptideMatches.size // should not happen (means that both scores are NaN)
  }
  
}