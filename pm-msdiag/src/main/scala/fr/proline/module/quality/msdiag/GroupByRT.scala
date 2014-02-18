package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi._
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.SpectrumTitleFields
import fr.proline.core.om.model.msi.SpectrumTitleParsingRule
import fr.proline.util.primitives.toFloat
import scala.math.floor
import scala.math.ceil

object GroupByRT extends Logging {

  def sortByScoreAndRT(rs: MSDiagResultSetManager, scores: Array[Float], parsingRules: Option[SpectrumTitleParsingRule]): Array[Array[MSDiagRTMatch]] = {
    if (parsingRules.isDefined) {
      val retentionTimes: Array[Int] = getRetentionTimes(rs, parsingRules)
      if (retentionTimes.length > 0) {
        val rtMin = retentionTimes(0)
        val rtMax = retentionTimes(retentionTimes.length - 1)
        val MSDiagMatches = Array.ofDim[MSDiagRTMatch](rtMax-rtMin+1, scores.length+1) // goes from first to last and includes missing rt (if any)
        // look for each "retention time in minute"
        for (rt <- MSDiagMatches.indices) {
          // for each score with this charge
          for (i <- 0 to scores.length) {
            val _minScore = if (i == 0) Float.NaN else scores(i - 1)
            val _maxScore = if (i == scores.length) Float.NaN else scores(i)
            MSDiagMatches(rt)(i) = new MSDiagRTMatch(rtmn = rt, minScore = _minScore, maxScore = _maxScore,
              targetMatches = filterByRTAndScore(rs, parsingRules, false, retentionTimes(rt), _minScore, _maxScore),
              decoyMatches = filterByRTAndScore(rs, parsingRules, true, retentionTimes(rt), _minScore, _maxScore))
          }
          // also look for unassigned spectra with this charge
          // TODO 
        }
        MSDiagMatches
      } else null
    } else null
  }

  private def getRetentionTimes(rs: MSDiagResultSetManager, parsingRules: Option[SpectrumTitleParsingRule]): Array[Int] = rs.getAllPeptideMatches.groupBy(pm => getRTInMinuteFromSpectrumTitle(parsingRules, pm.getMs2Query.spectrumTitle)).keys.toArray.sorted

  private def getRTInMinuteFromSpectrumTitle(parsingRules: Option[SpectrumTitleParsingRule], spectrumTitle: String): Int = {
    val specTitleFieldMap = parsingRules.map(_.parseTitle(spectrumTitle)).getOrElse(Map.empty[SpectrumTitleFields.Value, String])
    val firstTime = toFloatOrZero(specTitleFieldMap.getOrElse(SpectrumTitleFields.FIRST_TIME, 0f))
    val lastTime = toFloatOrZero(specTitleFieldMap.getOrElse(SpectrumTitleFields.LAST_TIME, 0f))
    // what should be returned ? in a mgf both values are the same
    ceil(firstTime).toInt
  }
  private def toFloatOrZero(v: Any): Float = try { toFloat(v) } catch { case e: Throwable => 0f }

  private def filterByRTAndScore(rs: MSDiagResultSetManager, parsingRules: Option[SpectrumTitleParsingRule], wantDecoy: Boolean, rt: Int, minScore: Float, maxScore: Float): Array[PeptideMatch] = {
    val peptideMatches = rs.getPeptideMatches(wantDecoy)
    if (peptideMatches == null) null // may happen when there is no decoy RS
    else if (minScore == Float.NaN && maxScore != Float.NaN) peptideMatches.filter(pm => getRTInMinuteFromSpectrumTitle(parsingRules, pm.getMs2Query.spectrumTitle) == rt && pm.score <= maxScore)
    else if (minScore != Float.NaN && maxScore != Float.NaN) peptideMatches.filter(pm => getRTInMinuteFromSpectrumTitle(parsingRules, pm.getMs2Query.spectrumTitle) == rt && minScore < pm.score && pm.score <= maxScore)
    else if (minScore != Float.NaN && maxScore == Float.NaN) peptideMatches.filter(pm => getRTInMinuteFromSpectrumTitle(parsingRules, pm.getMs2Query.spectrumTitle) == rt && pm.score > minScore)
    else peptideMatches.filter(pm => getRTInMinuteFromSpectrumTitle(parsingRules, pm.getMs2Query.spectrumTitle) == rt) // should not happen (means that both scores are NaN)
  }

}
