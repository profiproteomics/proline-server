package fr.proline.module.quality.msdiag

import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.{ MsQuery, Ms1Query, Ms2Query }
import fr.proline.core.om.model.msi.{ SpectrumTitleFields, SpectrumTitleParsingRule }
import scala.math.ceil
import fr.proline.module.quality.msdiag.msi.MSDiagUtils

/*
 *  This is what it should return
| Retention time | Unassigned | Score <= 20.0 | 20.0 < score <= 40.0 | 40.0 < score <= 60.0 | Score > 60.0 |
|              1 |          0 |             0 |                    0 |                    1 |            0 |
|              2 |         66 |             0 |                   11 |                    4 |            0 |
|              3 |         32 |             0 |                   10 |                   11 |            0 |
...
...
...
|             34 |         81 |             0 |                    2 |                    1 |            0 |
|             35 |         39 |             0 |                    0 |                    0 |            0 |  
 */

object MatchesPerMinuteAndScore extends Logging {

  private var rs: MSDiagResultSetManager = null
  private var parsingRules: Option[SpectrumTitleParsingRule] = None

  def get(_rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer, _parsingRules: Option[SpectrumTitleParsingRule]): MSDiagOutput = {

    rs = _rs
    parsingRules = _parsingRules
    val peptideMatches = rs.getAllPeptideMatches.filter(_.rank == maxRank)

    // get the boundaries
    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    if (!parsingRules.isDefined) throw new Exception("Parsing rules are missing")
    try {
	    val rts: Array[Int] = getRetentionTimeByMinutes
	    if (rts.length == 0) throw new Exception("No retention time found")
	    val data = Array.ofDim[Any](rts.length, scoreWindow.length + 3) // add one column for the rt, and one for the unassigned
	
	    val columnNames = new ArrayBuffer[String]()
	    columnNames += "Retention time"
	    columnNames += "Unassigned"
	    for (rt <- 0 until rts.length) {
	      data(rt)(0) = rts(rt)
	      data(rt)(1) = rs.getUnassignedQueries.count(q => getRetentionTimeInMinuteFromMsQuery(q) == rts(rt))
	      val peptideMatchesPerMinute = peptideMatches.filter(pm => getRetentionTimeInMinuteFromMsQuery(pm.msQuery) == rt)
	      for (s <- 0 to scoreWindow.length) {
	        val minScore = if (s == 0) Float.NaN else scoreWindow(s - 1)
	        val maxScore = if (s == scoreWindow.length) Float.NaN else scoreWindow(s)
	        // add array content
	        data(rt)(s + 2) = MSDiagUtils.countMatchesPerScore(peptideMatchesPerMinute, minScore, maxScore)
	        // add column headers
	        if (rt == 0) columnNames += MSDiagUtils.getScoreLabel(minScore, maxScore)
	      }
	    }
	
	    // return output
	    new MSDiagOutput(
	      matrix = data,
	      outputType = MSDiagOutputTypes.Chromatogram,
	      description = "Number of matches per minute of retention time and score",
	      columnNames = columnNames.toSeq,
	      xAxisDescription = "Retention time",
	      yAxisDescription = "Matches")
    } catch {
      case e: Exception => return null
    }
  }

  private def getRetentionTimeByMinutes(): Array[Int] = {
    rs.getAllMsQueries.groupBy(q => getRetentionTimeInMinuteFromMsQuery(q)).keys.filter(rt => rt >= 0).toArray.sorted
  }

  private def getRetentionTimeInMinuteFromMsQuery(msQuery: MsQuery): Int = {
    msQuery match {
      case ms1q: Ms1Query => -1
      case ms2q: Ms2Query => extractRetentionTimeFromTitle(ms2q.spectrumTitle)
    }
  }

  private def extractRetentionTimeFromTitle(title: String): Int = {
    val specTitleFieldMap = parsingRules.map(_.parseTitle(title)).getOrElse(Map.empty[SpectrumTitleFields.Value, String])
    val rt = specTitleFieldMap.get(SpectrumTitleFields.FIRST_TIME).get.toFloat
    ceil(rt).toInt
  }

}