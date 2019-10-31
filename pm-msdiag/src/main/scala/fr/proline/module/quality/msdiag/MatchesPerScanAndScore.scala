package fr.proline.module.quality.msdiag

import scala.collection.mutable.ArrayBuffer
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.core.om.model.msi.PeptideMatch
import scala.math.ceil
import com.typesafe.scalalogging.LazyLogging

/*
 *  This is what it should return
|  Scan group | Unassigned | Score <= 20.0 | 20.0 < score <= 40.0 | 40.0 < score <= 60.0 | Score > 60.0 |
|     1 - 100 |         60 |             0 |                    1 |                    0 |            0 |
|   101 - 200 |         70 |             0 |                    5 |                    0 |            0 |
...
...
...
| 3001 - 3100 |         45 |             0 |                   11 |                    7 |            0 |
| 3101 - 3200 |         73 |             0 |                   13 |                    3 |            0 |
| 3201 - 3208 |          6 |             0 |                    1 |                    1 |            0 |
 */

object MatchesPerScanAndScore extends LazyLogging {

  def get(rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer, nbScansPerGroup: Int, preferedOrder: Int = 0): MSDiagOutput = {

//    val peptideMatches = rs.getAllPeptideMatches.filter(_.rank == maxRank)
    val peptideMatches = if(rs.isTargetOnly) rs.getAllPeptideMatches.filter(_.sdPrettyRank <= maxRank) else rs.getAllPeptideMatches.filter(_.cdPrettyRank <= maxRank)
    
    // get the boundaries
    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    val nbGroups: Int = ceil(rs.getAllMsQueries.size / nbScansPerGroup).toInt + 1
    if (nbGroups == 0) throw new Exception("No groups of scans")
    val data = Array.ofDim[Any](nbGroups, scoreWindow.length + 3) // add one column for the scan group, and one for the unassigned

    val columnNames = new ArrayBuffer[String]()
    val columnTypes = new ArrayBuffer[String]()
    val columnCategories = new ArrayBuffer[String]()
    columnNames += "Scan group"
    columnNames += "Unassigned"
    columnTypes += "String"
    columnTypes += "Integer"
    val allQueries = rs.getAllMsQueries.map(q => q.initialId)
    for (i <- 0 until nbGroups) {
      // extract concerned query ids
      val min = i*nbScansPerGroup
      var max = (i+1)*nbScansPerGroup
      val queries = allQueries.slice(min, max)
      data(i)(0) = queries.head + " - " + queries.last
      data(i)(1) = rs.getUnassignedQueries.count(q => { queries.contains(q.initialId) })
	  val peptideMatchesPerGroup = peptideMatches.filter(pm => queries.contains(pm.msQuery.initialId))
      for (s <- 0 to scoreWindow.length) {
        val minScore = if (s == 0) Float.NaN else scoreWindow(s - 1)
        val maxScore = if (s == scoreWindow.length) Float.NaN else scoreWindow(s)
        // add array content
        data(i)(s+2) = MSDiagUtils.countMatchesPerScore(peptideMatchesPerGroup, minScore, maxScore)
        // add column headers
        if (i == 0) { 
          columnNames += MSDiagUtils.getScoreLabel(minScore, maxScore)
          columnTypes += "Integer"
        }
      }
    }

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Chromatogram,
      description = "Number of matches per group of scans and score",
      columnNames = columnNames.toSeq,
      columnTypes = columnTypes.toSeq,
      columnCategories = columnCategories.toSeq,
      xAxisDescription = "Scan numbers",
      yAxisDescription = "Matches",
      preferedOrder = preferedOrder)
  }
  
}
