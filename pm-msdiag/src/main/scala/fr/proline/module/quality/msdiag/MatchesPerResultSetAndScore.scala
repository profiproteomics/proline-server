package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.core.om.model.msi.PeptideMatch
import scala.collection.mutable.ArrayBuffer

/*
 *  This is what it should return
|      Resultset | Score <= 20.0 | 20.0 < score <= 40.0 | 40.0 < score <= 60.0 | Score > 60.0 |
| Target matches |             0 |                  277 |                  224 |            0 |
| Decoy matches  |             0 |                   19 |                    0 |            0 |
 */

object MatchesPerResultSetAndScore extends Logging {

  def get(rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer): MSDiagOutput = {
    
    val targetPeptideMatches = rs.getTargetPeptideMatches.filter(_.rank == maxRank)
    val decoyPeptideMatches = rs.getDecoyPeptideMatches.filter(_.rank == maxRank)
    
   // get the boundaries
    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    val data = Array.ofDim[Any](2, scoreWindow.length + 2) // add one column for the charge
    
    // set up main array
    val columnNames = new ArrayBuffer[String]()
    columnNames += "Resultset"
    data(0)(0) = "Target matches"
    data(1)(0) = "Decoy matches"
    for (s <- 0 to scoreWindow.length) {
		val minScore = if (s == 0) Float.NaN else scoreWindow(s - 1)
		val maxScore = if (s == scoreWindow.length) Float.NaN else scoreWindow(s)
		// add array content
		data(0)(s+1) = MSDiagUtils.countMatchesPerScore(targetPeptideMatches, minScore, maxScore)
		data(1)(s+1) = MSDiagUtils.countMatchesPerScore(decoyPeptideMatches, minScore, maxScore)
		// add column headers
		columnNames += MSDiagUtils.getScoreLabel(minScore, maxScore)
	}
    
    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Table,
      description = "Number of matches per resultset and score",
      columnNames = columnNames.toSeq,
      xAxisDescription = "Scores",
      yAxisDescription = "Resultsets")
  }
  
}