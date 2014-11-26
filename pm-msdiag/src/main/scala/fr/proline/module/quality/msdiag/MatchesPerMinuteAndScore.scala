package fr.proline.module.quality.msdiag

import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
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
  
  def get(_rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer): MSDiagOutput = {

    rs = _rs
//    val peptideMatches = rs.getSpectraPerPeptideMatches.filter(_._1.rank == maxRank)
    val unassignedSpectra = rs.getUnassignedSpectra
    val peptideMatches = if(rs.isTargetOnly) rs.getSpectraPerPeptideMatches.filter(_._1.sdPrettyRank <= maxRank) else rs.getSpectraPerPeptideMatches.filter(_._1.cdPrettyRank <= maxRank)

    // get the boundaries
    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    if (peptideMatches.isEmpty) throw new Exception("No peptide matches")
    try {
	    val rts: Array[Int] = rs.getAllSpectra.groupBy(extractRT(_)).keys.filter(_ > 0).toArray.sorted
	    if (rts.length == 0) throw new Exception("No retention time found")
	    val data = Array.ofDim[Any](rts.length, scoreWindow.length + 3) // add one column for the rt, and one for the unassigned
	
	    val columnNames = new ArrayBuffer[String]()
	    columnNames += "Retention time"
	    columnNames += "Unassigned"
	    for (rt <- 0 until rts.length) {
	      data(rt)(0) = rts(rt)
	      data(rt)(1) = unassignedSpectra.count(s => extractRT(s) == rts(rt))
	      val peptideMatchesPerMinute = peptideMatches.filter(pms => extractRT(pms._2) == rts(rt)).keys.toArray
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
      case e: Exception => 
//        logger.error(e.getMessage())
//        e.printStackTrace()
        return null
    }
  }
  
  private def extractRT(spectrum: Spectrum): Int = spectrum.firstTime.ceil.toInt

}