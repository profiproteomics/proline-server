package fr.proline.module.quality.msdiag

import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.core.om.model.msi.PeptideMatch
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.LazyLogging

/*
 *  This is what it should return
|      Resultset | Score <= 20.0 | 20.0 < score <= 40.0 | 40.0 < score <= 60.0 | Score > 60.0 |
| Target matches |             0 |                  277 |                  224 |            0 |
| Decoy matches  |             0 |                   19 |                    0 |            0 |
 */

object MatchesPerResultSetAndScore extends LazyLogging {

  def get(rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer, preferedOrder: Int = 0): MSDiagOutput = {
    
//    val targetPeptideMatches = rs.getTargetPeptideMatches.filter(_.rank == maxRank)
//    val decoyPeptideMatches = rs.getDecoyPeptideMatches.filter(_.rank == maxRank)
    val targetPeptideMatches = if(rs.isTargetOnly) rs.getTargetPeptideMatches.filter(_.sdPrettyRank <= maxRank) else rs.getTargetPeptideMatches.filter(_.cdPrettyRank <= maxRank)
    val decoyPeptideMatches = if(rs.isTargetOnly) rs.getDecoyPeptideMatches.filter(_.sdPrettyRank <= maxRank) else rs.getDecoyPeptideMatches.filter(_.cdPrettyRank <= maxRank)
    
   // get the boundaries
    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    val data = Array.ofDim[Any](2, scoreWindow.length + 2) // add one column for the charge
    
    // set up main array
    val columnNames = new ArrayBuffer[String]()
    val columnTypes = new ArrayBuffer[String]()
    val columnCategories = ArrayBuffer[String]()
    
    columnNames += "Resultset"
    columnTypes += "String"
    columnCategories += "Category"
    data(0)(0) = "Target PSM"
    data(1)(0) = "Decoy PSM"
    for (s <- 0 to scoreWindow.length) {
  		val minScore = if (s == 0) Float.NaN else scoreWindow(s - 1)
  		val maxScore = if (s == scoreWindow.length) Float.NaN else scoreWindow(s)
  		// add array content
  		data(0)(s+1) = MSDiagUtils.countMatchesPerScore(targetPeptideMatches, minScore, maxScore)
  		data(1)(s+1) = MSDiagUtils.countMatchesPerScore(decoyPeptideMatches, minScore, maxScore)
  //		System.out.println("ABU "+minScore+" "+maxScore+" "+data(0)(s+1)+" "+data(1)(s+1)+" "+targetPeptideMatches.size+" "+decoyPeptideMatches.size)
  		// add column headers
  		columnNames += MSDiagUtils.getScoreLabel(minScore, maxScore)
      columnTypes += "Integer"
      columnCategories += "Data"
	}
    
    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.CategoryPlot,
      description = "Score repartition for Target/Decoy data",
      columnNames = columnNames.toSeq,
      columnTypes= columnTypes.toSeq,
      columnCategories = columnCategories.toSeq,
      xAxisDescription = "Resultset type",
      yAxisDescription = "Number of categorized data",
      preferedOrder = preferedOrder)
  }
  
}
