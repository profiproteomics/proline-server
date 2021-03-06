package fr.proline.module.quality.msdiag

//import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.MsQuery
import scala.collection.mutable.ArrayBuffer
import scala.Int
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import com.typesafe.scalalogging.LazyLogging

/*
 *  This is what it should return
| Charge | Unassigned | Score <= 20.0 | 20.0 < score <= 40.0 | 40.0 < score <= 60.0 | Score > 60.0 |
|      1 |       1172 |             0 |                    6 |                    0 |            0 |
|      2 |        537 |             0 |                  244 |                  208 |            0 |
|      3 |        194 |             0 |                   46 |                   16 |            0 |
 */

object MatchesPerChargeAndScore extends LazyLogging {
  
  private var rs: MSDiagResultSetManager = null
  
  def get(_rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer, preferedOrder: Int = 0): MSDiagOutput = {
    
    rs = _rs
//    val peptideMatches = rs.getAllPeptideMatches.filter(_.rank == maxRank)
    val peptideMatches = if(rs.isTargetOnly) rs.getAllPeptideMatches.filter(_.sdPrettyRank <= maxRank) else rs.getAllPeptideMatches.filter(_.cdPrettyRank <= maxRank)
    
    // get the boundaries
    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    val charges: Array[Int] = MSDiagUtils.getCharges(rs)
    val data = Array.ofDim[Any](charges.length, scoreWindow.length + 3) // add one column for the charge, and one for the unassigned

    // set up main array
    val columnNames = new ArrayBuffer[String]()
    val columnTypes = new ArrayBuffer[String]()
    val columnCategories = new ArrayBuffer[String]()
    
    columnNames += "Charge"
    columnNames += "Unassigned"
    columnTypes += "Integer" // Charge
    columnTypes += "Integer" // Unassigned
      
    columnCategories += "Category"
    columnCategories += "Data"
    columnCategories += "Data" 
    columnCategories += "Data" 
      
      
      
    
    for (c <- 0 until charges.length) {
      data(c)(0) = charges(c)
      data(c)(1) = rs.getUnassignedQueries.count(q => q.charge == charges(c))
	  val peptideMatchesPerCharge = peptideMatches.filter(pm => pm.msQuery.charge == charges(c))
      for (s <- 0 to scoreWindow.length) {
        val minScore = if (s == 0) Float.NaN else scoreWindow(s - 1)
        val maxScore = if (s == scoreWindow.length) Float.NaN else scoreWindow(s)
        // add array content
        data(c)(s+2) = MSDiagUtils.countMatchesPerScore(peptideMatchesPerCharge, minScore, maxScore)
        // add column headers
        if(c == 0) {
          columnTypes += "Integer" // number of PSM per charge and score (AW)
          columnNames += MSDiagUtils.getScoreLabel(minScore, maxScore)
        }
      }
    }

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Chromatogram,
      description = "PSM per charge and score",
      columnNames = columnNames.toSeq,
      columnTypes = columnTypes.toSeq,
      columnCategories = columnCategories.toSeq,
      xAxisDescription = "Charge",
      yAxisDescription = "PSMs",
      preferedOrder = preferedOrder)
  }

}
