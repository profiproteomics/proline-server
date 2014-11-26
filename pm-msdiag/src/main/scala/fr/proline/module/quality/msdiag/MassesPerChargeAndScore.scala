package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Ms2Query

object MassesPerChargeAndScore extends Logging {

  def get(rs: MSDiagResultSetManager, scoreWindow: Array[Float], maxRank: Integer): MSDiagOutput = {

    if (scoreWindow.length == 0) throw new Exception("Score window is empty")
    val unassignedQueries = rs.getUnassignedQueries
    val peptideMatches = if(rs.isTargetOnly) rs.getAllPeptideMatches.filter(_.sdPrettyRank <= maxRank) else rs.getAllPeptideMatches.filter(_.cdPrettyRank <= maxRank)

    // get the boundaries
//    val columnNames = Array[String]("Charge", "Score", "Lowest Mass", "Highest Mass", "Average Mass", "Median Mass")
    val columnNames = Array[String]("Charge & Score", "Lowest Mass", "Highest Mass", "Average Mass", "Median Mass")
    val charges: Array[Int] = MSDiagUtils.getCharges(rs)
    val data = Array.ofDim[Any](charges.length*(scoreWindow.length+3), columnNames.length)
    var line = 0

    for (c <- 0 until charges.length) {
      val peptideMatchesPerCharge = peptideMatches.filter(_.getMs2Query.charge == charges(c))
      // unassigned
      data(line) = getRowValues(charges(c), "Unassigned", unassignedQueries)
      line += 1
      // for each score
      for (s <- 0 to scoreWindow.length) {
        val minScore = if (s == 0) Float.NaN else scoreWindow(s - 1)
        val maxScore = if (s == scoreWindow.length) Float.NaN else scoreWindow(s)
        data(line) = getRowValues(charges(c), MSDiagUtils.getScoreLabel(minScore, maxScore), MSDiagUtils.filterMatchesPerScore(peptideMatchesPerCharge, minScore, maxScore))
        line += 1
      }
      // total
      data(line) = getRowValues(charges(c), "All assigned", peptideMatchesPerCharge)
      line += 1
    }

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Table,
//      cellType = scala.Double.toString,
      cellType = scala.Double.toString,
      description = "Exp. MoZ per charge and score",
      columnNames = columnNames.toSeq,
      xAxisDescription = "Masses",
      yAxisDescription = "Charges and scores")
  }

//  private def getRowValues(charge: Int, score: String, matches: Any): Array[Any] = Array(charge, score, getMinMoz(matches), getMaxMoz(matches), getAverageMoz(matches), getMedianMoz(matches))  
  private def getRowValues(charge: Int, score: String, matches: Any): Array[Any] = Array(score+" for charge "+charge, getMinMoz(matches), getMaxMoz(matches), getAverageMoz(matches), getMedianMoz(matches))

  private def getMinMoz(matches: Any): Double = {
    matches match {
      case peptideMatches: Array[PeptideMatch] => if(!peptideMatches.isEmpty) peptideMatches.minBy(_.getMs2Query.moz).getMs2Query.moz else 0
      case msQueries: Array[Ms2Query] => if(!msQueries.isEmpty) msQueries.minBy(_.moz).moz else 0
      case _ => 0
    }
  }
  
  private def getMaxMoz(matches: Any): Double = {
    matches match {
      case peptideMatches: Array[PeptideMatch] => if(!peptideMatches.isEmpty) peptideMatches.maxBy(_.getMs2Query.moz).getMs2Query.moz else 0
      case msQueries: Array[Ms2Query] => if(!msQueries.isEmpty) msQueries.maxBy(_.moz).moz else 0
      case _ => 0
    }
  }
  
  private def getAverageMoz(matches: Any): Double = {
    matches match {
      case peptideMatches: Array[PeptideMatch] => if(!peptideMatches.isEmpty) (peptideMatches.map(_.getMs2Query.moz).sum / peptideMatches.size).toDouble else 0
      case msQueries: Array[Ms2Query] => if(!msQueries.isEmpty) (msQueries.map(_.moz).sum / msQueries.size).toDouble else 0
      case _ => 0
    }
  }
  
  private def getMedianMoz(matches: Any): Double = {
	  matches match {
	    case pms: Array[PeptideMatch] => {
	      val size = pms.size
	      if(size > 0) {
	        val (lower, upper) = pms.sortBy(_.getMs2Query.moz).splitAt(size / 2)
	        if (size % 2 == 0) (lower.last.getMs2Query.moz + upper.head.getMs2Query.moz) / 2.0 else upper.head.getMs2Query.moz
	      } else 0
	    }
	    case msq: Array[Ms2Query] => {
	      val size = msq.size
	      if(size > 0) {
	        val (lower, upper) = msq.sortBy(_.moz).splitAt(size / 2)
	        if (size % 2 == 0) (lower.last.moz + upper.head.moz) / 2.0 else upper.head.moz
	      } else 0
	    }
	    case _ => 0
    }
  }

}