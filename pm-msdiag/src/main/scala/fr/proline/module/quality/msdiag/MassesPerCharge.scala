package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.core.om.model.msi.PeptideMatch

/*
 * This is what it should return 
| Charge |  Lowest Mz |  Highest Mz |         Average Mz |   Median Mz |
|      1 | 501.291092 | 1008.596771 |  716.4439398290601 |  688.354401 |
|      2 | 610.281845 | 2444.279846 |  1175.752791098313 | 1104.584885 |
|      3 | 837.416687 | 3157.566681 | 1773.5517001666665 | 1810.419022 |
 */

object MassesPerCharge extends Logging {

  def get(rs: MSDiagResultSetManager, maxRank: Integer): MSDiagOutput = {
    
    val peptideMatches = rs.getAllPeptideMatches.filter(_.rank == maxRank)

    val columnNames = Array[String]("Charge", "Lowest Mass", "Highest Mass", "Average Mass", "Median Mass")
    val charges: Array[Int] = MSDiagUtils.getCharges(rs)
    val data = Array.ofDim[Any](charges.length, columnNames.length)

    for (c <- 0 until charges.length) {
      val peptideMatchesPerCharge = peptideMatches.filter(_.getMs2Query.charge == charges(c))
//      val pm = peptideMatchesPerCharge.sortBy(_.peptide.calculatedMass).splitAt(peptideMatchesPerCharge.size / 2)
      data(c)(0) = charges(c)
      data(c)(1) = peptideMatchesPerCharge.minBy(_.peptide.calculatedMass).peptide.calculatedMass
      data(c)(2) = peptideMatchesPerCharge.maxBy(_.peptide.calculatedMass).peptide.calculatedMass
      data(c)(3) = (peptideMatchesPerCharge.map(_.peptide.calculatedMass).sum / peptideMatchesPerCharge.size).toDouble
      data(c)(4) = getMedianMass(peptideMatchesPerCharge)
    }

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Table,
      cellType = scala.Double.toString,
      description = "Calculated masses per charge",
      columnNames = columnNames.toSeq,
      xAxisDescription = "Masses",
      yAxisDescription = "Charges")
  }
  
  private def getMedianMass(peptideMatchesPerCharge: Array[PeptideMatch]): Double = {
    val size = peptideMatchesPerCharge.size
    val (lower, upper) = peptideMatchesPerCharge.sortBy(_.peptide.calculatedMass).splitAt(size / 2)
    if (size % 2 == 0) {
      (lower.last.peptide.calculatedMass + upper.head.peptide.calculatedMass) / 2.0
    } else {
      upper.head.peptide.calculatedMass
    }
  }
  
}