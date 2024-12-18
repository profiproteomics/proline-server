package fr.proline.module.quality.msdiag

//import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import fr.proline.core.om.model.msi.PeptideMatch
import com.typesafe.scalalogging.LazyLogging

/*
 * This is what it should return 
| Charge |  Lowest Mz |  Highest Mz |         Average Mz |   Median Mz |
|      1 | 501.291092 | 1008.596771 |  716.4439398290601 |  688.354401 |
|      2 | 610.281845 | 2444.279846 |  1175.752791098313 | 1104.584885 |
|      3 | 837.416687 | 3157.566681 | 1773.5517001666665 | 1810.419022 |
 */

object MassesPerCharge extends LazyLogging {

  def get(rs: MSDiagResultSetManager, maxRank: Integer, preferedOrder: Int = 0): MSDiagOutput = {
    
//    val peptideMatches = rs.getAllPeptideMatches.filter(_.rank == maxRank)
    val peptideMatches = if(rs.isTargetOnly) rs.getAllPeptideMatches.filter(_.sdPrettyRank <= maxRank) else rs.getAllPeptideMatches.filter(_.cdPrettyRank <= maxRank)

    val columnNames = Array[String]("Charge", "Lowest Mass", "Highest Mass", "Average Mass", "Median Mass")
    val columnTypes = Array[String]("Integer", "Double", "Double", "Double", "Double")
    val columnCategories = Array[String]("Category", "Data",  "Data", "Data", "Data")
    val charges: Array[Int] = MSDiagUtils.getCharges(rs)
    val data = Array.ofDim[Any](charges.length, columnNames.length)

    for (c <- 0 until charges.length) {
      val peptideMatchesPerCharge = peptideMatches.filter(_.getMs2Query.charge == charges(c))
      data(c) = if(peptideMatchesPerCharge.size > 0) {
        Array(charges(c),
              peptideMatchesPerCharge.minBy(_.peptide.calculatedMass).peptide.calculatedMass,
              peptideMatchesPerCharge.maxBy(_.peptide.calculatedMass).peptide.calculatedMass,
              (peptideMatchesPerCharge.map(_.peptide.calculatedMass).sum / peptideMatchesPerCharge.size).toDouble,
              getMedianMass(peptideMatchesPerCharge))
      } else {
        Array(charges(c), 0, 0, 0, 0)
      }
    }

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Table,
      cellType = scala.Double.toString,
      description = "Calculated masses per charge",
      columnNames = columnNames.toSeq,
      columnTypes = columnTypes.toSeq,
      columnCategories = columnCategories.toSeq,
      xAxisDescription = "Masses",
      yAxisDescription = "Charges",
      preferedOrder = preferedOrder)
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