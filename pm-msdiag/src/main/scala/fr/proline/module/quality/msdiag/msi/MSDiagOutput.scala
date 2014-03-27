package fr.proline.module.quality.msdiag.msi

object MSDiagOutput {

  def generateFakeTable: MSDiagOutput = {
    val matrix: Array[Array[Any]] = Array(
    	Array(1.0, 501.291092, 1008.596771, 716.4439398290601, 688.354401), 
		Array(2.0, 610.281845, 2444.279846, 1175.752791098313, 1104.584885),
		Array(3.0, 837.416687, 3157.566681, 1773.5517001666665, 1810.419022))
    new MSDiagOutput(matrix, MSDiagOutputTypes.Table, scala.Double.toString, "Calculated masses per charge", Array("Charge", "Lowest Mz", "Highest Mz", "Average Mz", "Median Mz"), "Masses", "Charges")
  }
  def generateFakePie: MSDiagOutput = {
    val matrix: Array[Array[Any]] = Array(Array(1903, 1305))
    new MSDiagOutput(matrix, MSDiagOutputTypes.Pie, scala.Int.toString, "Repartition of assigned and unassigned spectra", Array("Unassigned", "Assigned"), "", "")
  }
  def generateFakeChromatogram: MSDiagOutput = {
    val matrix: Array[Array[Any]] = Array(
    	Array(1, 0, 0, 0, 0, 0), Array(2, 66, 0, 0, 1, 0), Array(3, 32, 0, 11, 4, 0), Array(4, 23, 0, 10, 11, 0), Array(5, 42, 0, 17, 8, 0), 
    	Array(6, 34, 0, 10, 8, 0), Array(7, 31, 0, 11, 9, 0), Array(8, 27, 0, 9, 9, 0), Array(9, 37, 0, 21, 8, 0), Array(10, 39, 0, 17, 8, 0),
		Array(11, 35, 0, 16, 8, 0), Array(12, 33, 0, 20, 11, 0), Array(13, 26, 0, 15, 18, 0), Array(14, 43, 0, 14, 16, 0), Array(15, 47, 0, 17, 15, 0), 
		Array(16, 44, 0, 19, 10, 0), Array(17, 46, 0, 16, 7, 0), Array(18, 61, 0, 10, 13, 0), Array(19, 62, 0, 7, 7, 0), Array(20, 63, 0, 5, 5, 0), 
		Array(21, 72, 0, 9, 5, 0), Array(22, 71, 0, 5, 6, 0), Array(23, 69, 0, 3, 3, 0), Array(24, 77, 0, 5, 7, 0), Array(25, 76, 0, 6, 5, 0), 
		Array(26, 73, 0, 5, 3, 0), Array(27, 72, 0, 4, 3, 0), Array(28, 84, 0, 1, 2, 0), Array(29, 76, 0, 4, 4, 0), Array(30, 87, 0, 1, 3, 0), 
		Array(31, 76, 0, 2, 2, 0), Array(32, 77, 0, 3, 3, 0), Array(33, 82, 0, 0, 0, 0), Array(34, 81, 0, 1, 1, 0),Array(35, 39, 0, 2, 1, 0))
    new MSDiagOutput(matrix, MSDiagOutputTypes.Chromatogram, scala.Int.toString, "Number of matches per minute of retention time and score", 
        Array("Retention time", "Unassigned", "Score <= 20.0", "20.0 < score <= 40.0", "40.0 < score <= 60.0", "Score > 60.0"), "Retention time", "Matches")
  }
  
}

case class MSDiagOutput(
  val matrix: Array[Array[Any]], // the output table containing all data
  val outputType: MSDiagOutputTypes.MSDiagOutputType, // the type of information that is represented by the table (chart, table, histogram, etc)
  val cellType: String = scala.Int.toString, // the (native) type of cell values (integer, string, float)
  val description: String = "", // the string description of the output (ie. Number of matches)
  val columnNames: Seq[_] = Seq[String](), // the column headers
  val xAxisDescription: String = "", // the string description of the X axis (ie. Retention times)
  val yAxisDescription: String = "" // the string description of the Y axis (ie. Score)
  ) {
  
  override def toString: String = {
    var str = "Output type => '" + outputType.toString() + "'"
    if (matrix != null && matrix.length > 0) {
      if (matrix(0).length > 0) str += " ; Matrix dimensions : '" + matrix.length + " * " + matrix(0).length + "'"
      else str += " ; Matrix dimensions : '" + matrix.length + " * 1'"
    } else str += " ; Matrix dimensions : 'empty'"
    if (description != "") str += " ; Description => '" + description + "'"
    if (xAxisDescription != "") str += " ; X axis => '" + xAxisDescription + "'"
    if (yAxisDescription != "") str += " ; Y axis => '" + yAxisDescription + "'"
    str
  }
  
  def getMatrixAsText: String = {
    var text = description+"\n"
    text += "| "+columnNames.mkString(" | ") + " |\n"
    for(i <- 0 until matrix.length) {
      text += "| "+matrix(i).mkString(" | ") + " |\n"
    }
    text
  }

}
