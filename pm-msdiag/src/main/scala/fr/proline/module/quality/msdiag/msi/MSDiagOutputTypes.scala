package fr.proline.module.quality.msdiag.msi

object MSDiagOutputTypes extends Enumeration {
  type MSDiagOutputType = Value // output should always be a matrix with some additional information (at least : what is X, what is Y, what is in a cell)
  val Table = Value("table") // matrix of textual data
  val Histogram = Value("histogram") // 1D
  val Pie = Value("pie") // ?
  val Chromatogram = Value("chromatogram") // 2D
}
