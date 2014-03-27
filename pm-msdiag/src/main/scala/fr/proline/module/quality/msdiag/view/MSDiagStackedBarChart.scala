package fr.proline.module.quality.msdiag.view

import java.util.ArrayList
import javafx.collections.FXCollections
import javafx.scene.Scene
import javafx.scene.chart.CategoryAxis
import javafx.scene.chart.NumberAxis
import javafx.scene.chart.StackedBarChart
import javafx.scene.chart.XYChart
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import javafx.scene.layout.VBox
import scala.collection.JavaConverters._

class MSDiagStackedBarChart(msdiag: MSDiagOutput) {

  val xAxis: CategoryAxis = new CategoryAxis()
  val yAxis: NumberAxis = new NumberAxis()
  val stackedChart: StackedBarChart[String, Number] = new StackedBarChart[String, Number](xAxis, yAxis)

  def getVBox: VBox = {

    // clear previous data if any
    stackedChart.getData().clear()

    // add descriptions
    xAxis.setCategories(FXCollections.observableList[String](msdiag.matrix.map(_(0).toString).toList.asJava))
    xAxis.setLabel(msdiag.xAxisDescription)
    yAxis.setLabel(msdiag.yAxisDescription)
    stackedChart.setTitle(msdiag.description)

    // add series
    for (column <- 1 until msdiag.columnNames.size) {
      val serie: XYChart.Series[String, Number] = new XYChart.Series[String, Number]()
      serie.setName(msdiag.columnNames(column).toString)
      msdiag.matrix.foreach(line => {
        serie.getData().add(new XYChart.Data[String, Number](line(0).toString, line(column).asInstanceOf[Int]))
      })
      stackedChart.getData().add(serie)
    }

    // generate vbox
    val vbox: VBox = new VBox()
    vbox.getChildren().addAll(stackedChart)
    vbox
  }
}
