package fr.proline.module.quality.msdiag.view

import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.scene.Scene
import javafx.scene.chart.PieChart
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import javafx.scene.layout.VBox

class MSDiagPieChart(msdiag: MSDiagOutput) {

  def getVBox: VBox = {
    val pieChart: PieChart = new PieChart()
    var total: Int = 0
    msdiag.matrix(0).foreach(total += _.asInstanceOf[Int])
    val pieChartData: ObservableList[PieChart.Data] = FXCollections.observableArrayList()

    for (i <- 0 until msdiag.columnNames.size) {
      val serie = msdiag.columnNames(i)
      val value = msdiag.matrix(0)(i).asInstanceOf[Int]
      pieChartData.add(new PieChart.Data(serie + ": " + pieDataAsString(value, total), value))
    }

    // generate vbox
    pieChart.setTitle(msdiag.description)
    pieChart.setData(pieChartData)
    val vbox: VBox = new VBox()
    vbox.getChildren().addAll(pieChart)
    vbox
  }

  private def pieDataAsString(value: Double, total: Double): String = value.toInt + " (" + (value * 100 / total).toInt + "%)"

}