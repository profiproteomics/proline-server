package fr.proline.module.quality.msdiag.view

import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Scene
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import javafx.scene.layout.VBox
import scala.collection.mutable.ArrayBuffer
import javafx.scene.control.ScrollPane

object MSDiagViewer {

  var width: Double = 800
  var heigth: Double = 600
  private var charts = new ArrayBuffer[VBox]
  
  // add a new chart to the list of charts to print
  def addNewChart(msdiag: MSDiagOutput) {
    msdiag.outputType match {
      case MSDiagOutputTypes.Table => charts += new MSDiagTable(msdiag).getVBox(width)
      case MSDiagOutputTypes.Chromatogram => charts += new MSDiagStackedBarChart(msdiag).getVBox
      case MSDiagOutputTypes.Pie => charts += new MSDiagPieChart(msdiag).getVBox
      case MSDiagOutputTypes.Histogram => // nothing yet
    }
  }
  
  // remove all charts
  def clearCharts = charts.clear
  
  // print every charts loaded
  def start {
    val args: Array[String] = Array.empty[String]
    Application.launch(classOf[MSDiagViewer], args: _*)
  }
}

class MSDiagViewer extends Application {

//  private val cssFile: String = this.getClass().getResource("application.css").toExternalForm()

  // print the charts
  override def start(primaryStage: Stage) {
    val vbox: VBox = new VBox()
    MSDiagViewer.charts.foreach(vbox.getChildren().add(_))
    val scrollPane = new ScrollPane
    scrollPane.setContent(vbox)
    primaryStage.setScene(new Scene(scrollPane, MSDiagViewer.width, MSDiagViewer.heigth))
//    scene.getStylesheets().add(cssFile)
    primaryStage.show()
  }

}
