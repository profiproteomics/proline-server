package fr.proline.module.quality.msdiag.view

import scala.annotation.migration
import scala.collection.mutable.HashMap
import fr.proline.module.quality.msdiag.MSDiag
import fr.proline.module.quality.msdiag.msi.{ MSDiagOutput, MSDiagOutputTypes }
import javafx.application.Application
import javafx.event.{ ActionEvent, EventHandler }
import javafx.geometry.Insets
import javafx.scene.{ Cursor, Scene }
import javafx.scene.control.{ Button, Label, ScrollPane, TextField }
import javafx.scene.layout.{ HBox, VBox }
import javafx.stage.Stage

object MSDiagViewer {

  var width: Double = 800
  var heigth: Double = 600
  private var msdiag: MSDiag = null
  private var msdiagStage: Stage = null
  private var vboxMSDiags: VBox = null
  private val cssFile: String = this.getClass.getClassLoader().getResource("application.css").toString()

  // load the MSDiag charts and show the window
  def load(msdiag: MSDiag) {
    this.msdiag = msdiag
    this.prepareCharts
    val args: Array[String] = Array.empty[String]
    Application.launch(classOf[MSDiagViewer], args: _*)
  }

  // set new settings and reload the window
  private def reload(newSettings: Map[String, Any]) {
    msdiag.setSettings(newSettings)
    this.prepareCharts
  }

  // get charts for each output of the MSDiag object with current settings
  private def prepareCharts {
    vboxMSDiags = new VBox()
    msdiag.getMethods.foreach(m => {
      msdiag.executeMethod(m) match {
        case msd: MSDiagOutput => vboxMSDiags.getChildren().add(this.getChart(msd))
        case _ =>
      }
    })
  }

  // get the chart corresponding to this output
  private def getChart(msdiag: MSDiagOutput): VBox = {
    msdiag.outputType match {
      case MSDiagOutputTypes.Table => new MSDiagTable(msdiag).getVBox(width)
      case MSDiagOutputTypes.Chromatogram => new MSDiagStackedBarChart(msdiag).getVBox
      case MSDiagOutputTypes.Pie => new MSDiagPieChart(msdiag).getVBox
      case MSDiagOutputTypes.Histogram => null // nothing yet
    }
  }

  // create the scene containing everything
  private def generateScene = {
    val vbox: VBox = new VBox()
    vbox.getChildren().add(MSDiagViewer.settingsLine)
    vbox.getChildren().add(MSDiagViewer.vboxMSDiags)
    val scrollPane = new ScrollPane
    scrollPane.setContent(vbox)
    msdiagStage.setScene(new Scene(scrollPane, width, heigth))
    msdiagStage.getScene.getStylesheets().add(cssFile)
  }

  // get first line with settings
  private lazy val settingsLine: HBox = {
    val hbox: HBox = new HBox()
    hbox.setSpacing(10)
    hbox.setPadding(new Insets(10, 0, 10, 0))
    MSDiagViewer.msdiag.getSettings.keys.foreach(s => {
      val tf = new TextField(MSDiagViewer.msdiag.getSettings.get(s).getOrElse("").toString)
      tf.setId(s)
      val lbl = new Label(s)
      lbl.setLabelFor(tf)
      hbox.getChildren().addAll(lbl, tf)
    })
    val btn = new Button
    btn.setText("Update")
    btn.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent) {
        msdiagStage.getScene().setCursor(Cursor.WAIT)
        val newSettings = new HashMap[String, Any]
        for (i <- 0 until hbox.getChildren().size()) {
          hbox.getChildren().get(i) match {
            case tf: TextField => newSettings += tf.getId() -> tf.getText()
            case _ =>
          }
        }
        MSDiagViewer.reload(newSettings.toMap)
        generateScene
        msdiagStage.getScene().setCursor(Cursor.DEFAULT)
      }
    })
    hbox.getChildren().add(btn)
    hbox
  }
}

class MSDiagViewer extends Application {

  // print the charts
  override def start(primaryStage: Stage) {
    MSDiagViewer.msdiagStage = primaryStage
    MSDiagViewer.generateScene
    primaryStage.show()
    primaryStage.requestFocus()
  }

}
