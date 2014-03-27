package fr.proline.module.quality.msdiag.view

import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import javafx.scene.Scene
import javafx.scene.control.TableView
import javafx.scene.control.Label
import javafx.geometry.Pos
import javafx.scene.control.TableColumn
import javafx.util.Callback
import javafx.beans.value.ObservableValue
import javafx.scene.control.TableColumn.CellDataFeatures
import javafx.beans.property.SimpleDoubleProperty
import javafx.collections.ObservableList
import javafx.collections.FXCollections
import javafx.scene.layout.VBox
import javafx.geometry.Insets
import javafx.beans.property.SimpleStringProperty

class MSDiagTable(msdiag: MSDiagOutput) {

  private val table: TableView[Array[String]] = new TableView[Array[String]]()
//  private val table: TableView[Array[Double]] = new TableView[Array[Double]]()

  def getVBox(width: Double): VBox = {

    // empty the table
    table.getColumns().clear
    table.getItems().clear

    // set title
    val label: Label = new Label(msdiag.description)
    label.setAlignment(Pos.BOTTOM_CENTER)
    label.setMinWidth(width)

    // add columns
    for (i <- 0 until msdiag.columnNames.size) {
      val j = i
//      val column: TableColumn[Array[Double], Double] = new TableColumn[Array[Double], Double](msdiag.columnNames(i).toString)
      val column: TableColumn[Array[String], String] = new TableColumn[Array[String], String](msdiag.columnNames(i).toString)
      column.setSortable(true)
      column.setMinWidth(width / msdiag.columnNames.size)
//      column.setCellValueFactory(new Callback[TableColumn.CellDataFeatures[Array[Double], Double], ObservableValue[Double]]() {
//        override def call(param: CellDataFeatures[Array[Double], Double]): ObservableValue[Double] = new SimpleDoubleProperty(param.getValue()(j).doubleValue())
//      })
      column.setCellValueFactory(new Callback[TableColumn.CellDataFeatures[Array[String], String], ObservableValue[String]]() {
        override def call(param: CellDataFeatures[Array[String], String]): ObservableValue[String] = new SimpleStringProperty(param.getValue()(j).toString())
      })
      table.getColumns().add(column)
    }

    // add data
    val data: ObservableList[Array[String]] = FXCollections.observableArrayList()
    msdiag.matrix.foreach(line => data.add(line.map(_.toString)))
//    val data: ObservableList[Array[Double]] = FXCollections.observableArrayList()
//    msdiag.matrix.foreach(line => data.add(line.map(_.asInstanceOf[Double])))
    table.setItems(data)
    table.setEditable(false)

    // generate vbox
    val vbox: VBox = new VBox()
    vbox.setSpacing(10)
    vbox.setPadding(new Insets(10, 0, 10, 0))
    vbox.getChildren().addAll(label, table)
    vbox

  }
}