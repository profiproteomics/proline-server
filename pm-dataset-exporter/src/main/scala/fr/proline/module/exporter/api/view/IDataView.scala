package fr.proline.module.exporter.api.view

trait IDataView {
  
  var viewName: String
  
  def getFieldsNames(): Array[String]
  
  def getAllRecords(): Seq[Map[String,Any]]  
    
  def onEachRecord( recordFormatter: Map[String,Any] => Unit )

}