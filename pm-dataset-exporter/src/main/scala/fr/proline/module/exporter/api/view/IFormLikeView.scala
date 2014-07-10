package fr.proline.module.exporter.api.view

trait IFormLikeView extends IDataView {
  
  def getFieldValueMap(): Map[String,Any]
  
  def getAllRecords(): Seq[Map[String,Any]] = Seq(this.getFieldValueMap())
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) = {
    recordFormatter(this.getFieldValueMap)
  }

}