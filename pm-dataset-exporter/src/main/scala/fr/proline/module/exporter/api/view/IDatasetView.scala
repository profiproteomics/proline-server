package fr.proline.module.exporter.api.view

import scala.collection.mutable.ArrayBuffer

trait IRecordBuildingContext

trait IDatasetView {
  
  var viewName: String
  val fields: IViewFieldEnumeration
  
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any]
  def formatRecord( buildingContext: IRecordBuildingContext, recordFormatter: Map[String,Any] => Unit ) {
    recordFormatter( this.buildRecord(buildingContext) )
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit )
  
  def getAllRecords(): Seq[Map[String,Any]] = {
    val records = new ArrayBuffer[Map[String,Any]]()
    
    this.onEachRecord( record => {
      records += record
    } )

    records
  }
  
  def formatFloat( float: Float ): String = "%.2f".format(float)

}