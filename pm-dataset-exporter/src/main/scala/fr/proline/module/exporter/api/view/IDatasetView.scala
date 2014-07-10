package fr.proline.module.exporter.api.view

import scala.collection.mutable.ArrayBuffer

trait IRecordBuildingContext

trait IDatasetView extends IDataView {  

  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any]
  def formatRecord( buildingContext: IRecordBuildingContext, recordFormatter: Map[String,Any] => Unit ) {
    recordFormatter( this.buildRecord(buildingContext) )
  }
  
  def getAllRecords(): Seq[Map[String,Any]] = {
    val records = new ArrayBuffer[Map[String,Any]]()
    
    this.onEachRecord( record => {
      records += record
    } )

    records
  }
  
}

trait IFixedDatasetView extends IDatasetView {
  
  val fields: IViewFieldEnumeration
  
  private lazy val fieldsNames = fields.values.toArray.map(_.toString)  
  def getFieldsNames(): Array[String] = fieldsNames
  
}