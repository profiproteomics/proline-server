package fr.proline.module.exporter.api.view

import scala.collection.mutable.ArrayBuffer

trait IDataView {
  
  var viewName: String
  
  def getFieldsNames(): Array[String]
  
  def getAllRecords(): Seq[Map[String,Any]]  
    
  def onEachRecord( recordFormatter: Map[String,Any] => Unit )

}

trait IFormLikeView extends IDataView {
  
  def getFieldValueMap(): Map[String,Any]
  
  def getAllRecords(): Seq[Map[String,Any]] = Seq(this.getFieldValueMap())
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) = {
    recordFormatter(this.getFieldValueMap)
  }

}

trait IRecordBuildingContext

/**
 * Use to export
 */
trait ITableLikeView extends IDataView {  

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

trait IFixedTableView extends ITableLikeView {
  
  val fields: IViewFieldEnumeration
  
  private lazy val fieldsNames = fields.values.toArray.map(_.toString)  
  def getFieldsNames(): Array[String] = fieldsNames
  
}

trait ICustomTableView extends ITableLikeView {
  
  val fields: IViewFieldEnumeration
  
  private lazy val fieldsNames = fields.values.toArray.map(_.toString)  
  def getFieldsNames(): Array[String] = fieldsNames
  
}