package fr.proline.module.exporter.api.view

import scala.collection.mutable.ArrayBuffer

trait IDataView {
  
  var viewName: String
  
  def getFieldsNames(): Array[String]

  def formatView(recordFormatter: Map[String,Any] => Unit )

}

trait IFormLikeView extends IDataView {

  def getFieldValueMap(): Map[String,Any]

  def formatView(recordFormatter: Map[String,Any] => Unit ) = {
    recordFormatter(this.getFieldValueMap())
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

}

// Must be used when a view as a predefined set of fields
// These fields are described through and IViewFieldEnumeration
trait IFixedTableView extends ITableLikeView {
  
  val fields: IViewFieldEnumeration
  
  private lazy val fieldsNames = fields.values.toArray.map(_.toString)  
  def getFieldsNames(): Array[String] = fieldsNames
  
}

// TODO: move this trait to commons view
trait ICustomTableView extends IFixedTableView {
  override val fields: fr.proline.module.exporter.commons.config.view.CustomViewFields
}