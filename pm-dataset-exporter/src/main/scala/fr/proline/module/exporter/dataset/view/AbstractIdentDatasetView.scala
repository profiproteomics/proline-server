package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import com.typesafe.scalalogging.LazyLogging

import fr.proline.module.exporter.api.view.ICustomTableView
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.CustomViewFields

import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

trait AbstractIdentDatasetView extends ICustomTableView with LazyLogging {
  
  val identDS: IdentDataset
  val sheetConfig: ExportConfigSheet
  val dateFormat: SimpleDateFormat
  val decimalFormat: SmartDecimalFormat
  val titleSep: String
  val exportAllProteinSet: Boolean
  
  lazy val fields = new CustomViewFields( buildCustomFieldsTitles() )
    
  protected def buildCustomFieldsTitles(): Seq[String] = sheetConfig.fields.map(_.title)
  
  protected val dcf1 = new SmartDecimalFormat("0.#", decimalFormat)
  protected val dcf2 = new SmartDecimalFormat("0.##", decimalFormat)
  protected val dcf4 = new SmartDecimalFormat("0.####", decimalFormat)
  protected val dcf6 = new SmartDecimalFormat("0.######", decimalFormat)
}