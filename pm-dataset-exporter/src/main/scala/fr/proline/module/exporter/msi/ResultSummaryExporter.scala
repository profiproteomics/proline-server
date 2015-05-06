package fr.proline.module.exporter.msi

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.msi.view.BuildResultSummaryView
import fr.proline.module.exporter.msi.view.IdentDataSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes
import fr.proline.module.exporter.commons.config.ExportConfig

class ResultSummaryExporter(
  val dataView: IDataView,
  val template: IViewTemplate,
  val exportConfig :ExportConfig
) extends XDatasetExporter {
  
  def this(
    ds: IdentDataSet,
    viewType: ResultSummaryViewTypes.Value,
    template: IViewTemplate, 
    exportConfig:ExportConfig
  ) = {
    this(BuildResultSummaryView( ds, viewType), template, exportConfig)
  }
  
}