package fr.proline.module.exporter.msi

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.msi.view.BuildResultSummaryView
import fr.proline.module.exporter.msi.view.DataSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes

class ResultSummaryExporter(
  val datasetView: IDatasetView,
  val template: IViewTemplate
) extends XDatasetExporter {
  
  def this(
    ds: DataSet,
    viewType: ResultSummaryViewTypes.Value,
    template: IViewTemplate
  ) = {
    this(BuildResultSummaryView( ds, viewType ), template)
  }
  
}