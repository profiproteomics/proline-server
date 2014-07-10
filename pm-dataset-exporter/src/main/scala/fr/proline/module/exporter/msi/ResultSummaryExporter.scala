package fr.proline.module.exporter.msi

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.msi.view.BuildResultSummaryView
import fr.proline.module.exporter.msi.view.IdentDataSet
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes

class ResultSummaryExporter(
  val dataView: IDataView,
  val template: IViewTemplate
) extends XDatasetExporter {
  
  def this(
    ds: IdentDataSet,
    viewType: ResultSummaryViewTypes.Value,
    template: IViewTemplate
  ) = {
    this(BuildResultSummaryView( ds, viewType ), template)
  }
  
}