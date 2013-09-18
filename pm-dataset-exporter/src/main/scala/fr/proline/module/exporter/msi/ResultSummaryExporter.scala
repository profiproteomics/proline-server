package fr.proline.module.exporter.msi

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.msi.view.BuildResultSummaryView
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes
import fr.proline.module.exporter.api.view.IDatasetView

class ResultSummaryExporter(
  val datasetView: IDatasetView,
  val template: IViewTemplate
) extends XDatasetExporter {
  
  def this(
    rsm: ResultSummary,
    viewType: ResultSummaryViewTypes.Value,
    template: IViewTemplate
  ) = {
    this(BuildResultSummaryView( rsm, viewType ), template)
  }
  
}