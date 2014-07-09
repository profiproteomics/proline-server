package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration

case class DataSet(
  projectName: String,
  rsm: ResultSummary
  // TODO: represent the result set hierarchy here (merge)
)

object BuildResultSummaryView {
  
  private def _builders: Map[IViewTypeEnumeration#Value, DataSet => IDatasetView ] = Map( 
    ResultSummaryViewTypes.MSI_SEARCH_EXTENDED -> { ds: DataSet => new MsiSearchExtendedView(ds.projectName, ds.rsm) },
    ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS -> { ds: DataSet => new ResultParseAndFiltersView(ds.rsm) },
    ResultSummaryViewTypes.STATISTICS -> { ds: DataSet => new StatisticsView(ds.rsm) },
    ResultSummaryViewTypes.PEP_SET_TO_PROT_MATCH -> { ds: DataSet => new PepSetToProtMatchView(ds.rsm) },
    ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH -> { ds: DataSet => new ProtSetToProtMatchView(ds.rsm) },
    ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: DataSet => new ProtSetToTypicalProtMatchView(ds.rsm) },
    ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: DataSet => new ProtSetToBestPepMatchView(ds.rsm) },    
    ResultSummaryViewTypes.ALL_PEPTIDE_MATCHES -> { ds: DataSet => new AllPepMatchesView(ds.rsm) }
  )

  def apply( ds: DataSet, viewType: IViewTypeEnumeration#Value ): IDatasetView = {
    _builders(viewType)(ds)
  }
  
}