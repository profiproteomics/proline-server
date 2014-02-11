package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration

object BuildResultSummaryView {
  
  private def _builders = Map( 
    ResultSummaryViewTypes.MSI_SEARCH_EXTENDED -> (rsm => new MsiSearchExtendedView(rsm)),
    ResultSummaryViewTypes.PEP_SET_TO_PROT_MATCH -> (rsm => new PepSetToProtMatchView(rsm)),
    ResultSummaryViewTypes.PROT_SET_TO_BEST_PEP_MATCH -> (rsm => new ProtSetToBestPepMatchView(rsm)),
    ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH -> (rsm => new ProtSetToProtMatchView(rsm)),
    ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> (rsm => new ProtSetToTypicalProtMatchView(rsm)),
    ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH_IRMA -> (rsm => new ProtSetToBestPepMatchIrmaView(rsm))
  ).map( e => e._1.asInstanceOf[IViewTypeEnumeration#Value] -> e._2 )

  def apply(rsm: ResultSummary, viewType: IViewTypeEnumeration#Value ): IDatasetView = {    
    _builders(viewType)(rsm)
  }
  
}