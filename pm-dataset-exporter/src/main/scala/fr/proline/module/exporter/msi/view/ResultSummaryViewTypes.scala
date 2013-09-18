package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view.IViewTypeEnumeration

object ResultSummaryViewTypes extends IViewTypeEnumeration {
  val MSI_SEARCH_EXTENDED = Value("MSI_SEARCH_EXTENDED")
  val PEP_SET_TO_PROT_MATCH = Value("PEP_SET_TO_PROT_MATCH")
  val PROT_SET_TO_BEST_PEP_MATCH = Value("PROT_SET_TO_BEST_PEP_MATCH")
  val PROT_SET_TO_PROT_MATCH = Value("PROT_SET_TO_PROT_MATCH")
  val PROT_SET_TO_TYPICAL_PROT_MATCH = Value("PROT_SET_TO_TYPICAL_PROT_MATCH")
}