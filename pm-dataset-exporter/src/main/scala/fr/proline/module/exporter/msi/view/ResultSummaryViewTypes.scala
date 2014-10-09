package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view.IViewTypeEnumeration

object ResultSummaryViewTypes extends IViewTypeEnumeration {
  val MSI_SEARCH_EXTENDED = Value("MSI_SEARCH_EXTENDED")
  val IMPORT_AND_VALIDATION_PROPS = Value("IMPORT_AND_VALIDATION_PROPS")
  val STATISTICS = Value("STATISTICS")
  val PEP_SET_TO_PROT_MATCH = Value("PEP_SET_TO_PROT_MATCH")
  val PROT_SET_TO_PROT_MATCH = Value("PROT_SET_TO_PROT_MATCH")
  val PROT_SET_TO_TYPICAL_PROT_MATCH = Value("PROT_SET_TO_TYPICAL_PROT_MATCH")
  val PROT_SET_TO_BEST_PEPTIDE_MATCH = Value("PROT_SET_TO_BEST_PEPTIDE_MATCH")
  val PROT_SET_TO_ALL_PEPTIDE_MATCH = Value("PROT_SET_TO_ALL_PEPTIDE_MATCH")
  val ALL_PEPTIDE_MATCHES = Value("ALL_PEPTIDE_MATCHES")  
  val ALL_PROT_SET_PEPTIDE_MATCHES= Value("ALL_PROT_SET_PEPTIDE_MATCHES")
}