package fr.proline.module.exporter.commons.config.view

import fr.proline.module.exporter.api.view.IViewTypeEnumeration

object DatasetViewType extends IViewTypeEnumeration {
  val MSI_SEARCH_EXTENDED = Value
  val IMPORT_AND_VALIDATION_PROPS = Value
  val STATISTICS = Value
  val QUANT_CONFIG = Value
  val PEP_SET_TO_PROT_MATCH = Value
  val PROT_SET_TO_PROT_MATCH = Value
  val PROT_SET_TO_TYPICAL_PROT_MATCH = Value
  val PROT_SET_TO_BEST_PEPTIDE_MATCH = Value
  val PROT_SET_TO_ALL_PEPTIDE_MATCHES = Value
  val TYPICAL_PROT_MATCH_TO_ALL_PEP_MATCHES = Value
  val MASTER_QUANT_PEPTIDE_ION = Value
  val MASTER_QUANT_PEPTIDE = Value
  val MASTER_QUANT_REPORTER_ION = Value
  val PTM_CLUSTER = Value
}