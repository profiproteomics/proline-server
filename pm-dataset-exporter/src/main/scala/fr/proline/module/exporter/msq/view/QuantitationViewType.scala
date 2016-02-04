package fr.proline.module.exporter.msq.view

import fr.proline.module.exporter.api.view.IViewTypeEnumeration

/**
 * 4 different types for the quantitation export
 */

object QuantitationViewTypes extends IViewTypeEnumeration {
	val MASTER_QUANT_PEPTIDE_IONS = Value("MASTER_QUANT_PEPTIDE_IONS")
	val MASTER_QUANT_PEPTIDE = Value("MASTER_QUANT_PEPTIDE")
	val MASTER_QUANT_PROTEIN_SETS = Value("MASTER_QUANT_PROTEIN_SETS")
	val BASIC_MASTER_QUANT_PROTEIN_SETS = Value("BASIC_MASTER_QUANT_PROTEIN_SETS")
}