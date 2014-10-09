package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.msi.view.PeptideViewFields

object IRMaLikeTemplateFields {

  // TODO: define other sets of fields
  val infosFields = Seq()
  val proteinSetsFields = Seq()
  val peptidesFields = Seq(
    PeptideViewFields.PROTEIN_SET_ID,
    PeptideViewFields.ACCESSION,
    PeptideViewFields.IS_PROTEIN_SET_VALIDATED,
    PeptideViewFields.START,
    PeptideViewFields.END,
    PeptideViewFields.RESIDUE_BEFORE,
    PeptideViewFields.SEQUENCE,
    PeptideViewFields.RESIDUE_AFTER,
    PeptideViewFields.MODIFICATIONS,
    PeptideViewFields.MISSED_CLEAVAGES,
    PeptideViewFields.RANK,
    PeptideViewFields.PEPMATCH_SCORE,
    PeptideViewFields.CALCULATED_MASS,
    PeptideViewFields.CHARGE,
    PeptideViewFields.EXPERIMENTAL_MOZ,
    PeptideViewFields.DELTA_MOZ,
    PeptideViewFields.FRAGMENT_MATCHES_COUNT,
    PeptideViewFields.SPECTRUM_TITLE
  )
  val proteinMatchesFields = Seq()
  
  
}