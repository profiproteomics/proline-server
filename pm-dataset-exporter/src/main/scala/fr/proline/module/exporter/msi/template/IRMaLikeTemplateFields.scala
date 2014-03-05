package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.msi.view.{ ProtSetToBestPepMatchViewFields => PeptideViewFields }

object IRMaLikeTemplateFields {

  // TODO: define other sets of fields
  val infosFields = Seq()
  val proteinSetsFields = Seq()
  val peptidesFields = Seq(
    PeptideViewFields.PROTEIN_SET_ID,
    PeptideViewFields.ACCESSION,
    PeptideViewFields.SEQUENCE,
    PeptideViewFields.PEPMATCH_SCORE,
    PeptideViewFields.CALCULATED_MASS,
    PeptideViewFields.MISSED_CLEAVAGES,
    PeptideViewFields.EXPERIMENTAL_MOZ,
    PeptideViewFields.DELTA_MOZ,
    PeptideViewFields.RANK,
    PeptideViewFields.FRAGMENT_MATCHES_COUNT,
    PeptideViewFields.SPECTRUM_TITLE
  )
  val proteinMatchesFields = Seq()
  
  
}