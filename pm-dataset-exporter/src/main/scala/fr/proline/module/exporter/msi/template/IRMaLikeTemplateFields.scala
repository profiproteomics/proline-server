package fr.proline.module.exporter.msi.template

import fr.proline.module.exporter.msi.view.ProtSetToPepMatchViewFields

object IRMaLikeTemplateFields {

  // TODO: define other sets of fields
  val infosFields = Seq()
  val proteinSetsFields = Seq()
  val peptidesFields = Seq(
    ProtSetToPepMatchViewFields.PROTEIN_SET_ID,
    ProtSetToPepMatchViewFields.ACCESSION,
    ProtSetToPepMatchViewFields.IS_PROTEIN_SET_VALIDATED,
    ProtSetToPepMatchViewFields.START,
    ProtSetToPepMatchViewFields.END,
    ProtSetToPepMatchViewFields.RESIDUE_BEFORE,
    ProtSetToPepMatchViewFields.SEQUENCE,
    ProtSetToPepMatchViewFields.RESIDUE_AFTER,
    ProtSetToPepMatchViewFields.MODIFICATIONS,
    ProtSetToPepMatchViewFields.MISSED_CLEAVAGES,
    ProtSetToPepMatchViewFields.RANK,
    ProtSetToPepMatchViewFields.PEPMATCH_SCORE,
    ProtSetToPepMatchViewFields.CALCULATED_MASS,
    ProtSetToPepMatchViewFields.CHARGE,
    ProtSetToPepMatchViewFields.EXPERIMENTAL_MOZ,
    ProtSetToPepMatchViewFields.DELTA_MOZ,
    ProtSetToPepMatchViewFields.FRAGMENT_MATCHES_COUNT,
    ProtSetToPepMatchViewFields.SPECTRUM_TITLE
  )
  val proteinMatchesFields = Seq()
  
  
}