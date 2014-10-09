package fr.proline.module.exporter.msi.view

import java.io.File
import java.io.OutputStream
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi.SequenceMatch

trait IPeptideMatchViewFields extends IViewFieldEnumeration {
  val PEPTIDE_ID = Field("peptide_id")
  val SEQUENCE = Field("sequence")
  val MODIFICATIONS = Field("modifications")
  val PEPMATCH_SCORE = Field("score")
  val CALCULATED_MASS = Field("calculated_mass")
  val CHARGE = Field("charge")
  val EXPERIMENTAL_MOZ = Field("experimental_moz")
  val DELTA_MOZ = Field("delta_moz")
  val RT = Field("rt")
  val PEPTIDE_LENGTH = Field("peptide_length")
  val INITIAL_QUERY_ID = Field("initial_query_id")
  //val NOM_DU_MS_QUERY_DATASET = Field("NOM_DU_MS_QUERY_DATASET") => result file ???  
  val MISSED_CLEAVAGES = Field("missed_cleavages")
  val RANK = Field("rank")
  val CD_PRETTY_RANK = Field("cd_pretty_rank")
  //val IS_PSM_VALIDATED = Field("is_psm_validated")  
  val FRAGMENT_MATCHES_COUNT = Field("fragment_matches_count")
  val SPECTRUM_TITLE = Field("spectrum_title")
  val PROTEIN_SETS_COUNT = Field("#protein_sets")
  val PROTEIN_MATCHES_COUNT = Field("#protein_matches")
  val DB_PROTEIN_MATCHES_COUNT = Field("#databank_protein_matches")
  val START = Field("start")
  val END = Field("end")
  val RESIDUE_BEFORE = Field("residue_before")
  val RESIDUE_AFTER = Field("residue_after")
}

object ProtSetToPepMatchViewFields extends IPeptideMatchViewFields {

  val PROTEIN_SET_ID = Field("protein_set_id")
  val ACCESSION = Field("accession")
  val IS_PROTEIN_SET_VALIDATED = Field("is_protein_set_validated")
  val DESCRIPTION = Field("description")
  val PROTEIN_SET_SCORE = Field("protein_set_score")
}

abstract class AbstractPeptideMatchView extends AbstractProtSetToTypicalProtMatchView {

  override val fields = ProtSetToPepMatchViewFields

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {
    // Cast the building context
    val allPepMatchesBuildingCtx = buildingContext.asInstanceOf[PepMatchBuildingContext]

    val protSetBuildingCtxOpt = allPepMatchesBuildingCtx.protMatchBuildingCtx
    var protSetId = -1l
    var protSetScore = -1d
    var protSetValid = "false"
    if (protSetBuildingCtxOpt.isDefined) {
      protSetId = protSetBuildingCtxOpt.get.protSet.id
      protSetScore = "%.1f".format(protSetBuildingCtxOpt.get.protSet.peptideSet.score).toDouble
      protSetValid = protSetBuildingCtxOpt.get.protSet.isValidated.toString
    }
    val protMatch = if (protSetBuildingCtxOpt.isDefined) protSetBuildingCtxOpt.get.protMatch else allPepMatchesBuildingCtx.protMatch

    val pepMatch = allPepMatchesBuildingCtx.pepMatch
    val seqMatch = allPepMatchesBuildingCtx.seqMatch

    val peptide = pepMatch.peptide
    val initialQueryId = Option(pepMatch.msQuery).map(_.initialId).getOrElse(null)
    val experimentalMoz = Option(pepMatch.msQuery).map(_.moz).getOrElse(null)

    val resBefore = if (seqMatch.residueBefore == '\0') '-' else seqMatch.residueBefore
    val resAfter = if (seqMatch.residueAfter == '\0') '-' else seqMatch.residueAfter

    val dbProtMatchesCount = {
      if (identDS.allProtMatchSetByPepId.get(pepMatch.peptideId).isDefined) {
        identDS.allProtMatchSetByPepId.get(pepMatch.peptideId).get.size
      } else
        0
    }

    // Build the full record
    Map(
      fields.PEPTIDE_ID.toString -> peptide.id,
      fields.SEQUENCE.toString -> peptide.sequence,
      fields.MODIFICATIONS.toString -> peptide.readablePtmString,
      fields.MISSED_CLEAVAGES.toString -> pepMatch.missedCleavage,
      fields.RANK.toString -> pepMatch.rank,
      fields.CD_PRETTY_RANK.toString -> pepMatch.cdPrettyRank,
      fields.PEPMATCH_SCORE.toString -> "%.2f".format(pepMatch.score).toDouble,
      //fields.IS_PSM_VALIDATED -> pepMatch.isValidated,
      fields.CALCULATED_MASS.toString -> "%.4f".format(peptide.calculatedMass).toDouble,
      fields.CHARGE.toString -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null),
      fields.EXPERIMENTAL_MOZ.toString -> experimentalMoz,
      fields.DELTA_MOZ.toString -> pepMatch.deltaMoz, // FIXME: to convert in PPM we need the experimentalMoz and thus the msQuery
      fields.RT.toString -> "-",
      fields.PEPTIDE_LENGTH.toString -> peptide.sequence.length,
      fields.INITIAL_QUERY_ID.toString -> initialQueryId,
      fields.FRAGMENT_MATCHES_COUNT.toString -> pepMatch.fragmentMatchesCount,
      fields.SPECTRUM_TITLE.toString -> Option(pepMatch.getMs2Query).map(_.spectrumTitle).getOrElse(""),
      fields.PROTEIN_SETS_COUNT.toString -> identDS.protSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
      fields.PROTEIN_MATCHES_COUNT.toString -> identDS.protMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
      fields.DB_PROTEIN_MATCHES_COUNT.toString -> dbProtMatchesCount,
      fields.START.toString -> seqMatch.start,
      fields.END.toString -> seqMatch.end,
      fields.RESIDUE_BEFORE.toString -> resBefore,
      fields.RESIDUE_AFTER.toString -> resAfter,
      fields.PROTEIN_SET_ID.toString -> protSetId,
      fields.ACCESSION.toString -> protMatch.accession,
      fields.DESCRIPTION.toString -> protMatch.description,
      fields.PROTEIN_SET_SCORE.toString -> protSetScore,
      fields.IS_PROTEIN_SET_VALIDATED.toString -> protSetValid

    ).map(r => r._1.toString -> r._2)

  }

}

class ProtSetToAllPepMatchesView(override val identDS: IdentDataSet) extends AbstractPeptideMatchView {

  override var viewName = "all_prot_set_peptide_matches"

  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    val pepMatchById = rs.peptideMatchById

    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used

      // Typical Protein Match is put first
      val typicalProtMatchId = protSet.getTypicalProteinMatchId

      val typicalProtMatch = if (typicalProtMatchId != 0) {
        protMatchById(typicalProtMatchId)
      } else {
        protMatchById(protSet.getSameSetProteinMatchIds.head)
      }

      val seqMatchByPepId: Map[Long, SequenceMatch] = typicalProtMatch.sequenceMatches.map { seqMatch => (seqMatch.getPeptideId -> seqMatch) }toMap

      val protMatchBuildingCtx = new ProtMatchBuildingContext(
        protSet,
        protSet.peptideSet,
        typicalProtMatch
      )

      protSet.peptideSet.getPeptideInstances.foreach(pepI => {
        val allPepMatchIds = pepI.getPeptideMatchIds
        allPepMatchIds.foreach(pepMatchId => {
          val buildingContext = new PepMatchBuildingContext(
            pepMatch = pepMatchById(pepMatchId),
            protMatch = typicalProtMatch,
            seqMatch = seqMatchByPepId(pepMatchById(pepMatchId).peptideId),
            protMatchBuildingCtx = Some(protMatchBuildingCtx)
          )
          // Format this peptide match with protein set information
          this.formatRecord(buildingContext, recordFormatter)

        })
      })

    }

  }

}