package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

class ProtSetToBestPepMatchView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToMQPepView {

  var viewName = "prot_set_to_best_pep_match"
  
  // TODO: factorize this code with ProtSetToAllPepMatchesView in AbsractProtSetToPepMatchView (onEachPeptideMatch method)
  override def formatView(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets.sortBy( - _.peptideSet.score ) ) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend of provider which have been used

        // Representative Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch().getOrElse(protSet.samesetProteinMatches.get.head)

        val protMatchBuildingCtx = new ProtMatchBuildingContext(
          protSet,
          protSet.peptideSet,
          reprProtMatch
        )
        
        // We group sequence matches by peptide id because a given protein may have the same peptide at different locations
        val seqMatchesByPepId = reprProtMatch.sequenceMatches.groupByLong(_.getPeptideId)

        for (
          pepInst <- protSet.peptideSet.getPeptideInstances.sortBy(_.peptide.calculatedMass);
          seqMatch <- seqMatchesByPepId(pepInst.peptideId)
        ) {
          require (pepInst.peptideMatches != null, "the peptide matches must be loaded to be able to export them")
          
          val peptideId = pepInst.peptide.id
          val bestPepMatch = pepInst.peptideMatches.filter(_.id.equals(pepInst.bestPeptideMatchId)).head
          val mqPepOpt = Option(quantDs).flatMap( _.mqPepByPepId.get(peptideId) )

          val buildingContext =  { if (!isQuantDs || mqPepOpt.isEmpty) {
            new PepMatchBuildingContext(
              pepMatch = bestPepMatch,
              isInSubset = false,
              seqMatch = seqMatch,
              protMatchBuildingCtx = Some(protMatchBuildingCtx)
            )

          } else {
              new MasterQuantPeptideBuildingContext(
                pepMatch = bestPepMatch,
                seqMatch = seqMatch,
                protMatchBuildingCtx = Some(protMatchBuildingCtx),
                mqPepOpt.get,
                groupSetupNumber = groupSetupNumber
              )
            }
          }
          
          // Format this peptide match with protein set information
          this.formatRecord(buildingContext, recordFormatter)
        }
      }
    }

  }
}