package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

class ProtSetToAllPepMatchesView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToMQPepView {
  
  override var viewName = "all_prot_set_peptide_matches"
  
  // TODO: factorize this code with ProtSetToBestPepMatchView in AbsractProtSetToPepMatchView (onEachPeptideMatch method)
  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet

    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets.sortBy( - _.peptideSet.score ) ) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend of provider which have been used

        // Representative Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch.getOrElse(protSet.samesetProteinMatches.get.head)

        val protMatchBuildingCtx = new ProtMatchBuildingContext(
          protSet,
          protSet.peptideSet,
          reprProtMatch
        )
        
        // We group sequence matches by peptide id because a given protein may have the same peptide at different locations
        val seqMatchesByPepId = reprProtMatch.sequenceMatches.groupBy(_.getPeptideId)

        for (
          pepInst <- protSet.peptideSet.getPeptideInstances.sortBy(_.peptide.calculatedMass);
          seqMatch <- seqMatchesByPepId(pepInst.peptideId)
        ) {
          require (pepInst.peptideMatches != null, "the peptide matches must be loaded to be able to export them")

          val peptideId = pepInst.peptide.id
          val allPepMatches = pepInst.peptideMatches
          val mqPepOpt = Option(quantDs).flatMap( _.mqPepByPepId.get(peptideId) )

          if (!isQuantDs || mqPepOpt.isEmpty) {

            for (pepMatch <- allPepMatches.sortBy(_.charge)) {
              val identRecordBuildingCtx = new PepMatchBuildingContext(
                pepMatch = pepMatch,
                isInSubset = false,
                protMatch = reprProtMatch,
                seqMatch = seqMatch,
                protMatchBuildingCtx = Some(protMatchBuildingCtx)
              )
              // Format this peptide match with protein set information
              this.formatRecord(identRecordBuildingCtx, recordFormatter)
            }
          } else {
            
            for (pepMatch <- allPepMatches.sortBy(_.charge)) {
              val quantRecordBuildingCtx = new MasterQuantPeptideBuildingContext(
                pepMatch = pepMatch,
                protMatch = reprProtMatch,
                seqMatch = seqMatch,
                protMatchBuildingCtx = Some(protMatchBuildingCtx),
                mqPepOpt.get,
                groupSetupNumber = groupSetupNumber
              )

              // Format this peptide match with protein set information
              this.formatRecord(quantRecordBuildingCtx, recordFormatter)
            }
          }
        }
      }
    }
  }
  
}