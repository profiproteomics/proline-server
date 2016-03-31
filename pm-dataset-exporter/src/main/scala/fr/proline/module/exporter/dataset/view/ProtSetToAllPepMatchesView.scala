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
  
  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet

    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend of provider which have been used

        // Representatiive Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch.getOrElse(protSet.samesetProteinMatches.get.head)
        val seqMatchByPepId = reprProtMatch.sequenceMatches.toLongMapWith { seqMatch =>
          (seqMatch.getPeptideId -> seqMatch)
        }

        val protMatchBuildingCtx = new ProtMatchBuildingContext(
          protSet,
          protSet.peptideSet,
          reprProtMatch
        )

        // FIXME: why pepInst.peptideMatches should be null ???
        for (pepInst <- protSet.peptideSet.getPeptideInstances; if pepInst.peptideMatches != null) {

          val peptideId = pepInst.peptide.id
          val allPepMatches = pepInst.peptideMatches
          val mqPepOpt = Option(quantDs).flatMap( _.mqPepByPepId.get(peptideId) )

          if (!isQuantDs || mqPepOpt.isEmpty) {

            for (pepMatch <- allPepMatches) {

              val identRecordBuildingCtx = new PepMatchBuildingContext(
                pepMatch = pepMatch,
                isInSubset = false,
                protMatch = reprProtMatch,
                seqMatch = seqMatchByPepId(peptideId),
                protMatchBuildingCtx = Some(protMatchBuildingCtx)
              )
              // Format this peptide match with protein set information
              this.formatRecord(identRecordBuildingCtx, recordFormatter)
            }
          } else {
            val peptideId = pepInst.peptide.id

            for (pepMatch <- allPepMatches) {
              val quantRecordBuildingCtx = new MasterQuantPeptideBuildingContext(
                pepMatch = pepMatch,
                protMatch = reprProtMatch,
                seqMatch = seqMatchByPepId(peptideId),
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