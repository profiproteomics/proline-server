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
  
  /*override def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val buildingCtx = buildingContext.asInstanceOf[PepMatchBuildingContext]
    val protMatchRecord = super.buildRecord(buildingCtx.protMatchBuildingCtx.get)
    
    protMatchRecord
  }*/

  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet
    val protMatchById = rs.proteinMatchById
    val pepMatchById = rs.peptideMatchById
    val pepMatchesByPepId = rs.peptideMatches.groupBy(_.peptideId)

    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets.sortBy( - _.peptideSet.score ) ) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend of provider which have been used

        // Representative Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch().getOrElse(protSet.samesetProteinMatches.get.head)
        val seqMatchByPepId = reprProtMatch.sequenceMatches.toLongMapWith { seqMatch =>
          (seqMatch.getPeptideId -> seqMatch)
        }

        val protMatchBuildingCtx = new ProtMatchBuildingContext(
          protSet,
          protSet.peptideSet,
          reprProtMatch
        )

        for (pepInst <- protSet.peptideSet.getPeptideInstances.sortBy(_.peptide.calculatedMass) ) {
          require (pepInst.peptideMatches != null, "the peptide matches must be loaded to be able to export them")
          
          val peptideId = pepInst.peptide.id
          val bestPepMatch = pepInst.peptideMatches.maxBy(_.score)

          val pepMatchBuildingCtx = new PepMatchBuildingContext(
            pepMatch = bestPepMatch,
            isInSubset = false,
            protMatch = reprProtMatch,
            seqMatch = seqMatchByPepId(peptideId),
            protMatchBuildingCtx = Some(protMatchBuildingCtx)
          )
          
          val buildingContext = if (!isQuantDs) pepMatchBuildingCtx
          else {
            val mqPepOpt = quantDs.mqPepByPepId.get(peptideId)
            
            if (mqPepOpt.isEmpty) pepMatchBuildingCtx
            else {
              new MasterQuantPeptideBuildingContext(
                pepMatch = bestPepMatch,
                protMatch = reprProtMatch,
                seqMatch = seqMatchByPepId(peptideId),
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