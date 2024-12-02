package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat
import fr.profi.util.collection._
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant.FIELD_PROTEIN_SETS_QUANT_NB_PEPTIDE
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

  protected val protSetBestPSMFieldSet = Set( FIELD_PROTEIN_SETS_QUANT_NB_PEPTIDE  )
  protected val mqProtSetBestPSMFieldsConfigs = sheetConfig.fields.filter(f => protSetBestPSMFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val protToMQPepecord = super.buildRecord(buildingContext)

    // If this building context is not a MasterQuantProteinSetProfileBuildingContext then we return the record as is
    if (!buildingContext.isInstanceOf[MasterQuantPeptideBuildingContext]) {
      return protToMQPepecord
    }

    val mqProtSetPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideBuildingContext]
    var mqProtSetQPepCount = -1
    if(mqProtSetPepBuildingCtx.protMatchBuildingCtx.isDefined) {
      val mqProtSetOpt = quantDs.mqProtSetByProtSetId.get(mqProtSetPepBuildingCtx.protMatchBuildingCtx.get.protSet.id)
      if(mqProtSetOpt.isDefined) {
        val mqProtSet = mqProtSetOpt.get
         mqProtSetQPepCount = if (mqProtSet.properties.isDefined && mqProtSet.properties.get.getSelectionLevelByMqPeptideId().isDefined) {
          val mqPepBySelectionMLevel = mqProtSet.properties.get.getSelectionLevelByMqPeptideId().get
          if (mqPepBySelectionMLevel != null && mqPepBySelectionMLevel.nonEmpty)
            mqPepBySelectionMLevel.count(_._2 >= 2)
          else
            0
        } else
          -1
      }
    }

    val recordBuilder = Map.newBuilder[String, Any]
    recordBuilder ++= protToMQPepecord
    for (fieldConfig <- mqProtSetBestPSMFieldsConfigs) {

      fieldConfig.id match {
        case FIELD_PROTEIN_SETS_QUANT_NB_PEPTIDE => {
          if (mqProtSetQPepCount < 0)
            recordBuilder += fieldConfig.title -> "UNKNOWN"
          else
            recordBuilder += fieldConfig.title -> mqProtSetQPepCount
        }
      }
    }
    recordBuilder.result()
  }
    // TODO: factorize this code with ProtSetToAllPepMatchesView in AbsractProtSetToPepMatchView (onEachPeptideMatch method)
  override def formatView(recordFormatter: Map[String, Any] => Unit): Unit = {

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
        val seqMatchesByPepId = reprProtMatch.sequenceMatches.groupByLong(_.getPeptideId())

        for (
          pepInst <- protSet.peptideSet.getPeptideInstances().sortBy(_.peptide.calculatedMass);
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

            val isValidForProtSet = isPepQuantValidForProtSet(protSet, mqPepOpt.get.id)

            new MasterQuantPeptideBuildingContext(
                pepMatch = bestPepMatch,
                seqMatch = seqMatch,
                protMatchBuildingCtx = Some(protMatchBuildingCtx),
                mqPepOpt.get,
                groupSetupNumber = groupSetupNumber,
                isUsedForProteinQuantification = Some(isValidForProtSet)
              )
            }
          }
          
          // Format this peptide match with protein set information
          this.formatRecord(buildingContext, recordFormatter)
        }//end for all peptides instances
      }//end protein set is to be exported
    } //end go through proteinsets

  }
}