package fr.proline.module.exporter.dataset.view

import fr.proline.core.om.model.msi.ProteinSet
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

import java.text.SimpleDateFormat
import scala.collection.mutable.{ArrayBuffer, HashMap}

class MasterQuantPeptideView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToMQPepView {

  assert(isQuantDs, "this view can only be used for quantitative datasets")

  var viewName = "master_quant_peptide"

  protected val mqPeptideFieldSet = Set(
    FIELD_MASTER_QUANT_PEPTIDE_PROTEIN_SETS
  )

  protected val mqPeptideFieldsConfigs = sheetConfig.fields.filter( f => mqPeptideFieldSet.contains(f.id) )

  // TODO: factorize this code with ProtSetToBestPepMatchView in AbsractProtSetToPepMatchView (onEachPeptideMatch method)
  override def formatView(recordFormatter: Map[String, Any] => Unit) : Unit = {

    val rsm = identDS.resultSummary

    val proteinSets = rsm.proteinSets.filter(exportAllProteinSet || _.isValidated)
    val protSetsByPepInst = new HashMap[Long, ArrayBuffer[ProteinSet]]
    proteinSets.map { protSet =>
      protSet.peptideSet.getPeptideInstances().foreach { pepInst =>
        protSetsByPepInst.getOrElseUpdate(pepInst.id, new ArrayBuffer[ProteinSet]()) += protSet
      }
    }

    val pepInstances = proteinSets.map(_.peptideSet).flatMap(_.getPeptideInstances()).distinct;


    // Iterate over RSM protein sets
    for (pepInst <- pepInstances) {

      require(pepInst.peptideMatches != null, "the peptide matches must be loaded to be able to export them")

      val peptideId = pepInst.peptide.id
      val bestPepMatch = pepInst.peptideMatches.find(_.id == pepInst.bestPeptideMatchId).getOrElse(pepInst.peptideMatches.maxBy(_.score))
      val mqPepOpt = Option(quantDs).flatMap(_.mqPepByPepId.get(peptideId))

      // these values are necessary for inherited methods (due to incorrect inheritance design) but the fields extracted from this value
      // wont be used in this export view.
      val protSet = protSetsByPepInst(pepInst.id).maxBy(_.peptideSet.score)
      val reprProtMatch = protSet.getRepresentativeProteinMatch().getOrElse(protSet.samesetProteinMatches.get.head)
      val seqMatch = reprProtMatch.sequenceMatches.filter(_.getPeptideId() == peptideId).head

      val protMatchBuildingCtx = new ProtMatchBuildingContext(
        protSet,
        protSet.peptideSet,
        reprProtMatch
      )

      if (!isQuantDs || mqPepOpt.isEmpty) {
        val identRecordBuildingCtx = new PepMatchBuildingContext(
          pepMatch = bestPepMatch,
          isInSubset = false,
          seqMatch = seqMatch,
          protMatchBuildingCtx = Some(protMatchBuildingCtx)
        )
        // Format this peptide match with protein set information
        this.formatRecord(identRecordBuildingCtx, recordFormatter)
      } else {

        val proteinSets = identDS.validProtSetsByPeptideId(peptideId)
        var quantProtSetList = ""
        for(nextProtSet <- proteinSets){
          if (quantDs.mqProtSetByProtSetId.contains(nextProtSet.id)) {
            val mqProtSet = quantDs.mqProtSetByProtSetId(nextProtSet.id)
            if(mqProtSet.properties.isDefined && mqProtSet.properties.get.getSelectionLevelByMqPeptideId().isDefined) {
              val selLevel = mqProtSet.properties.get.getSelectionLevelByMqPeptideId().get.get(mqPepOpt.get.id)
              if(selLevel.isDefined && selLevel.get>=2)
                quantProtSetList = quantProtSetList+nextProtSet.getRepresentativeProteinMatch().get.accession+';'
            }
          }
        }

        val quantRecordBuildingCtx = new MasterQuantPeptideBuildingContext(
          pepMatch = bestPepMatch,
          seqMatch = seqMatch,
          protMatchBuildingCtx = Some(protMatchBuildingCtx),
          mqPepOpt.get,
          groupSetupNumber = groupSetupNumber,
          quantProtSetList
        )

        // Format this peptide match with protein set information
        this.formatRecord(quantRecordBuildingCtx, recordFormatter)
      }
    }
  }

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {
    val pepMatchRecord = super.buildRecord(buildingContext)


    if (!buildingContext.isInstanceOf[MasterQuantPeptideBuildingContext]) {
      return pepMatchRecord
    }

    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord

    val mqPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideBuildingContext]

    val reprProtSet = mqPepBuildingCtx.quantProtListAccession

      for (fieldConfig <- mqPeptideFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_MASTER_QUANT_PEPTIDE_PROTEIN_SETS => reprProtSet
      }
      if (fieldValue != null) recordBuilder += fieldConfig.title -> fieldValue

    }

    recordBuilder.result()
  }
}