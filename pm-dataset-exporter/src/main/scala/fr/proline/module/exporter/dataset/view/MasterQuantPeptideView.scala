package fr.proline.module.exporter.dataset.view

import fr.profi.util.collection._
import fr.proline.core.om.model.msi.{PeptideInstance, ProteinSet}
import fr.proline.core.om.model.msq.QuantPeptideIon
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.{CustomFieldConfig, ExportConfigSheet}
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

import java.text.SimpleDateFormat
import scala.collection.mutable
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
  override def formatView(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary

    val proteinSets = rsm.proteinSets.filter(exportAllProteinSet || _.isValidated)
    val protSetsByPepInst = new HashMap[Long, ArrayBuffer[ProteinSet]]
    proteinSets.map { protSet =>
      protSet.peptideSet.getPeptideInstances.foreach { pepInst =>
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
      val protSet = protSetsByPepInst.get(pepInst.id).get.maxBy(_.peptideSet.score)
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

        val quantRecordBuildingCtx = new MasterQuantPeptideBuildingContext(
          pepMatch = bestPepMatch,
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

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {
    val pepMatchRecord = super.buildRecord(buildingContext)


    if (!buildingContext.isInstanceOf[MasterQuantPeptideBuildingContext]) {
      return pepMatchRecord
    }

    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord

    val mqPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideBuildingContext]
    val peptideId = mqPepBuildingCtx.masterQuantPeptide.getPeptideId.get
    val proteinSets = identDS.validProtSetsByPeptideId(peptideId)

    val reprProtSet = proteinSets.map(ps => ps.getRepresentativeProteinMatch().getOrElse(ps.samesetProteinMatches.get.head))

    for (fieldConfig <- mqPeptideFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_MASTER_QUANT_PEPTIDE_PROTEIN_SETS => reprProtSet.map(_.accession).mkString(";")
      }
      if (fieldValue != null) recordBuilder += fieldConfig.title -> fieldValue

    }

    recordBuilder.result()
  }
}