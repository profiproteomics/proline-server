package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.core.om.model.msq.QuantPeptideIon
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.CustomFieldConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

import scala.collection.mutable

class MasterQuantPeptideIonView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToMQPepView {

  assert(isQuantDs, "this view can only be used for quantitative datasets")

  var viewName = "master_quant_peptide_ion"
  
  private val qPepIonFieldSet = Set(
    FIELD_QUANT_PEPTIDE_ION_BEST_SCORE,
    FIELD_QUANT_PEPTIDE_ION_ELUTION_TIME,
    FIELD_QUANT_PEPTIDE_ION_CORRECTED_ELUTION_TIME
  )
  
  // Override getQcFieldSet in order to generate QuantChannel based columns
  override protected def getQcFieldSet() =  super.getQcFieldSet() ++ qPepIonFieldSet
  
  protected val mqPepIonViewFieldSet = Set(
    FIELD_MASTER_QUANT_PEPTIDE_ION_ID,
    FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ,
    FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE,
    FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME,
    FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID
  ) ++ qPepIonFieldSet
  
  protected val mqPepIonViewFieldsConfigs = sheetConfig.fields.filter( f => mqPepIonViewFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchRecord = super.buildRecord(buildingContext)

    // If this building context is not a MasterQuantPeptideIonBuildingContext then we return the record as is
    if( !buildingContext.isInstanceOf[MasterQuantPeptideIonBuildingContext]) {
      return pepMatchRecord
    }

    val mqPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideIonBuildingContext]

    val mqPepIon = mqPepBuildingCtx.masterQuantPeptideIon
    val childPepMatchById=mqPepBuildingCtx.childPepMatchById
    val qPepIonMap = mqPepIon.quantPeptideIonMap
    val bestPepMatchIdByQcIdOpt = mqPepIon.properties.map(_.getBestPeptideMatchIdMap())

    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord

    def appendQcValuesToRecord(fieldConfig: CustomFieldConfig)( qcValueFn: (Long,QuantPeptideIon) => Any ): Null = {
      for (qcId <- quantDs.qcIds; qPepIon <- qPepIonMap.get(qcId) ) {
        recordBuilder += mkQcFieldTitle(fieldConfig, qcId) -> qcValueFn(qcId, qPepIon)
      }
      null
    }

    for (fieldConfig <- mqPepIonViewFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ID => mqPepIon.id
        case FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE => mqPepIon.charge
        case FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ => mqPepIon.unlabeledMoz
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME => dcf2.format(mqPepIon.elutionTime / 60)
        case FIELD_QUANT_PEPTIDE_ION_BEST_SCORE => {
          appendQcValuesToRecord(fieldConfig) { (qcId, qPepIon) =>
            val bestScoreOpt = bestPepMatchIdByQcIdOpt.flatMap(_.get(qcId).map(childPepMatchById(_).score))
            dcf2.format(bestScoreOpt.orNull)
          }
        }
        case FIELD_QUANT_PEPTIDE_ION_ELUTION_TIME => {
          appendQcValuesToRecord(fieldConfig) { (qcId, qPepIon) =>
            dcf2.format(qPepIon.elutionTime / 60)
          }
        }
        case FIELD_QUANT_PEPTIDE_ION_CORRECTED_ELUTION_TIME => {
          appendQcValuesToRecord(fieldConfig) { (qcId, qPepIon) =>
            dcf2.format(qPepIon.correctedElutionTime / 60)
          }
        }
        case FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID => mqPepIon.lcmsMasterFeatureId.getOrElse(0)
      }

      if( fieldValue != null ) recordBuilder += fieldConfig.title -> fieldValue
    }

    recordBuilder.result()
  }

  override def formatView(recordFormatter: Map[String, Any] => Unit) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet
    val pepMatchById = rs.peptideMatchById

    val childPepMatchesId : mutable.HashSet[Long]= new mutable.HashSet[Long]()
    rsm.proteinSets.map(prS => prS.peptideSet).flatMap(_.getPeptideInstances()).foreach(pepI => {
        val mqPepOpt = quantDs.mqPepByPepId.get(pepI.peptideId)
        if(mqPepOpt.isDefined){
          val pepMIds = mqPepOpt.get.masterQuantPeptideIons.flatMap(_.bestPeptideMatchId).filter(id => {!pepMatchById.contains(id)})
          childPepMatchesId ++= pepMIds //Add bestPepMatch for MQPepIon
          val qPepIonBestPepMatchId =  mqPepOpt.get.masterQuantPeptideIons.flatMap(_.properties.map(_.getBestPeptideMatchIdMap().values)).flatten
          childPepMatchesId ++= qPepIonBestPepMatchId //Add bestPepMatch for QPepIon
        }
    })

    val childPepMatchByIds = quantDs.loadPepMatches(childPepMatchesId.toArray).mapByLong(_.id).toMap

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets.sortBy( - _.peptideSet.score ) ) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend on provider which have been used

        // Typical Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch().getOrElse(protSet.samesetProteinMatches.get.head)
        val seqMatchByPepId = reprProtMatch.sequenceMatches.toLongMapWith { seqMatch => 
          seqMatch.getPeptideId -> seqMatch
        }

        val protMatchBuildingCtx = new ProtMatchBuildingContext(
          protSet,
          protSet.peptideSet,
          reprProtMatch
        )


        for (pepInst <- protSet.peptideSet.getPeptideInstances.sortBy(_.peptide.calculatedMass) ) {

          val peptideId = pepInst.peptide.id
          
          // TODO: DBO => Why this could be false ???
          // VDS Set RuntimeException to catch this case ... to remove once testesd !
          if (!isQuantDs) {
            logger.warn("----> WARNING in MQPepIonView !isQuantDs")
            throw new RuntimeException("----> WARNING in MQPepIonView !isQuantDs")
//            val allPepMatches = pepInst.peptideMatches
//            for (pepMatch <- allPepMatches.sortBy(_.charge) ) {
//
//              val identRecordBuildingCtx = new PepMatchBuildingContext(
//                pepMatch = pepMatch,
//                isInSubset = false,
//                protMatch = reprProtMatch,
//                seqMatch = seqMatchByPepId(peptideId),
//                protMatchBuildingCtx = Some(protMatchBuildingCtx)
//              )
//
//              // Format this peptide match with protein set information
//              this.formatRecord(identRecordBuildingCtx, recordFormatter)
//            }
          } else {
            
            val mqPepOpt = quantDs.mqPepByPepId.get(peptideId)

            for (mqPep <- mqPepOpt; mqPepIon <- mqPep.masterQuantPeptideIons) {
              
              // DBO: WORKAROUND FOR issues #15834, #17582
              val bestPeptMatchId = mqPepIon.bestPeptideMatchId.get
              val parentPepMatchOpt = pepMatchById.get(bestPeptMatchId)
              val parentOrChildBestPepMatchOpt = if (parentPepMatchOpt.isDefined) parentPepMatchOpt  else childPepMatchByIds.get(bestPeptMatchId)
              
              assert(
                parentOrChildBestPepMatchOpt.isDefined,
                s"Can't retrieve the best peptide match with ID = $bestPeptMatchId"
              )
              
              val quantRecordBuildingCtx = new MasterQuantPeptideIonBuildingContext(
                pepMatch = parentOrChildBestPepMatchOpt.get,
                seqMatch = seqMatchByPepId(peptideId),
                protMatchBuildingCtx = Some(protMatchBuildingCtx),
                mqPep,
                mqPepIon,
                childPepMatchByIds,
                groupSetupNumber = groupSetupNumber
              )

              // Format this master quant peptide ion with protein set information
              this.formatRecord(quantRecordBuildingCtx, recordFormatter)
            }
          }
        }
      }
    }

  }

}