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

class MasterQuantPeptideIonView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToMQPepView {
  
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
  
  protected lazy val identPepMatchById = identDS.childResultSummaries.flatMap { childRsm =>
    childRsm.lazyResultSet.peptideMatchById
  } toLongMap()

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantPeptideIonBuildingContext then we return the record as is
    if( buildingContext.isInstanceOf[MasterQuantPeptideIonBuildingContext] == false) {
      return pepMatchRecord
    }
    
    val mqPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideIonBuildingContext]
    
    val mqPepIon = mqPepBuildingCtx.masterQuantPeptideIon
    val qPepIonMap = mqPepIon.quantPeptideIonMap
    val bestPepMatchIdByQcId = mqPepIon.properties.get.getBestPeptideMatchIdMap()

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
            val bestScoreOpt = bestPepMatchIdByQcId.get(qcId).map(identPepMatchById(_).score)
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
  
  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet
    val pepMatchById = rs.peptideMatchById

    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets.sortBy( - _.peptideSet.score ) ) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend on provider which have been used

        // Typical Protein Match is put first
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

          val peptideId = pepInst.peptide.id
          
          // TODO: DBO => Why this could be false ???
          if (!isQuantDs) {

            val allPepMatches = pepInst.peptideMatches
            for (pepMatch <- allPepMatches.sortBy(_.charge) ) {

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
            
            val mqPepOpt = quantDs.mqPepByPepId.get(peptideId)

            for (mqPep <- mqPepOpt; mqPepIon <- mqPep.masterQuantPeptideIons) {
              
              val quantRecordBuildingCtx = new MasterQuantPeptideIonBuildingContext(
                pepMatch = pepMatchById(mqPepIon.bestPeptideMatchId.get),
                protMatch = reprProtMatch,
                seqMatch = seqMatchByPepId(peptideId),
                protMatchBuildingCtx = Some(protMatchBuildingCtx),
                mqPep,
                mqPepIon,
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