package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.module.exporter.api.view.IRecordBuildingContext
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
  
  protected val mqPepIonFieldSet = Set(
    FIELD_MASTER_QUANT_PEPTIDE_ION_ID,
    FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE,
    FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME,
    FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID
  )
  
  protected val mqPepIonFieldsConfigs = sheetConfig.fields.filter( f => mqPepIonFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantPeptideIonBuildingContext then we return the record as is
    if( buildingContext.isInstanceOf[MasterQuantPeptideIonBuildingContext] == false) {
      return pepMatchRecord
    }
    
    val mqPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideIonBuildingContext]
    
    val mqPepIon = mqPepBuildingCtx.masterQuantPeptideIon

    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord

    for (fieldConfig <- mqPepIonFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ID => mqPepIon.id
        case FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE => mqPepIon.charge
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME => dcf2.format(mqPepIon.elutionTime / 60)
        case FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID => mqPepIon.lcmsMasterFeatureId.get
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
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
    for (protSet <- rsm.proteinSets) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend on provider which have been used

        // Typical Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch().getOrElse(protSet.samesetProteinMatches.get.head)
        val seqMatchByPepId = reprProtMatch.sequenceMatches.toLongMap { seqMatch => 
          (seqMatch.getPeptideId -> seqMatch)
        }

        val protMatchBuildingCtx = new ProtMatchBuildingContext(
          protSet,
          protSet.peptideSet,
          reprProtMatch
        )

        for (pepInst <- protSet.peptideSet.getPeptideInstances) {

          val peptideId = pepInst.peptide.id
          
          // TODO: DBO => Why this could be false ???
          if (!isQuantDs) {

            val allPepMatches = pepInst.peptideMatches
            for (pepMatch <- allPepMatches) {

              val identRecordBuildingCtx = new PepMatchBuildingContext(
                pepMatch = pepMatch,
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