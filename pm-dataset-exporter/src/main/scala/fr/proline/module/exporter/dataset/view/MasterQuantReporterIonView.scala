package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

class MasterQuantReporterIonView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,  // TODO: remove ?
  val exportBestProfile: Boolean // TODO: remove ?
) extends AbstractProtSetToMQPepView {
  
  assert(isQuantDs, "this view can only be used for quantitative datasets")
  
  var viewName = "master_quant_reporter_ion"
  
  /*private val qRepIonFieldSet = Set(
    FIELD_QUANT_PEPTIDE_ION_BEST_SCORE
  )*/
  
  // Override getQcFieldSet in order to generate QuantChannel based columns
  override protected def getQcFieldSet() =  super.getQcFieldSet()// ++ qRepIonFieldSet
  
  protected val mqRepIonViewFieldSet = Set(
    FIELD_MASTER_QUANT_PEPTIDE_ION_ID,
    FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ,
    FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE,
    FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME
  ) //++ qRepIonFieldSet
  
  protected val mqRepIonViewFieldsConfigs = sheetConfig.fields.filter( f => mqRepIonViewFieldSet.contains(f.id) )
  
  protected lazy val identPepMatchesByMsQueryId = identDS.childResultSummaries.flatMap { childRsm =>
    childRsm.lazyResultSet.peptideMatches
  } groupByLong { pepMatch => pepMatch.msQueryId }

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantReporterIonBuildingContext then we return the record as is
    if (!buildingContext.isInstanceOf[MasterQuantReporterIonBuildingContext]) {
      return pepMatchRecord
    }
    
    val mqRepIonBuildingCtx = buildingContext.asInstanceOf[MasterQuantReporterIonBuildingContext]
    
    val mqPepIon = mqRepIonBuildingCtx.masterQuantPeptideIon
    val mqRepIon = mqRepIonBuildingCtx.masterQuantReporterIon
    val qRepIonMap = mqRepIon.quantReporterIonMap

    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord
    
    /*def appendQcValuesToRecord(fieldConfig: CustomFieldConfig)( qcValueFn: (Long,QuantReporterIon) => Any ): Null = {
      for (qcId <- quantDs.qcIds; qRepIon <- qRepIonMap.get(qcId) ) {
        recordBuilder += mkQcFieldTitle(fieldConfig, qcId) -> qcValueFn(qcId, qRepIon)
      }
      null
    }*/

    for (fieldConfig <- mqRepIonViewFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ID => mqPepIon.id
        case FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE => mqPepIon.charge
        case FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ => mqPepIon.unlabeledMoz
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME => dcf2.format(mqPepIon.elutionTime / 60)
      }
      
      if( fieldValue != null ) recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }
  
  override def formatView(recordFormatter: Map[String, Any] => Unit) {
    
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
          val mqPepOpt = quantDs.mqPepByPepId.get(peptideId)

          for (
            mqPep <- mqPepOpt;
            mqPepIon <- mqPep.masterQuantPeptideIons;
            mqRepIon <- mqPepIon.masterQuantReporterIons
          ) {
            
            val identPepMatches = identPepMatchesByMsQueryId(mqRepIon.msQueryId)
            val identPepMatchOpt = identPepMatches.find(pepMatch => pepMatch.peptideId == peptideId)
            assert(identPepMatchOpt.isDefined, "can't find a peptide match for the MS query id = " + mqRepIon.msQueryId)
            
            val quantRecordBuildingCtx = new MasterQuantReporterIonBuildingContext(
              pepMatch = identPepMatchOpt.get,
              protMatch = reprProtMatch,
              seqMatch = seqMatchByPepId(peptideId),
              protMatchBuildingCtx = Some(protMatchBuildingCtx),
              mqPep,
              mqPepIon,
              mqRepIon,
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