package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

import scala.collection.mutable.ArrayBuffer

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
  
  // Override getQcFieldSet in order to generate QuantChannel based columns
  override protected def getQcFieldSet() =  super.getQcFieldSet()// ++ qRepIonFieldSet
  
  private val mqRepIonViewFieldSet = Set(
    FIELD_MASTER_QUANT_PEPTIDE_ION_ID,
    FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ,
    FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE,
    FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME
  ) //++ qRepIonFieldSet
  
  private val mqRepIonViewFieldsConfigs = sheetConfig.fields.filter(f => mqRepIonViewFieldSet.contains(f.id) )


  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantReporterIonBuildingContext then we return the record as is
    if (!buildingContext.isInstanceOf[MasterQuantReporterIonBuildingContext]) {
      return pepMatchRecord
    }
    
    val mqRepIonBuildingCtx = buildingContext.asInstanceOf[MasterQuantReporterIonBuildingContext]
    
    val mqPepIon = mqRepIonBuildingCtx.masterQuantPeptideIon


    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord
    

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
  
  override def formatView(recordFormatter: Map[String, Any] => Unit): Unit = {
    
    val rsm = identDS.resultSummary

    val msQueryIds : ArrayBuffer[Long]= new ArrayBuffer[Long]()
    rsm.proteinSets.map(prS => prS.peptideSet).flatMap(_.getPeptideInstances()).foreach(pepI => {
      val mqPepOpt = quantDs.mqPepByPepId.get(pepI.peptideId)
      for (
        mqPep <- mqPepOpt;
        mqPepIon <- mqPep.masterQuantPeptideIons;
        mqRepIon <- mqPepIon.masterQuantReporterIons
      ) { msQueryIds += mqRepIon.msQueryId  }
    })

    val childPepMatchById = quantDs.loadPepMatchesByMsQIs(msQueryIds.toArray).mapByLong(_.id).toMap
    val childPepMatchByMsQueryIds = childPepMatchById.values.toArray.groupBy(_.msQueryId)

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

        for (pepInst <- protSet.peptideSet.getPeptideInstances().sortBy(_.peptide.calculatedMass)) {

          val peptideId = pepInst.peptide.id
          val mqPepOpt = quantDs.mqPepByPepId.get(peptideId)
          val isValidForProtSetopt = if(mqPepOpt.isDefined) Some(isPepQuantValidForProtSet(protSet, mqPepOpt.get.id)) else None

          for (
            mqPep <- mqPepOpt;
            mqPepIon <- mqPep.masterQuantPeptideIons;
            mqRepIon <- mqPepIon.masterQuantReporterIons
          ) {

            val identPepMatchList = childPepMatchByMsQueryIds(mqRepIon.msQueryId).filter(pepMatch => pepMatch.peptideId == peptideId)
            val identPepMatchOpt = if(identPepMatchList.length<=0)
              None
            else if(identPepMatchList.length ==1)
              Some(identPepMatchList(0))
            else {
              identPepMatchList.find(pepMatch => pepMatch.resultSetId == quantDs.quantRSM.lazyResultSummary.resultSetId)
            }

            assert(identPepMatchOpt.isDefined, "can't find a peptide match for the MS query id = " + mqRepIon.msQueryId)
            
            val quantRecordBuildingCtx = new MasterQuantReporterIonBuildingContext(
              pepMatch = identPepMatchOpt.get,
              seqMatch = seqMatchByPepId(peptideId),
              protMatchBuildingCtx = Some(protMatchBuildingCtx),
              mqPep,
              mqPepIon,
              childPepMatchById,
              mqRepIon,
              groupSetupNumber = groupSetupNumber,
              isValidForProtSetopt
            )

            // Format this master quant peptide ion with protein set information
            this.formatRecord(quantRecordBuildingCtx, recordFormatter)
          }
        }
      }
    }

  }

}