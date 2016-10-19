package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.LongMap

import fr.profi.util.collection._
import fr.proline.core.om.model.msi.LazyResultSummary
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.ProteinSet
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._

abstract class AbstractProtSetToMQProtSetProfileView extends AbstractProtSetToTypicalProtMatchView with AbstractQuantDatasetView {

  protected val mqProtSetProfileFieldSet = Set(
    FIELD_PROTEIN_SETS_QUANT_STATUS,
    FIELD_PROTEIN_SETS_QUANT_PEPTIDE_NUMBER
  )

  // Override getQcFieldSet in order to generate QuantChannel based columns for fields defined by mqProtSetProfileFieldSet
  override protected def getQcFieldSet() = {
    val superQcFieldSet = super.getQcFieldSet()
    superQcFieldSet ++ mqProtSetProfileFieldSet
  }

  protected val mqProtSetProfileFieldsConfigs = sheetConfig.fields.filter(f => mqProtSetProfileFieldSet.contains(f.id))

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val protMatchRecord = super.buildRecord(buildingContext)

    // If this building context is not a MasterQuantProteinSetProfileBuildingContext then we return the record as is
    if (buildingContext.isInstanceOf[MasterQuantProteinSetProfileBuildingContext] == false) {
      return protMatchRecord
    }

    val mqProtSetProfileBuildingCtx = buildingContext.asInstanceOf[MasterQuantProteinSetProfileBuildingContext]

    val mqProtSet = mqProtSetProfileBuildingCtx.masterQuantProteinSet
    val qProtSetMap = mqProtSet.quantProteinSetMap
    val qcCount = qcIds.length

    def getProteinMatchId(qcId: Long, identRsm: LazyResultSummary): Option[Long] = {
      val protMatchIdOpt = qProtSetMap.get(qcId).flatMap(_.proteinMatchId)

      if (protMatchIdOpt.isDefined) protMatchIdOpt
      else Some(mqProtSet.proteinSet.getRepresentativeProteinMatchId)
    }

    /*def getProteinSet(qcId: Long, identRsm: LazyResultSummary): Option[ProteinSet] = {

      // Try to get QuantProteinSet for current ProteinSet in current QuantChannel
      val qProtSetOpt = qProtSetMap.get(qcId)

      // TODO: remove me
      if (qProtSetOpt.isEmpty) None
      else {
        val protSetId = qProtSetOpt.get.proteinSetId.getOrElse(0L)
        identRsm.proteinSetById.get(protSetId)
      }
    }*/
    
    def getQcPeptidesCount(qcId: Long): Int = {
      
      // If peptideCountByProtMatchIdByQCId is provided (SC mode)
      if (quantDs.peptideCountByProtMatchIdByQCId.isDefined) {
        val pepCountByProtMatchIdByQCId = quantDs.peptideCountByProtMatchIdByQCId.get
        
        // Get QuantProteinSet for current ProteinSet in current QuantChannel
        val quantProtSetOpt = qProtSetMap.get(qcId)
        
        // Retrieve the corresponding protein match id
        var pmId: Long = -1
        if (quantProtSetOpt.isDefined) {
          if (quantProtSetOpt.get.proteinSetId.isDefined) {
            pmId = quantProtSetOpt.get.proteinMatchId.getOrElse(-1)
          }
        }
        
        val qcPeptideNumber = if (pepCountByProtMatchIdByQCId.contains(qcId) == false) 0
        else {
          val pepCountByProtMatchId = pepCountByProtMatchIdByQCId.get(qcId).get
          pepCountByProtMatchId.getOrElse(pmId,0)
        }
        
        qcPeptideNumber
      }
      // Else peptideCountByProtMatchIdByQCId is not provided (XIC mode)
      else if (quantDs.peptideCountByMqProtSetByQCId.isDefined) {
        val pepCountByMqProtSetByQCId = quantDs.peptideCountByMqProtSetByQCId.get

        val qcPeptideNumber = if (pepCountByMqProtSetByQCId.contains(qcId) == false) 0
        else {
          val pepCountByMqProtSet = pepCountByMqProtSetByQCId.get(qcId).get
          pepCountByMqProtSet.getOrElse(mqProtSet.id,0)
        }
        
        qcPeptideNumber
        
      } else 0
    }

    val recordBuilder = Map.newBuilder[String, Any]
    recordBuilder ++= protMatchRecord

    for (fieldConfig <- mqProtSetProfileFieldsConfigs) {

      fieldConfig.id match {
        case FIELD_PROTEIN_SETS_QUANT_STATUS => {
          for (qcId <- qcIds) {
            val identRsm = quantDs.identRsmByQcId(qcId)
            val protMatchIdOpt = getProteinMatchId(qcId, identRsm)

            if (protMatchIdOpt.isDefined) {
              val qcStatusOpt = quantDs.getProteinMatchStatus(identRsm, protMatchIdOpt.get)
              if (qcStatusOpt.isDefined) {
                recordBuilder += Tuple2(mkQcFieldTitle(fieldConfig, qcId), qcStatusOpt.get.toString)
              }
            }
          }
        }
        case FIELD_PROTEIN_SETS_QUANT_PEPTIDE_NUMBER => {
          for (qcId <- qcIds) {
            val qcPepCount = getQcPeptidesCount(qcId)
            recordBuilder += Tuple2(mkQcFieldTitle(fieldConfig, qcId), if(qcPepCount > 0) qcPepCount else "")            
          }
        }
      }

    }

    recordBuilder.result()
  }

}

