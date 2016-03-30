package fr.proline.module.exporter.dataset.view

import fr.proline.core.om.model.msi.LazyResultSummary
import fr.proline.core.om.model.msi.ProteinMatch
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
  
  protected val mqProtSetProfileFieldsConfigs = sheetConfig.fields.filter( f => mqProtSetProfileFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val protMatchRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantProteinSetProfileBuildingContext then we return the record as is
    if( buildingContext.isInstanceOf[MasterQuantProteinSetProfileBuildingContext] == false) {
      return protMatchRecord
    }
    
    val mqProtSetProfileBuildingCtx = buildingContext.asInstanceOf[MasterQuantProteinSetProfileBuildingContext]
    
    val mqProtSet = mqProtSetProfileBuildingCtx.masterQuantProteinSet
    val qProtSetMap = mqProtSet.quantProteinSetMap
    
    def getProteinMatch(qcId: Long, identRsm: LazyResultSummary): Option[ProteinMatch] = {
      val identRsm = quantDs.identRsmByQcId(qcId)
      
      // Try to get QuantProteinSet for current ProteinSet in current QuantChannel
      val protMatchIdOpt = qProtSetMap.get(qcId).flatMap( _.proteinMatchId )
      
      // TODO: remove me
      val protMatchOpt = if( protMatchIdOpt.isEmpty ) {
        val reprProtMatchOpt = mqProtSet.proteinSet.getRepresentativeProteinMatch()
        if (reprProtMatchOpt.isEmpty) None
        else quantDs.getIdentifiedProteinMatchByAc(identRsm, reprProtMatchOpt.get.accession)
      } else {
        identRsm.lazyResultSet.proteinMatchById.get(protMatchIdOpt.get)
      }
      
      protMatchOpt
    }
    
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= protMatchRecord
    
    for (fieldConfig <- mqProtSetProfileFieldsConfigs) {
      
      fieldConfig.id match {
        case FIELD_PROTEIN_SETS_QUANT_STATUS => {
          for( qcId <- qcIds ) {
            val identRsm = quantDs.identRsmByQcId(qcId)
            val protMatchOpt = getProteinMatch(qcId, identRsm)
            
            if(protMatchOpt.isDefined) {
              
              val qcStatusOpt = quantDs.getProteinMatchStatus(identRsm, protMatchOpt.get.id)
              if( qcStatusOpt.isDefined ) {
                recordBuilder += Tuple2(mkQcFieldTitle(fieldConfig,qcId), qcStatusOpt.get.toString)
              }
            }
          }
        }
        case FIELD_PROTEIN_SETS_QUANT_PEPTIDE_NUMBER => {
          for( qcId <- qcIds ) {
            val identRsm = quantDs.identRsmByQcId(qcId)
            val protMatchOpt = getProteinMatch(qcId, identRsm)
            
            if(protMatchOpt.isDefined) {
              val protMatchPepCount = protMatchOpt.map(_.peptidesCount).getOrElse(0)
              
              recordBuilder += Tuple2(mkQcFieldTitle(fieldConfig,qcId), protMatchPepCount)
            }
          }
        }
      }
      
    }
    
    recordBuilder.result()
  }

}

