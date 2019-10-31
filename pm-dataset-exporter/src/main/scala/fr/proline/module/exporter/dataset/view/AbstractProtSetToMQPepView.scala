package fr.proline.module.exporter.dataset.view

import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._

abstract class AbstractProtSetToMQPepView extends AbstractProtSetToPepMatchView with AbstractQuantDatasetView {
  
  protected val mqPepFieldSet = Set(
    FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID,
    FIELD_PSM_QUANT_ELUTION_TIME,
    FIELD_PSM_QUANT_SELECTION_LEVEL
  )
  
  protected val mqPepFieldsConfigs = sheetConfig.fields.filter( f => mqPepFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantPeptideBuildingContext then we return the record as is
    if( !buildingContext.isInstanceOf[MasterQuantPeptideBuildingContext]) {
      return pepMatchRecord
    }
    
    val mqPepBuildingCtx = buildingContext.asInstanceOf[MasterQuantPeptideBuildingContext]
    
    val mqPep = mqPepBuildingCtx.masterQuantPeptide
    val bestQPep = mqPep.getBestQuantPeptide
    
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord

    for (fieldConfig <- mqPepFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID => mqPep.id
        case FIELD_PSM_QUANT_ELUTION_TIME => dcf2.format(bestQPep.elutionTime / 60)
        case FIELD_PSM_QUANT_SELECTION_LEVEL => mqPep.selectionLevel
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }

}

