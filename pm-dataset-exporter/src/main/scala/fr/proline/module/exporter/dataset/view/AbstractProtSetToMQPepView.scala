package fr.proline.module.exporter.dataset.view

import fr.proline.core.om.model.msi.ProteinSet
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._

abstract class AbstractProtSetToMQPepView extends AbstractProtSetToPepMatchView with AbstractQuantDatasetView {
  
  protected val mqPepFieldSet = Set(
    FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID,
    FIELD_PSM_QUANT_ELUTION_TIME,
    FIELD_PSM_QUANT_SELECTION_LEVEL,
    FIELD_PSM_QUANT_IS_VALID_FOR_PROT_SET
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
    val isQuantValid = if(mqPepBuildingCtx.isUsedForProteinQuantification.isDefined) mqPepBuildingCtx.isUsedForProteinQuantification.get else "NOT DEF"
    val bestQPep = mqPep.getBestQuantPeptide
    
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord
    val isSelected = mqPep.selectionLevel>=2
    for (fieldConfig <- mqPepFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID => mqPep.id
        case FIELD_PSM_QUANT_ELUTION_TIME => dcf2.format(bestQPep.elutionTime / 60)
        case FIELD_PSM_QUANT_SELECTION_LEVEL => isSelected //mqPep.selectionLevel
        case FIELD_PSM_QUANT_IS_VALID_FOR_PROT_SET => isQuantValid
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }

  def isPepQuantValidForProtSet (protSet : ProteinSet,  mqPepId : Long) : Boolean = {

    var isValidForProtSet = true
    require(quantDs.mqProtSetByProtSetId.contains(protSet.id), "quantified protein set not found for peptide matches !")
    val mqProtSet = quantDs.mqProtSetByProtSetId(protSet.id)
    if (mqProtSet.properties.isDefined && mqProtSet.properties.get.getSelectionLevelByMqPeptideId().isDefined) {
      isValidForProtSet = false
      val selLevel = mqProtSet.properties.get.getSelectionLevelByMqPeptideId().get.get(mqPepId)
      if (selLevel.isDefined && selLevel.get >= 2)
        isValidForProtSet = true
    }
    isValidForProtSet
  }

}

