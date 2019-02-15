package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.ArrayBuffer

import fr.proline.core.om.model.msq.ComputedRatio
import fr.proline.core.om.model.msq.QuantComponent
import fr.proline.core.om.model.msq.RatioDefinition

import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.CustomFieldConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.dataset.QuantDataset

trait AbstractQuantDatasetView extends AbstractIdentDatasetView {
  
  val exportBestProfile: Boolean
  
  protected val quantDs = identDS match {
    case qds: QuantDataset => qds
    case _ => null
  }
  protected val isQuantDs = quantDs != null
  protected val qcIds = if(quantDs != null) quantDs.qcIds else null
  protected val ratioDefs = if(quantDs != null) quantDs.ratioDefs else null

  // TODO: retrieve the right value
  protected val groupSetupNumber = 1
  
  protected val qcFieldSet = Set(
    FIELD_PROTEIN_SETS_QUANT_ABUNDANCE,
    FIELD_PROTEIN_SETS_QUANT_RAW_ABUNDANCE,
    FIELD_PROTEIN_SETS_QUANT_PSM_COUNT
  )
  protected val profilizerFieldSet = Set(
    FIELD_PROFILIZER_RATIO,
    FIELD_PROFILIZER_TTEST_PVALUE,
    FIELD_PROFILIZER_ZTEST_PVALUE,
    FIELD_PROFILIZER_ZSCORE
  )
  
  // Define two methods which may be overridden my classes using this trait
  // These FieldSets are used by buildCustomFieldsTitles() to generate columns for each QuantChannel or each RatioDefinition
  protected def getQcFieldSet() = qcFieldSet
  protected def getProfilizerFieldSet() = profilizerFieldSet
  
  override protected def buildCustomFieldsTitles() = {
    val givenQcFieldSet = getQcFieldSet()
    val givenProfilizerFieldSet = getProfilizerFieldSet()
    
    val fieldsTitles = new ArrayBuffer[String](sheetConfig.fields.length)
    for (fieldConfig <- sheetConfig.fields ) {
  
      val fieldTitle = fieldConfig.title
  
      if (!isQuantDs) fieldsTitles += fieldTitle
      else {
        val fieldId = fieldConfig.id
        
        if (givenQcFieldSet.contains(fieldId)) {
          for (qcId <- quantDs.qcIds) {
            fieldsTitles += mkQcFieldTitle(fieldTitle, qcId)
          }
        } else if (givenProfilizerFieldSet.contains(fieldId)) {
          for (r <- ratioDefs.get) {
            fieldsTitles += mkRatioTitle(fieldTitle, r)
          }
        } else {
          fieldsTitles += fieldTitle
        }
      }
    }
    
    fieldsTitles
  }
  
  protected val quantEntityFieldSet = qcFieldSet ++ profilizerFieldSet  
  protected val quantEntityFieldsConfigs = sheetConfig.fields.filter( f => quantEntityFieldSet.contains(f.id) )

  abstract override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val identDatasetRecord = super.buildRecord(buildingContext)
    
    // If this building context is not a MasterQuantProteinSetProfileBuildingContext then we return the record as is
    if( !buildingContext.isInstanceOf[IMasterQuantEntityBuildingContext]) {
      return identDatasetRecord
    }
    
    val quantEntityBuildingCtx = buildingContext.asInstanceOf[IMasterQuantEntityBuildingContext]
    
    val qComponentMap = quantEntityBuildingCtx.getQuantComponentMap()
    val ratiosWithDefOpt = if(ratioDefs.isEmpty) None
    else quantEntityBuildingCtx.getRatios.map( _.zip(ratioDefs.get) )
    
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= identDatasetRecord
    
    def addQuantComponentsProperty( fieldConfig: CustomFieldConfig, propertyExtractor: (QuantComponent) => Any) {
      for (qcId <- qcIds; qComponent <- qComponentMap.get(qcId) ) {
        recordBuilder += Tuple2(mkQcFieldTitle(fieldConfig, qcId), propertyExtractor(qComponent) )
      }
    }
    
    def addRatiosProperty( fieldConfig: CustomFieldConfig, propertyExtractor: (ComputedRatio) => Option[Any]) {
      for (
        ratiosWithDef <- ratiosWithDefOpt;
        (ratioOpt, ratioDef) <- ratiosWithDef;
        ratio <- ratioOpt
      ) {
        val propOpt = propertyExtractor(ratio)
        if( propOpt.isDefined ) {
          recordBuilder += Tuple2(mkRatioTitle(fieldConfig, ratioDef), propOpt.get)
        }
      }
    }

    for (fieldConfig <- quantEntityFieldsConfigs) {

      fieldConfig.id match {
        case FIELD_PROTEIN_SETS_QUANT_RAW_ABUNDANCE => {
          addQuantComponentsProperty(fieldConfig, q => dcf2.format(q.rawAbundance) )
        }
        case FIELD_PROTEIN_SETS_QUANT_ABUNDANCE => {
          addQuantComponentsProperty(fieldConfig, q => dcf2.format(q.abundance) )
        }
        case FIELD_PROTEIN_SETS_QUANT_PSM_COUNT => {
          addQuantComponentsProperty(fieldConfig, _.peptideMatchesCount)
        }
        case FIELD_PROFILIZER_RATIO => {
          addRatiosProperty(fieldConfig, r => Some(dcf2.format(r.ratioValue)) )
        }
        case FIELD_PROFILIZER_TTEST_PVALUE => {
          addRatiosProperty(fieldConfig, _.getTTestPValue.map(dcf6.format(_)) )
        }
        case FIELD_PROFILIZER_ZTEST_PVALUE => {
          addRatiosProperty(fieldConfig, _.getZTestPValue.map(dcf6.format(_)) )
        }
        case FIELD_PROFILIZER_ZSCORE => {
          addRatiosProperty(fieldConfig, _.getZScore.map(dcf2.format(_)) )
        }
      }
    }
    
    recordBuilder.result()
  }
  
  protected def mkRatioTitle(fieldConfig: CustomFieldConfig, r: RatioDefinition): String = mkRatioTitle(fieldConfig.title, r)
  
  protected def mkRatioTitle(fieldTitle: String, r: RatioDefinition): String = {
    val(v1, v2) = (r.numeratorGroupNumber, r.denominatorGroupNumber)
    fieldTitle + titleSep + GROUP + v1 + titleSep + VERSUS + titleSep + GROUP + v2
  }
  
  protected def mkQcFieldTitle(fieldConfig: CustomFieldConfig, qcId: Long): String = mkQcFieldTitle(fieldConfig.title, qcId)
  
  protected def mkQcFieldTitle(fieldTitle: String, qcId: Long): String = {
    fieldTitle + titleSep + quantDs.qcNameById(qcId)
  }

}