package fr.proline.module.exporter.msi.view


import fr.proline.core.om.model.msi._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.SheetViewFieldsConfig
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import java.text.SimpleDateFormat
import java.text.DecimalFormat
import fr.proline.module.exporter.api.view.IFormLikeView
import scala.collection.immutable.ListMap


class ImportAndValidationPropsView (val rsm: ResultSummary, val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat) extends IFormLikeView {
  
  var viewName = "import and validation"
  private val rs = rsm.resultSet
  val fields = new SheetViewFieldsConfig(sheetConfig)
  
  def getFieldValueMap() = _fieldValueMap
  def getFieldsNames() = _fieldValueMap.keys.toArray  
  
   private val _fieldValueMap = {
   // *** Get import Parameters
    val importParamsBuilder = new StringBuilder("")
    if (rs.isDefined && rs.get.properties.isDefined) {
      val rsProp = rs.get.properties.get
  
      // Targer/Decoy Mode 
      if (rsProp.getTargetDecoyMode.isDefined)
        importParamsBuilder.append("Target-Decoy Mode : ").append(rs.get.properties.get.getTargetDecoyMode.get).append("; ")
        
      // TODO: more generic way to handle search engine specific params (iterate over bean properties)
  
      // Search engine specific params  
      if (rsProp.getMascotImportProperties.isDefined) { // MASCOT
        if (rsProp.getMascotImportProperties.get.getIonsScoreCutoff.isDefined) {
          importParamsBuilder.append("Mascot Ions Score Cut-off : ").append(rsProp.getMascotImportProperties.get.getIonsScoreCutoff.get).append("; ")
        }
        if (rsProp.getMascotImportProperties.get.getSubsetsThreshold.isDefined) {
          importParamsBuilder.append("Mascot Subsets Threshold : ").append(rsProp.getMascotImportProperties.get.getSubsetsThreshold.get).append("; ")
        }
        if (rsProp.getMascotImportProperties.get.getProteinsPvalueCutoff.isDefined) {
          importParamsBuilder.append("Mascot Proteins Pvalue Cut-off: ").append(rsProp.getMascotImportProperties.get.getProteinsPvalueCutoff.get).append("; ")
        }
  
      } else if (rsProp.getOmssaImportProperties.isDefined) { // OMSSA
        if (rsProp.getOmssaImportProperties.get.getRawSettings.isDefined) {
          importParamsBuilder.append("Omssa import params : ")
          rsProp.getOmssaImportProperties.get.getRawSettings.get.foreach(entry => {
            importParamsBuilder.append(entry._1).append(" : ").append(entry._2).append("; ")
          })
        }
      }
    } //End get Parser parameters
  
  // 
  var hasPsmFilterExpectedFdr: Boolean = false
  var psmFilterExpectedFdr: Float = Float.NaN
  var hasPsmFilter: Boolean = false
  var psmFilters: Array[String] = Array()
  var hasProteinFilterExpectedFdr: Boolean = false
  var proteinFilterExpectedFdr:Float =  Float.NaN
  var hasProteinFilter: Boolean = false
  var proteinFilters: Array[String] = Array()
    
    // *** Get Filters Parameters 
    if (rsm.properties.isDefined) {
      if (rsm.properties.get.getValidationProperties.isDefined) {
        val rsmValProp = rsm.properties.get.getValidationProperties.get
  
        //Add Peptide Expected FDR
        //resultMapBuilder += (fields.PSM_FILTER_EXPECTED_FDR.toString -> { if (rsmValProp.getParams.getPeptideExpectedFdr.isDefined) rsmValProp.getParams.getPeptideExpectedFdr.get else "-" })
        //allFieldsNames += fields.PSM_FILTER_EXPECTED_FDR.toString 
        hasPsmFilterExpectedFdr = true
        psmFilterExpectedFdr = decimalFormat.format(if (rsmValProp.getParams.getPeptideExpectedFdr.isDefined) rsmValProp.getParams.getPeptideExpectedFdr.get else "-").toFloat
        
        //Add PSM Filters
        if (rsmValProp.getParams.getPeptideFilters.isDefined) {
          
          var fNumber = 1
          var nbFilters: Int = rsmValProp.getParams.getPeptideFilters.size
          psmFilters = new Array(nbFilters)
          //Go through applied PSM filters and register them 
          rsmValProp.getParams.getPeptideFilters.get.foreach(filter => {
            val fBuilder = new StringBuilder()
            fBuilder.append("PSM FILTER : ").append(filter.getParameter).append("; ")
            if (filter.getDescription.isDefined) {
              fBuilder.append("Description : ").append(filter.getDescription.get).append("; ")
            }
            if (filter.getProperties.isDefined) {
              fBuilder.append("Properties : [")
              filter.getProperties.get.foreach(entry => {
                fBuilder.append(entry._1).append(" : ").append(entry._2.toString()).append("; ")
              })
              fBuilder.append("]; ")
            }
  
            //resultMapBuilder += ( "psm_filter_"+fNumber -> fBuilder.result) //Save current PSM Filter
            //allFieldsNames += "psm_filter_"+fNumber
            psmFilters(fNumber-1) =  fBuilder.result
            hasPsmFilter = true
            fNumber += 1
          })
  
        } //End getPeptideFilters defined
        
        //Add Protein Expected FDR
        //resultMapBuilder += (fields.PROT_FILTER_EXPECTED_FDR.toString -> { if (rsmValProp.getParams.getProteinExpectedFdr.isDefined) rsmValProp.getParams.getProteinExpectedFdr.get else "-" })
		//allFieldsNames +=  fields.PROT_FILTER_EXPECTED_FDR.toString
		hasProteinFilterExpectedFdr = true
		proteinFilterExpectedFdr = decimalFormat.format(if (rsmValProp.getParams.getProteinExpectedFdr.isDefined) rsmValProp.getParams.getProteinExpectedFdr.get else "-" ).toFloat
        
		//Add Protein Filters
        if (rsmValProp.getParams.getProteinFilters.isDefined) {
          
          var fNumber = 0
           var nbFilters: Int = rsmValProp.getParams.getProteinFilters.size
           proteinFilters = new Array(nbFilters)
          rsmValProp.getParams.getProteinFilters.get.foreach(filter => {
            val fBuilder = new StringBuilder()
            fBuilder.append("Protein FILTER : ").append(filter.getParameter).append("; ")
            if (filter.getDescription.isDefined) {
              fBuilder.append("Description : ").append(filter.getDescription.get).append("; ")
            }
            if (filter.getProperties.isDefined) {
              fBuilder.append("Properties : [ ")
              filter.getProperties.get.foreach(entry => {
                fBuilder.append(entry._1).append(" : ").append(entry._2.toString()).append(" ; ")
              })
              fBuilder.append("]; ")
            }
  
            //resultMapBuilder += ( "prot_filter_"+ fNumber -> fBuilder.result) //Save current prot filter
            //allFieldsNames += "prot_filter_"+ fNumber
            hasProteinFilter = true
            proteinFilters(fNumber) =  fBuilder.result
            fNumber += 1
          })
  
        } //End getProteinFilters defined
		
      } //End getProteinFilters defined
  
      } //End Validation properties defined
      
  
   var exportMap:ListMap[String,Any] = ListMap()
    
    
    val listFields :Array[ExportConfigField] = sheetConfig.fields
    for ( f <- listFields ) {
      f.id match{
        case ExportConfigConstant.FIELD_IMPORT_PARAMS => {
          exportMap += ( fields.addField(f.title) -> importParamsBuilder.result)
        }
        case ExportConfigConstant.FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR => {
          if (hasPsmFilterExpectedFdr) {
        	  exportMap += ( fields.addField(f.title) -> psmFilterExpectedFdr)
          }
        }
        case ExportConfigConstant.FIELD_IMPORT_PSM_FILTER => {
          if (hasPsmFilter) {
             for(i <- 0 to (psmFilters.size - 1))
        	  exportMap += ( fields.addField(f.title+" "+(i +1)) -> psmFilters(i))
          }
        }
        case ExportConfigConstant.FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR => {
          if (hasProteinFilterExpectedFdr) {
        	  exportMap += ( fields.addField(f.title) -> proteinFilterExpectedFdr)
          }
        }
        case ExportConfigConstant.FIELD_IMPORT_PROT_FILTER => {
          if (hasProteinFilter) {
             for(i <- 0 to (proteinFilters.size - 1))
        	  exportMap += ( fields.addField(f.title+" "+i ) -> proteinFilters(i))
          }
        }
        case other => {
          // should not happen
        }
      }
    }
    
    //exportMap.map( r => r._1.toString -> r._2)
   exportMap
  }
}