package fr.proline.module.exporter.dataset.view


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
import fr.proline.module.exporter.api.view.IFixedDatasetView
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import com.typesafe.scalalogging.slf4j.Logging


class ImportAndValidationPropsView (val identDS: IdentDataSet, val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat) extends IFixedDatasetView with Logging{
  
  var viewName = "import and validation"
  val childResultSummarys = identDS.childsResultSummarys
  var listFields: ArrayBuffer[String] = new ArrayBuffer()
  var nbFiltersPsm: Int = 0
  var nbFiltersProt: Int = 0
  if (identDS.resultSummary.properties.isDefined) {
      if (identDS.resultSummary.properties.get.getValidationProperties.isDefined) {
        val rsmValProp = identDS.resultSummary.properties.get.getValidationProperties.get
  
        //Add PSM Filters
        if (rsmValProp.getParams.getPeptideFilters.isDefined) {
          nbFiltersPsm = rsmValProp.getParams.getPeptideFilters.get.size
        }
        if (rsmValProp.getParams.getProteinFilters.isDefined) {
           nbFiltersProt = rsmValProp.getParams.getProteinFilters.size
        }
      }
  }
  logger.debug("ImportAndValidationPropsView with "+nbFiltersPsm+", "+nbFiltersProt)
  for ( f <- sheetConfig.fields ) {
     f.id match{
        case ExportConfigConstant.FIELD_IMPORT_PSM_FILTER => {
          if (nbFiltersPsm > 0) {
             for(i <- 0 to (nbFiltersPsm - 1)){
        	  listFields += f.title+" "+(i +1)
            }
          }
        }
        case ExportConfigConstant.FIELD_IMPORT_PROT_FILTER => {
          if (nbFiltersProt > 0) {
             for(i <- 0 to (nbFiltersProt - 1)){
        	  listFields += f.title+" "+i 
             }
          }
        }
        case other => {
          logger.debug("build Fields for import "+f.title)
    	 listFields += f.title
        }
     }
   }
  val fields = new SheetViewFieldsConfig(listFields.toArray)
  
  case class MyBuildingContext(rsm : ResultSummary ) extends IRecordBuildingContext
   
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val rsm = myBuildingContext.rsm
    val rs = rsm.resultSet.get
    
    var fileName: String = ""
    for( msiSearch <- rs.msiSearch if msiSearch != null ) {
      if (rs.childMsiSearches.isEmpty) {
    		for (msiSearch <- rs.msiSearch if msiSearch != null) {
    			fileName = msiSearch.resultFileName
    		}
    	} else {
    		for (msiSearch <- rs.childMsiSearches) {
    			fileName = msiSearch.resultFileName
    		}
    	}
    }
    logger.debug("Import and validation fileName "+fileName)
    // *** Get import Parameters
    val importParamsBuilder = new StringBuilder("")
    if ( rs.properties.isDefined) {
      val rsProp = rs.properties.get
  
      // Targer/Decoy Mode 
      if (rsProp.getTargetDecoyMode.isDefined)
        importParamsBuilder.append("Target-Decoy Mode : ").append(rs.properties.get.getTargetDecoyMode.get).append("; ")
        
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
  var psmFilterExpectedFdr: String = "-"
  var hasPsmFilter: Boolean = false
  var psmFilters: Array[String] = Array()
  var hasProteinFilterExpectedFdr: Boolean = false
  var proteinFilterExpectedFdr: String =  "-"
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
        if (rsmValProp.getParams.getPeptideExpectedFdr.isDefined){
        	psmFilterExpectedFdr = decimalFormat.format(rsmValProp.getParams.getPeptideExpectedFdr.get)
        }
        
        //Add PSM Filters
        if (rsmValProp.getParams.getPeptideFilters.isDefined) {
          
          var fNumber = 1
          var nbFilters: Int = rsmValProp.getParams.getPeptideFilters.get.size
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
		if (rsmValProp.getParams.getProteinExpectedFdr.isDefined){
		  proteinFilterExpectedFdr= decimalFormat.format(rsmValProp.getParams.getProteinExpectedFdr.get)
		}
        
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
        case ExportConfigConstant.FIELD_INFORMATION_RESULT_FILE_NAME => {
          	exportMap += ( fields.addField(f.title) -> fileName)
        }
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
             for(i <- 0 to (psmFilters.size - 1)){
        	  exportMap += ( fields.addField(f.title+" "+(i +1)) -> psmFilters(i))
            }
          }
        }
        case ExportConfigConstant.FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR => {
          if (hasProteinFilterExpectedFdr) {
        	  exportMap += ( fields.addField(f.title) -> proteinFilterExpectedFdr)
          }
        }
        case ExportConfigConstant.FIELD_IMPORT_PROT_FILTER => {
          if (hasProteinFilter) {
             for(i <- 0 to (proteinFilters.size - 1)){
        	  exportMap += ( fields.addField(f.title+" "+i ) -> proteinFilters(i))
             }
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
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    for (childRsm <- childResultSummarys) {
    	this.formatRecord(MyBuildingContext(childRsm), recordFormatter)
    }
    
  }
}