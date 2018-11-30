package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.serialization.ProfiJson
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view.ICustomTableView
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.CustomViewFields
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset._

class ImportAndValidationPropsView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String
) extends ICustomTableView with LazyLogging {

  var viewName = "import and validation"
  
  val quantDS: QuantDataset = identDS match {
    case qds: QuantDataset => qds
    case _ => null
  }
  protected val isQuantDS = quantDS != null
  protected val qcNameByRsId = if (isQuantDS) quantDS.qcNameByRsId else null
  
  val fields: CustomViewFields = {
    
    // Count the number of PSM/Protein filters
    var psmFiltersCount = 0
    var protFiltersCount = 0
  
    for (
      rsm <- identDS.allResultSummaries;
      props <- rsm.descriptor.properties;
      valProps <- props.getValidationProperties
    ) {
      val validationParams = valProps.getParams
      
      val psmFiltersOpt = validationParams.getPeptideFilters
      if (psmFiltersOpt.isDefined) {
        psmFiltersCount = Math.max(psmFiltersCount, psmFiltersOpt.get.length)
      }
      val protFiltersOpt = validationParams.getProteinFilters
      if (psmFiltersOpt.isDefined) {
        protFiltersCount = Math.max(protFiltersCount, psmFiltersOpt.get.length)
      }
    }
  
    logger.debug(s"ImportAndValidationPropsView with $psmFiltersCount PSM filters and $protFiltersCount Protein filters")
    
    val fieldsTitles = new ArrayBuffer[String](sheetConfig.fields.length + psmFiltersCount + protFiltersCount)
    def appendFilterTitles(title: String, filtersCount: Int): Unit = {
      for (i <- 1 to psmFiltersCount) {
        fieldsTitles += title + titleSep + i
      }
    }
    
    for (fieldConfig <- sheetConfig.fields) {
      fieldConfig.id match {
        case FIELD_IMPORT_PSM_FILTER => appendFilterTitles(fieldConfig.title, psmFiltersCount)
        case FIELD_IMPORT_PROT_FILTER => appendFilterTitles(fieldConfig.title, protFiltersCount)
        case FIELD_INFORMATION_QUANT_CHANNEL_NAME => if (isQuantDS) fieldsTitles += fieldConfig.title
        case other => fieldsTitles += fieldConfig.title
      }
    }
    
    new CustomViewFields(fieldsTitles)
  }

  case class MyBuildingContext(rsm: LazyResultSummary) extends IRecordBuildingContext

  def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val rsm = myBuildingContext.rsm
    val rs = rsm.lazyResultSet
    val qcNameOpt = if (isQuantDS) qcNameByRsId.get(rs.id) else None

    // FIXME: merged result sets should be named with the name of corresponding dataset  to be passed in IdentDS  
    val fileNameOrRsName = rs.msiSearch.map( _.resultFileName ).getOrElse( rs.descriptor.name )
    val fileName = if( StringUtils.isEmpty(fileNameOrRsName) ) "DATASET_IdentSummary_Id"+rsm.id else fileNameOrRsName
    logger.debug("Exporting 'import and validation' information of dataset named " + fileName)
    
    // *** Get import Parameters
    lazy val importParams = {
      val importParamsBuffer = new ArrayBuffer[String]()
      
      val rsPropsOpt = rs.descriptor.properties
      if (rsPropsOpt.isDefined) {
        val rsProps = rsPropsOpt.get
  
        // Target/Decoy Mode
        if (rsProps.getTargetDecoyMode.isDefined) {
          importParamsBuffer += s"Target-Decoy Mode: ${rsProps.getTargetDecoyMode.get}"
        }
  
        // Search engine specific params
        if (rsProps.getMascotImportProperties.isDefined) { // MASCOT
          val rsMascotProps = rsProps.getMascotImportProperties.get
  
          val importPropByKey = ProfiJson.getObjectMapper().convertValue(rsMascotProps, classOf[Map[String, Any]])
          val sortedKeys = importPropByKey.keys.toList.sorted
          for (key <- sortedKeys) {
            val value = importPropByKey(key)
            val keyAsStr = key.split("_").map(_.capitalize).mkString(" ")
            
            importParamsBuffer += s"Mascot $keyAsStr: $value"
          }
  
        } else if (rsProps.getOmssaImportProperties.isDefined) { // OMSSA
          
          if (rsProps.getOmssaImportProperties.get.getRawSettings.isDefined) {
            importParamsBuffer += "Omssa import params: "
            
            for( (k,v) <- rsProps.getOmssaImportProperties.get.getRawSettings.get ) {
              importParamsBuffer += s"$k=$v"
            }
          }
        }
      } //End get Parser parameters
      
      importParamsBuffer
    }
    
    // Init some vars
    val psmFilters = new ArrayBuffer[String]()
    var psmFilterExpectedFdr = Option.empty[Any]
    val protFilters = new ArrayBuffer[String]()
    var proteinFilterExpectedFdr = Option.empty[Any]

    // *** Get Filters Parameters
    val rsmPropsOpt = rsm.descriptor.properties
    if (rsmPropsOpt.isDefined) {
      val rsmValPropsOpt = rsmPropsOpt.get.getValidationProperties
      if (rsmValPropsOpt.isDefined) {
        val rsmValParams = rsmValPropsOpt.get.getParams

        // Add PSM Filters
        if (rsmValParams.getPeptideFilters.isDefined) {
          for(filter <- rsmValParams.getPeptideFilters.get) {
            psmFilters += this._stringifyValidationFilter(filter, "PSM")
          }
        }
        
        // Add Peptide Expected FDR
        if (rsmValParams.getPeptideExpectedFdr.isDefined) {
          psmFilterExpectedFdr = Some(
            decimalFormat.format(rsmValParams.getPeptideExpectedFdr.get)
          )
        }

        // Add Protein Filters
        if (rsmValParams.getProteinFilters.isDefined) {
          for (filter <- rsmValParams.getProteinFilters.get) {
            protFilters += this._stringifyValidationFilter(filter, "Protein")
          }
        }
        
        // Add Protein Expected FDR
        if (rsmValParams.getProteinExpectedFdr.isDefined) {
          proteinFilterExpectedFdr = Some(
            decimalFormat.format(rsmValParams.getProteinExpectedFdr.get)
          )
        }

      } //End getValidationProperties defined

    } //End properties defined

    val recordBuilder = Map.newBuilder[String,Any]

    for (fieldConfig <- sheetConfig.fields) {
      
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_INFORMATION_QUANT_CHANNEL_NAME => qcNameOpt.getOrElse("-")
        case FIELD_INFORMATION_RESULT_FILE_NAME => fileName
        case FIELD_IMPORT_PARAMS => importParams.mkString("; ")
        case FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR => psmFilterExpectedFdr.getOrElse("-")
        case FIELD_IMPORT_PSM_FILTER => psmFilters
        case FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR => proteinFilterExpectedFdr.getOrElse("-")
        case FIELD_IMPORT_PROT_FILTER => protFilters
      }
      
      fieldValue match {
        case filters: ArrayBuffer[_] => {
          for (i <- filters.indices) {
            recordBuilder += (fieldConfig.title + titleSep + (i+1)) -> filters(i)
          }
        }
        case v: Any => recordBuilder += fieldConfig.title -> v
      }
    }

    recordBuilder.result
  }

  def formatView(recordFormatter: Map[String, Any] => Unit) {
    for (rsm <- identDS.allResultSummaries) {
      this.formatRecord(MyBuildingContext(rsm), recordFormatter)
    }
  }
  
  private def _stringifyValidationFilter(filter: FilterDescriptor, prefix: String): String = {
    val pepFiltersBuffer = new ArrayBuffer[String]()
            
    pepFiltersBuffer += s"$prefix FILTER: ${filter.getParameter}"
    
    if (filter.getDescription.isDefined) {
      pepFiltersBuffer += s"Description: ${filter.getDescription.get}"
    }
    
    if (filter.getProperties.isDefined) {
      val strBuilder = new StringBuilder()
      strBuilder ++= "Properties: ["
      strBuilder ++= filter.getProperties.get.map { case (k,v) => s"$k=$v" } mkString(", ")
      strBuilder ++= "]"
      
      pepFiltersBuffer += strBuilder.result()
    }
    
    pepFiltersBuffer.mkString("; ")
  }
}