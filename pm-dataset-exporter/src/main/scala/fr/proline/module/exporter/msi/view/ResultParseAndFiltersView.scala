package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import scala.collection.mutable.HashMap

// TODO: rename into ImportAndValidationPropsFields
object ResultParseAndFiltersFields extends IViewFieldEnumeration {
  val IMPORT_PARAMS = Field("import_params")
  val PSM_FILTER_EXPECTED_FDR = Field("psm_filter_expected_fdr")
  val PROT_FILTER_EXPECTED_FDR = Field("prot_filter_expected_fdr")
}

// TODO: rename into ImportAndValidationPropsView
class ResultParseAndFiltersView(val rsm: ResultSummary) extends IFormLikeView {

  var viewName = "import and validation"
  private val rs = rsm.resultSet
  private val fields = ResultParseAndFiltersFields
  
  def getFieldValueMap() = _fieldValueMap
  def getFieldsNames() = _fieldValueMap.keys.toArray

  private val _fieldValueMap = {
    
    val resultMapBuilder = Map.newBuilder[String, Any]
  
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
  
    resultMapBuilder += (fields.IMPORT_PARAMS.toString -> importParamsBuilder.result) // Save Parser Params
  
    // *** Get Filters Parameters 
    if (rsm.properties.isDefined) {
      if (rsm.properties.get.getValidationProperties.isDefined) {
        val rsmValProp = rsm.properties.get.getValidationProperties.get
  
        //Add Peptide Expected FDR
        resultMapBuilder += (fields.PSM_FILTER_EXPECTED_FDR.toString -> { if (rsmValProp.getParams.getPeptideExpectedFdr.isDefined) rsmValProp.getParams.getPeptideExpectedFdr.get else "-" })
  
        //Add PSM Filters
        if (rsmValProp.getParams.getPeptideFilters.isDefined) {
          
          var fNumber = 1

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
  
            resultMapBuilder += ( "psm_filter_"+fNumber -> fBuilder.result) //Save current PSM Filter
            fNumber += 1
          })
  
        } //End getPeptideFilters defined
  
        //Add Protein Expected FDR
        resultMapBuilder += (fields.PROT_FILTER_EXPECTED_FDR.toString -> { if (rsmValProp.getParams.getProteinExpectedFdr.isDefined) rsmValProp.getParams.getProteinExpectedFdr.get else "-" })
  
        //Add Protein Filters
        if (rsmValProp.getParams.getProteinFilters.isDefined) {
          
          var fNumber = 0
          
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
  
            resultMapBuilder += ( "prot_filter_"+ fNumber -> fBuilder.result) //Save current prot filter
            
            fNumber += 1
          })
  
        } //End getProteinFilters defined
  
      } //End Validation properties defined
  
      resultMapBuilder.result
  
    } else { // No rsm.properties
      Map(
        fields.IMPORT_PARAMS -> importParamsBuilder.result
      ).map(r => r._1.toString -> r._2)
    }
    
  }

}