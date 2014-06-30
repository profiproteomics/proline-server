package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import scala.collection.mutable.HashMap

object ResultParseAndFiltersFields extends IViewFieldEnumeration {
  
  //TODO : Better management of multiple filters ...
  val PARSE_PARAM = Field("parse_param")  
  val PSM_FILTER_EXPECTED_FDR = Field("psm_filter_expected_fdr") 
  val PSM_FILTER_1 = Field("psm_filter_1")
  val PSM_FILTER_2 = Field("psm_filter_2")
  val PSM_FILTER_3 = Field("psm_filter_3")
  val PSM_FILTER_4 = Field("psm_filter_4")
  val PSM_FILTER_5 = Field("psm_filter_5")
  val PSM_FILTER_6 = Field("psm_filter_6")
  val PSM_FILTER_7 = Field("psm_filter_7")
  val PSM_FILTER_8 = Field("psm_filter_8")
  val PSM_FILTER_9 = Field("psm_filter_9")
  val PSM_FILTER_10 = Field("psm_filter_10")

  val PROT_FILTER_EXPECTED_FDR = Field("prot_filter_expected_fdr") 
  val PROT_FILTER_1 = Field("prot_filter_1")
  val PROT_FILTER_2 = Field("prot_filter_2")
  val PROT_FILTER_3 = Field("prot_filter_3")
  val PROT_FILTER_4 = Field("prot_filter_4")

  val PSM_RESULT_INFO = Field("psm_result")
  val PROT_RESULT_INFO = Field("prot_result")
}

class ResultParseAndFiltersView( val rsm: ResultSummary ) extends IDatasetView {
  
  var viewName = "parse and filter"
  val fields = ResultParseAndFiltersFields
  
  case class MyBuildingContext( rsm: ResultSummary) extends IRecordBuildingContext
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val rsm = myBuildingContext.rsm
    val rs = rsm.resultSet
    
    val resultMapBuilder = Map.newBuilder[String,Any]

    
    // *** Get Parser Parameters
    val parseBuilder = new StringBuilder("")
    if(rs.isDefined && rs.get.properties.isDefined){
      val rsProp = rs.get.properties.get
      
          // Targer/Decoy Mode 
      if(rsProp.getTargetDecoyMode.isDefined)
    	  parseBuilder.append("Target-Decoy Mode : ").append(rs.get.properties.get.getTargetDecoyMode.get).append("; ")
    	  
    	  // Search engine specific params  
      if(rsProp.getMascotImportProperties.isDefined){    // MASCOT
        if(rsProp.getMascotImportProperties.get.getIonsScoreCutoff.isDefined){
    	  parseBuilder.append("Mascot Ions Score Cut-off : ").append(rsProp.getMascotImportProperties.get.getIonsScoreCutoff.get).append("; ")
        }
        if(rsProp.getMascotImportProperties.get.getSubsetsThreshold.isDefined){
    	  parseBuilder.append("Mascot Subsets Threshold : ").append(rsProp.getMascotImportProperties.get.getSubsetsThreshold.get).append("; ")
        }
        if(rsProp.getMascotImportProperties.get.getProteinsPvalueCutoff.isDefined){
    	  parseBuilder.append("Mascot Proteins Pvalue Cut-off: ").append(rsProp.getMascotImportProperties.get.getProteinsPvalueCutoff.get).append("; ")
        }        
        
      } else if(rsProp.getOmssaImportProperties.isDefined){ // OMSSA
         if(rsProp.getOmssaImportProperties.get.getRawSettings.isDefined){
           parseBuilder.append("Omssa params : ")
           rsProp.getOmssaImportProperties.get.getRawSettings.get.foreach(entry =>{
             parseBuilder.append(entry._1).append(" : ").append(entry._2).append("; ")
           })    	  
        }
      }
    } //End get Parser parameters
   
    resultMapBuilder +=(fields.PARSE_PARAM.toString -> parseBuilder.result) // Save Parser Params
    
	// *** Get Filters Parameters 
    if(rsm.properties.isDefined) {
      if(rsm.properties.get.getValidationProperties.isDefined){
        val rsmValProp = rsm.properties.get.getValidationProperties.get
        
        //Add Peptide Expected FDR
        resultMapBuilder +=(fields.PSM_FILTER_EXPECTED_FDR.toString ->{if(rsmValProp.getParams.getPeptideExpectedFdr.isDefined)rsmValProp.getParams.getPeptideExpectedFdr.get else "-" })

        //Add PSM Filters
         if(rsmValProp.getParams.getPeptideFilters.isDefined){
          var fIndex =0;
          //TODO Better managments of multiples filters !!
          //Indexed array (^^) of filters fields 
          val fieldArray = Seq(fields.PSM_FILTER_1, fields.PSM_FILTER_2,fields.PSM_FILTER_3,fields.PSM_FILTER_4,fields.PSM_FILTER_5,fields.PSM_FILTER_6,fields.PSM_FILTER_7,fields.PSM_FILTER_8,fields.PSM_FILTER_9,fields.PSM_FILTER_10)
          //Go through applied PSM filters and register them 
          rsmValProp.getParams.getPeptideFilters.get.foreach(filter => {
             val fBuilder= new StringBuilder()
             fBuilder.append("PSM FILTER : ").append(filter.getParameter).append("; ")
             if(filter.getDescription.isDefined){
               fBuilder.append("Description : ").append(filter.getDescription.get).append("; ")
             }
             if(filter.getProperties.isDefined){
               fBuilder.append("Properties : [")
               filter.getProperties.get.foreach( entry => {
                 fBuilder.append(entry._1).append(" : ").append(entry._2.toString()).append("; ")
               })
               fBuilder.append("]; ")
             }
             
             resultMapBuilder +=(fieldArray(fIndex).toString -> fBuilder.result) //Save current PSM Filter
             fIndex +=1
          })
          
        } //End getPeptideFilters defined
        
        //Add Protein Expected FDR
        resultMapBuilder +=(fields.PROT_FILTER_EXPECTED_FDR.toString ->{if(rsmValProp.getParams.getProteinExpectedFdr.isDefined)rsmValProp.getParams.getProteinExpectedFdr.get else "-" })
		
        //Add Protein Filters
		if(rsmValProp.getParams.getProteinFilters.isDefined){
          var fIndex =0;
          //TODO Better multiples filters fields
          val fieldArray = Seq(fields.PROT_FILTER_1, fields.PROT_FILTER_2,fields.PROT_FILTER_3,fields.PROT_FILTER_4)
          
          rsmValProp.getParams.getProteinFilters.get.foreach(filter => {
             val fBuilder= new StringBuilder()
             fBuilder.append("Protein FILTER : ").append(filter.getParameter).append("; ")
             if(filter.getDescription.isDefined){
               fBuilder.append("Description : ").append(filter.getDescription.get).append("; ")
             }
             if(filter.getProperties.isDefined){
               fBuilder.append("Properties : [ ")
               filter.getProperties.get.foreach( entry => {
                 fBuilder.append(entry._1).append(" : ").append(entry._2.toString()).append(" ; ")
               })
               fBuilder.append("]; ")
             }
             
             resultMapBuilder +=(fieldArray(fIndex).toString -> fBuilder.result) //Save current prot filter
             fIndex +=1
          })
          
        } //End getProteinFilters defined
        
        //Add Peptide Result
        if(rsmValProp.getResults.getPeptideResults.isDefined){
          val psmResultBuilder= new StringBuilder()
          psmResultBuilder.append("TARGET Matches Count: ").append(rsmValProp.getResults.getPeptideResults.get.getTargetMatchesCount).append("; ")
          if(rsmValProp.getResults.getPeptideResults.get.getDecoyMatchesCount.isDefined)
        	  psmResultBuilder.append("DECOY Matches Count: ").append(rsmValProp.getResults.getPeptideResults.get.getDecoyMatchesCount.get).append("; ")
          if(rsmValProp.getResults.getPeptideResults.get.getFdr.isDefined)
        	  psmResultBuilder.append("PSM FDR: ").append(rsmValProp.getResults.getPeptideResults.get.getFdr.get).append("; ")
        	  
          resultMapBuilder += (fields.PSM_RESULT_INFO.toString ->psmResultBuilder.result)
        }
        
        //Add Proteins Result
        if(rsmValProp.getResults.getProteinResults.isDefined){
          val psmResultBuilder= new StringBuilder()
          psmResultBuilder.append("TARGET Matches Count: ").append(rsmValProp.getResults.getProteinResults.get.getTargetMatchesCount).append("; ")
          if(rsmValProp.getResults.getProteinResults.get.getDecoyMatchesCount.isDefined)
        	  psmResultBuilder.append("DECOY Matches Count: ").append(rsmValProp.getResults.getProteinResults.get.getDecoyMatchesCount.get).append("; ")
          if(rsmValProp.getResults.getProteinResults.get.getFdr.isDefined)
        	  psmResultBuilder.append("PROTEINS FDR: ").append(rsmValProp.getResults.getProteinResults.get.getFdr.get).append("; ")
        	  
          resultMapBuilder += (fields.PROT_RESULT_INFO.toString ->psmResultBuilder.result)
        }
                
      } //End Validation properties defined

      
      resultMapBuilder.result
      
    } else { // No rsm.properties
      Map(
      fields.PARSE_PARAM -> parseBuilder.result
	  ).map( r => r._1.toString -> r._2)
    }
  }
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {    
      this.formatRecord(MyBuildingContext(rsm), recordFormatter)    
  }

}