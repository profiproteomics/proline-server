package fr.proline.module.exporter.dataset.view


import java.text.DecimalFormat
import fr.proline.module.exporter.commons.config.view.SheetViewFieldsConfig
import java.text.SimpleDateFormat
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.api.view.IFixedDatasetView
import scala.collection.immutable.ListMap
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant

class ProtSetToProtMatchView ( val identDS: IdentDataSet, val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat, val titleSep: String, val exportAllProteinSet: Boolean , val exportBestProfile: Boolean ) extends AbstractProtSetToTypicalProtMatchView {
   var viewName = "prot_set_to_prot_match"
   
  override  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    
    for( protSet <- rsm.proteinSets ) {
      if (exportAllProteinSet || protSet.isValidated){ // filter on validated proteinSet
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      
      // Typical Protein Match is put first
      val typicalProteinMatchId = protSet.getTypicalProteinMatchId
      val typicalProtMatch = protMatchById.get(typicalProteinMatchId).get
      this.formatRecord(new ProtMatchBuildingContext(protSet, protSet.peptideSet, typicalProtMatch ), recordFormatter)

            
      for (       
        // Go through all peptide matches of the sameset peptide set
        protMatchId <- protSet.peptideSet.proteinMatchIds;
        // Retrieve the protein match
        protMatch <- protMatchById.get(protMatchId)
      ) {
    	  if(protMatchId !=typicalProteinMatchId )
        	this.formatRecord(new ProtMatchBuildingContext(protSet, protSet.peptideSet, protMatch ), recordFormatter)
      }

      
      // Sort strict subsets by descending score
      val strictSubsetsSortedByDescScore = protSet.peptideSet.strictSubsets.get.sortWith(_.score > _.score)
     
      for (
        // Go through all peptide set
        peptideSet <- strictSubsetsSortedByDescScore;
        // Go through all peptide matches of the peptide set
        protMatchId <- peptideSet.proteinMatchIds;
        // Retrieve the protein match
        protMatch <- protMatchById.get(protMatchId)
      ) {
         this.formatRecord(new ProtMatchBuildingContext(protSet, peptideSet, protMatch ), recordFormatter)
      }
      }
    }
  }
}