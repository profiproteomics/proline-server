package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

class ProtSetToProtMatchView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToTypicalProtMatchView {
  
  var viewName = "prot_set_to_prot_match"

  protected val protMatchFieldSet = Set(
    FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN,
    FIELD_PROTEIN_MATCH_IS_SAMESET,
    FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE
  )
  
  protected val protMatchFieldsConfigs = sheetConfig.fields.filter( f => protMatchFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val buildingCtx = buildingContext.asInstanceOf[ProtMatchBuildingContext]
    val protMatchRecord = super.buildRecord(buildingCtx)
    
    val protSet = buildingCtx.protSet
    val pepSet = buildingCtx.peptideSet
    val protMatch = buildingCtx.protMatch
    
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= protMatchRecord

    for (fieldConfig <- protMatchFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN => protSet.getRepresentativeProteinMatchId == protMatch.id
        case FIELD_PROTEIN_MATCH_IS_SAMESET => !pepSet.isSubset
        case FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE => dcf1.format(pepSet.score)
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }

  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet
    val protMatchById = rs.proteinMatchById

    for (protSet <- rsm.proteinSets) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend of provider which have been used

        // Typical Protein Match is put first
        val reprProtMatch = protSet.getRepresentativeProteinMatch.getOrElse(protSet.samesetProteinMatches.get.head)
        this.formatRecord(new ProtMatchBuildingContext(protSet, protSet.peptideSet, reprProtMatch), recordFormatter)
        
        val reprProtMatchId = reprProtMatch.id

        for (
          // Go through all peptide matches of the sameset peptide set
          protMatchId <- protSet.peptideSet.proteinMatchIds;
          // Retrieve the protein match
          protMatch <- protMatchById.get(protMatchId)
        ) {
          if (protMatchId != reprProtMatchId)
            this.formatRecord(new ProtMatchBuildingContext(protSet, protSet.peptideSet, protMatch), recordFormatter)
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
          println(peptideSet.isSubset)
          this.formatRecord(new ProtMatchBuildingContext(protSet, peptideSet, protMatch), recordFormatter)
        }
      }
    }
  }
}