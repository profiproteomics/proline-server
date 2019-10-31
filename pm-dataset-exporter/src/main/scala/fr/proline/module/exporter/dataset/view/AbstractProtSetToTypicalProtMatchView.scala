package fr.proline.module.exporter.dataset.view

import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._

abstract class AbstractProtSetToTypicalProtMatchView extends AbstractIdentDatasetView {
  
  private val proteinMatchById = identDS.resultSummary.lazyResultSet.proteinMatchById
  
  protected val protSetFieldSet = Set(
    FIELD_PROTEIN_SETS_ID,
    FIELD_PROTEIN_SETS_ACCESSION,
    FIELD_PROTEIN_MATCH_GENE_NAME,
    FIELD_PROTEIN_SETS_SAMESETS_ACCESSIONS,
    FIELD_PROTEIN_SETS_SUBSETS_ACCESSIONS,
    FIELD_PROTEIN_SETS_DESCRIPTION,
    FIELD_PROTEIN_SETS_SCORE,
    FIELD_PROTEIN_SETS_IS_VALIDATED,
    FIELD_PROTEIN_SETS_SELECTION_LEVEL,
    FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES,
    FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES,
    FIELD_PROTEIN_SETS_COVERAGE,
    FIELD_PROTEIN_SETS_MW,
    FIELD_PROTEIN_SETS_NB_OBSERVABLE_PEPTIDES,
    FIELD_PROTEIN_SETS_NB_SEQUENCES,
    FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES,
    FIELD_PROTEIN_SETS_NB_PEPTIDES,
    FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES,
    FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES,
    FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES
  )
  
  protected val protSetFieldsConfigs = sheetConfig.fields.filter( f => protSetFieldSet.contains(f.id) )

  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val buildingCtx = buildingContext.asInstanceOf[ProtMatchBuildingContext]
    val protSet = buildingCtx.protSet
    val pepSet = protSet.peptideSet
    val protMatch = buildingCtx.protMatch // here it is the typical protein match
    
    // Retrieve sameset & subset accessions
    def protMatchIdsToAcs(ids: Array[Long]) = ids.map( proteinMatchById.get(_).map(_.accession).getOrElse("-") )
    
    val samesetProtMatches = protSet.samesetProteinMatches
    val samesetAcs = if (samesetProtMatches == null || samesetProtMatches.isEmpty) {
      protMatchIdsToAcs(protSet.getSameSetProteinMatchIds)
    } else samesetProtMatches.get.map(_.accession)
    
    val subsetProtMatches = protSet.subsetProteinMatches
    val subsetAcs = if (subsetProtMatches == null || subsetProtMatches.isEmpty) {
      protMatchIdsToAcs(protSet.getSubSetProteinMatchIds)
    } else subsetProtMatches.get.map(_.accession)
    
    // MW
    val bioSequenceById = identDS.bioSequenceById
    val proteinId = protMatch.getProteinId()
    val massOpt = if (proteinId > 0) bioSequenceById.get(proteinId).map(_.mass) else None
    val mass = massOpt.getOrElse(0.0)
    
    // Coverage
    val proteinMatchCoverageById = Option(protSet.proteinMatchCoverageById).getOrElse(Map())
    val protMatchCoverage = proteinMatchCoverageById(protMatch.id)
    
    val recordBuilder = Map.newBuilder[String,Any]

    for (fieldConfig <- protSetFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PROTEIN_SETS_ID => protSet.id
        case FIELD_PROTEIN_SETS_ACCESSION => protMatch.accession
        case FIELD_PROTEIN_MATCH_GENE_NAME => protMatch.geneName
        case FIELD_PROTEIN_SETS_SAMESETS_ACCESSIONS => samesetAcs.sorted.mkString("; ")
        case FIELD_PROTEIN_SETS_SUBSETS_ACCESSIONS => subsetAcs.sorted.mkString("; ")
        case FIELD_PROTEIN_SETS_DESCRIPTION => protMatch.description
        case FIELD_PROTEIN_SETS_SCORE => decimalFormat.format(pepSet.score) // FIXME: this column does not exist in the MSIdb
        case FIELD_PROTEIN_SETS_IS_VALIDATED => protSet.isValidated
        case FIELD_PROTEIN_SETS_SELECTION_LEVEL => protSet.selectionLevel
        case FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES => protSet.getSameSetProteinMatchIds.length
        case FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES => protSet.getSubSetProteinMatchIds.length
        case FIELD_PROTEIN_SETS_COVERAGE => dcf2.format(protMatchCoverage)
        case FIELD_PROTEIN_SETS_MW => decimalFormat.format(mass)
        case FIELD_PROTEIN_SETS_NB_OBSERVABLE_PEPTIDES => if (protMatch.properties.isDefined) protMatch.properties.get.getObservablePeptideCount else -1
        case FIELD_PROTEIN_SETS_NB_SEQUENCES => buildingCtx.sequencesCount
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES => if (protSet.isValidated) buildingCtx.specificSequencesCount else -1
        case FIELD_PROTEIN_SETS_NB_PEPTIDES => buildingCtx.peptidesCount
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES => if(protSet.isValidated && buildingCtx.specificPeptidesCount >= 0) buildingCtx.specificPeptidesCount else -1
        case FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES => buildingCtx.peptideMatchesCount
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES => if (protSet.isValidated && buildingCtx.specificPeptideMatchesCount >= 0) buildingCtx.specificPeptideMatchesCount else -1
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }

  
}