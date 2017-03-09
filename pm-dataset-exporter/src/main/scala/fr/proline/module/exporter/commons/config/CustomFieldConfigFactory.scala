package fr.proline.module.exporter.commons.config

import scala.collection.mutable.ArrayBuffer
import ExportConfigConstant._

object CustomFieldConfigFactory {

  // get all fields for information sheet
  def getInformationSheetFields(fromXIC: Boolean): Array[CustomFieldConfig] = {
    Array(
      CustomFieldConfig(FIELD_INFORMATION_PROJECT_NAME, "project_name"),
      if (fromXIC) CustomFieldConfig(FIELD_INFORMATION_QUANT_CHANNEL_NAME, "quant_channel_name") else null,
      CustomFieldConfig(FIELD_INFORMATION_RESULT_SET_NAME, "result_set_name"),
      CustomFieldConfig(FIELD_INFORMATION_SEARCH_TITLE, "search_title"),
      CustomFieldConfig(FIELD_INFORMATION_SEARCH_DATE, "search_date"),
      CustomFieldConfig(FIELD_INFORMATION_RAW_FILE_NAME, "raw_file_name"),
      CustomFieldConfig(FIELD_INFORMATION_PEAKLIST_FILE_PATH, "peaklist_file_path"),
      CustomFieldConfig(FIELD_INFORMATION_RESULT_FILE_NAME, "result_file_name"),
      CustomFieldConfig(FIELD_INFORMATION_RESULT_FILE_DIRECTORY, "result_file_directory"),
      CustomFieldConfig(FIELD_INFORMATION_JOB_NUMBER, "job_number"),
      CustomFieldConfig(FIELD_INFORMATION_USER_NAME, "user_name"),
      CustomFieldConfig(FIELD_INFORMATION_USER_EMAIL, "user_email"),
      CustomFieldConfig(FIELD_INFORMATION_QUERIES_COUNT, "queries_count"),
      CustomFieldConfig(FIELD_INFORMATION_SEARCHED_SEQUENCES_COUNT, "searched_sequences_count"),
      CustomFieldConfig(FIELD_INFORMATION_SOFTWARE_NAME, "software_name"),
      CustomFieldConfig(FIELD_INFORMATION_SOFTWARE_VERSION, "software_version"),
      CustomFieldConfig(FIELD_INFORMATION_INSTRUMENT_CONFIG, "instrument_config"),
      CustomFieldConfig(FIELD_INFORMATION_DATABASE_NAMES, "database_names"),
      CustomFieldConfig(FIELD_INFORMATION_DATABASE_RELEASES, "database_releases"),
      CustomFieldConfig(FIELD_INFORMATION_DATABASE_SEQUENCES_COUNT, "database_sequences_count"),
      CustomFieldConfig(FIELD_INFORMATION_TAXONOMY, "taxonomy"),
      CustomFieldConfig(FIELD_INFORMATION_ENZYMES, "enzymes"),
      CustomFieldConfig(FIELD_INFORMATION_MAX_MISSED_CLEAVAGES, "max_missed_cleavages"),
      CustomFieldConfig(FIELD_INFORMATION_FIXED_PTMS, "fixed_ptms"),
      CustomFieldConfig(FIELD_INFORMATION_VARIABLE_PTMS, "variable_ptms"),
      CustomFieldConfig(FIELD_INFORMATION_PEPTIDE_CHARGE_STATES, "peptide_charge_states"),
      CustomFieldConfig(FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE, "peptide_mass_error_tolerance"),
      CustomFieldConfig(FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE, "fragment_mass_error_tolerance"),
      CustomFieldConfig(FIELD_INFORMATION_IS_DECOY, "is_decoy")
    ) filter(_ != null) // remove empty fields
  }

  // get all fields for import sheet
  def getImportSheetFields(fromXIC: Boolean): Array[CustomFieldConfig] = {
    
    val identFields = Array(
      CustomFieldConfig(FIELD_INFORMATION_RESULT_FILE_NAME, "result_file_name"),
      CustomFieldConfig(FIELD_IMPORT_PARAMS, "import_params"),
      CustomFieldConfig(FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR, "psm_filter_expected_fdr"),
      CustomFieldConfig(FIELD_IMPORT_PSM_FILTER, "psm_filter"), // incremental
      CustomFieldConfig(FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR, "prot_filter_expected_fdr"),
      CustomFieldConfig(FIELD_IMPORT_PROT_FILTER, "import_prot_filter") // incremental
    )
    
    if (!fromXIC) identFields
    else Array(CustomFieldConfig(FIELD_INFORMATION_QUANT_CHANNEL_NAME, "quant_channel_name")) ++ identFields
  }

  // get all fields for protein sets sheet
  def getProteinSetsSheetFields(fromProtein: Boolean, fromXIC: Boolean, fromSC: Boolean): Array[CustomFieldConfig] = {

    val fieldsBuffer = ArrayBuffer(
      CustomFieldConfig(FIELD_PROTEIN_SETS_ID, "protein_set_id"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_ACCESSION, "accession"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_DESCRIPTION, "description"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_SCORE, "protein_set_score"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_IS_VALIDATED, "is_validated"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_SELECTION_LEVEL, "selection_level", defaultDisplayed = fromXIC || fromSC),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES, "#sameset_protein_matches", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES, "#subset_protein_matches", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_COVERAGE, "coverage", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_MW, "MW", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_OBSERVABLE_PEPTIDES, "#observable_peptides", defaultDisplayed = fromProtein),      
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches", defaultDisplayed = fromProtein),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches", defaultDisplayed = fromProtein)
    )
    
    if (fromSC) { // Currently not available for XIC 
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_STATUS, "status", defaultDisplayed = fromSC)
    }
    
    if (fromXIC || fromSC) {
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_PEPTIDE_NUMBER, "peptides_count", defaultDisplayed = fromXIC || fromSC)
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_PSM_COUNT, if (fromXIC) "psm_count" else "Basic SC")
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_RAW_ABUNDANCE, if (fromXIC) "raw_abundance" else "Specific SC")
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_ABUNDANCE, if (fromXIC) "abundance" else "Weighted SC")
    }
    
    if (fromXIC) {
      this._appendProfilizerFields(fieldsBuffer)
    }

    return fieldsBuffer.toArray
  }
  
  private def _appendProfilizerFields( fieldsBuffer: ArrayBuffer[CustomFieldConfig] ) {
    fieldsBuffer += CustomFieldConfig(FIELD_PROFILIZER_RATIO, "ratio")
    fieldsBuffer += CustomFieldConfig(FIELD_PROFILIZER_TTEST_PVALUE, "t-test")
    fieldsBuffer += CustomFieldConfig(FIELD_PROFILIZER_ZTEST_PVALUE, "z-test")
    fieldsBuffer += CustomFieldConfig(FIELD_PROFILIZER_ZSCORE, "z-score")
  }

  // get all fields for protein match sheet
  def getProteinMatchesSheetFields(): Array[CustomFieldConfig] = {
    Array(
      CustomFieldConfig(FIELD_PROTEIN_SETS_ID, "protein_set_id"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_ACCESSION, "accession"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_DESCRIPTION, "description"),
      CustomFieldConfig(FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN, "is_representative"),
      CustomFieldConfig(FIELD_PROTEIN_MATCH_IS_SAMESET, "is_sameset"),
      CustomFieldConfig(FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE, "peptide_set_score"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_COVERAGE, "coverage"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_MW, "MW"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_OBSERVABLE_PEPTIDES, "#observable_peptides"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches")
    )
  }

  // get all fields for Best PSM sheet
  def getPeptideMatchesSheetFields(fromXIC: Boolean, fromSC: Boolean): Array[CustomFieldConfig] = {
    
    val fieldsBuffer = ArrayBuffer(
      CustomFieldConfig(FIELD_PSM_PEPTIDE_ID, "peptide_id"),
      CustomFieldConfig(FIELD_PSM_SEQUENCE, "sequence"),
      CustomFieldConfig(FIELD_PSM_MODIFICATIONS, "modifications"),
      CustomFieldConfig(FIELD_PSM_PTM_PROTEIN_POSITIONS, "ptm_protein_positions"),
      CustomFieldConfig(FIELD_PSM_ID, "psm_id"),
      CustomFieldConfig(FIELD_PSM_SCORE, "psm_score"),
      CustomFieldConfig(FIELD_PSM_CALCULATED_MASS, "calculated_mass"),
      CustomFieldConfig(FIELD_PSM_CHARGE, "charge"),
      CustomFieldConfig(FIELD_PSM_EXPERIMENTAL_MOZ, "experimental_moz"),
      CustomFieldConfig(FIELD_PSM_DELTA_MOZ, "delta_moz"),
      CustomFieldConfig(FIELD_PSM_RT, "rt"),
      CustomFieldConfig(FIELD_PSM_PEPTIDE_LENGTH, "peptide_length"),
      CustomFieldConfig(FIELD_PSM_INITIAL_QUERY_ID, "initial_query_id"),
      CustomFieldConfig(FIELD_PSM_MISSED_CLEAVAGES, "missed_cleavages"),
      CustomFieldConfig(FIELD_PSM_RANK, "rank"),
      CustomFieldConfig(FIELD_PSM_CD_PRETTY_RANK, "cd_pretty_rank"),
      CustomFieldConfig(FIELD_PSM_FRAGMENT_MATCHES_COUNT, "fragment_matches_count"),
      CustomFieldConfig(FIELD_PSM_SPECTRUM_TITLE, "spectrum_title"),
      CustomFieldConfig(FIELD_PSM_NB_PROTEIN_SETS, "#psm_prot_sets"),
      CustomFieldConfig(FIELD_PSM_NB_SAMESET_PROTEIN_MATCHES, "#psm_sameset_prot_matches"),
      if (fromXIC || fromSC) null else CustomFieldConfig(FIELD_PSM_NB_PROTEIN_MATCHES, "#psm_prot_matches"),
      if (fromXIC || fromSC) null else CustomFieldConfig(FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES, "#psm_db_prot_matches"),
      CustomFieldConfig(FIELD_PSM_START, "start"),
      CustomFieldConfig(FIELD_PSM_END, "end"),
      CustomFieldConfig(FIELD_PSM_RESIDUE_BEFORE, "residue_before"),
      CustomFieldConfig(FIELD_PSM_RESIDUE_AFTER, "residue_after"),
      CustomFieldConfig(FIELD_PSM_PTM_SCORE, "ptm_score"),
      CustomFieldConfig(FIELD_PSM_PTM_SITES_CONFIDENCE, "ptm_sites_confidence")
    ).filter( _ != null )
    
    fieldsBuffer ++= getProteinSetsSheetFields(false, false, false)

    if (fromXIC || fromSC) {
      fieldsBuffer += CustomFieldConfig(FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID, "master_quant_peptide_id")
      fieldsBuffer += CustomFieldConfig(FIELD_PSM_QUANT_ELUTION_TIME, "master_elution_time")
      fieldsBuffer += CustomFieldConfig(FIELD_PSM_QUANT_SELECTION_LEVEL, "master_quant_selection_level")
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_PSM_COUNT, if (fromXIC) "psm_count" else "Basic SC")
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_RAW_ABUNDANCE, if (fromXIC) "raw_abundance" else "Specific SC")      
    }
    
    if (fromXIC) {
      fieldsBuffer += CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_ABUNDANCE,  "abundance" ) // field only used in XIC. for SC : no WSC for peptide   
      this._appendProfilizerFields(fieldsBuffer)
    }
    
    fieldsBuffer.toArray
  }

  // get all fields for all PSM sheet
  def getBestPeptideMatchesSheetFields(fromXIC: Boolean, fromSC: Boolean): Array[CustomFieldConfig] = {
    getPeptideMatchesSheetFields(fromXIC, fromSC)
  }

  // get all fields for masterQuantPeptideIon
  def getMasterQuantPepIonSheetFields(): Array[CustomFieldConfig] = {
    
    val fieldsBuffer = ArrayBuffer(
      CustomFieldConfig(FIELD_PSM_PEPTIDE_ID, "peptide_id"),
      CustomFieldConfig(FIELD_PSM_SEQUENCE, "sequence"),
      CustomFieldConfig(FIELD_PSM_MODIFICATIONS, "modifications"),
      CustomFieldConfig(FIELD_PSM_PTM_PROTEIN_POSITIONS, "ptm_protein_positions"),
      CustomFieldConfig(FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID, "master_quant_peptide_id"),
      CustomFieldConfig(FIELD_PSM_QUANT_ELUTION_TIME, "master_elution_time"),
      CustomFieldConfig(FIELD_PSM_QUANT_SELECTION_LEVEL, "master_quant_selection_level"),
      CustomFieldConfig(FIELD_PSM_NB_PROTEIN_SETS, "#psm_prot_sets"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_ID, "master_quant_peptide_ion_id"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ, "master_quant_peptide_ion_moz"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE, "master_quant_peptide_ion_charge"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME, "master_quant_peptide_ion_elution_time"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID, "master_quant_peptide_ion_feature_id")
    )
    
    fieldsBuffer ++= getProteinSetsSheetFields(false, false, false)
    
    fieldsBuffer ++= Array(
      CustomFieldConfig(FIELD_QUANT_PEPTIDE_ION_BEST_SCORE, "best_score"), // incremental
      CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_PSM_COUNT, "psm_count"), // incremental
      CustomFieldConfig(FIELD_QUANT_PEPTIDE_ION_ELUTION_TIME, "elution_time"), // incremental
      CustomFieldConfig(FIELD_QUANT_PEPTIDE_ION_CORRECTED_ELUTION_TIME, "corrected_time"), // incremental
      CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_RAW_ABUNDANCE, "raw_abundance"), // incremental
      CustomFieldConfig(FIELD_PROTEIN_SETS_QUANT_ABUNDANCE, "abundance"), // incremental
      CustomFieldConfig(FIELD_PROFILIZER_RATIO, "ratio"),
      CustomFieldConfig(FIELD_PROFILIZER_TTEST_PVALUE, "t-test"),
      CustomFieldConfig(FIELD_PROFILIZER_ZTEST_PVALUE, "z-test"),
      CustomFieldConfig(FIELD_PROFILIZER_ZSCORE, "z-score")
    )
    
    fieldsBuffer.toArray
  }

  // get all fields for stat sheet
  def getStatisticsSheetFields(): Array[CustomFieldConfig] = {
    Array(
      CustomFieldConfig(FIELD_STAT_PSM_VALIDATION, "psm_validation"),
      CustomFieldConfig(FIELD_STAT_NB_TOTAL_PSMS, "#total_psms"),
      CustomFieldConfig(FIELD_STAT_NB_TOTAL_PRECURSORS, "#total_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_Z2_PRECURSORS, "#z2_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_Z3_PRECURSORS, "#z3_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_Z4PLUS_PRECURSORS, "#z4+_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_PEPTIDES, "#peptides"),
      CustomFieldConfig(FIELD_STAT_NB_MODIFIED_PEPTIDES, "#modified_peptides"),
      CustomFieldConfig(FIELD_STAT_NB_UNMODIFIED_PEPTIDES, "#unmodified_peptides"),
      CustomFieldConfig(FIELD_STAT_NB_DISTINCT_SEQS, "#distinct_peptide_sequences"),
      CustomFieldConfig(FIELD_STAT_PROT_VALIDATION, "prot_validation"),
      CustomFieldConfig(FIELD_STAT_NB_PROTEIN_SETS, "#protein_sets"),
      CustomFieldConfig(FIELD_STAT_NB_VALIDATED_PROT_SETS, "#valid_protein_sets"),
      CustomFieldConfig(FIELD_STAT_NB_VALIDATED_PROT_SETS_SINGLE_PEPTIDE, "#valid_protein_sets_with_single_peptide"),
      CustomFieldConfig(FIELD_STAT_NB_VALIDATED_PROT_SETS_MULTI_PEPTIDES, "#valid_protein_sets_with_multiple_peptides"),
      CustomFieldConfig(FIELD_STAT_NB_VALIDATED_PROT_SETS_SINGLE_SPE_PEP_SEQUENCE, "#valid_prot_sets_with_single_specific_pep_sequence"),
      CustomFieldConfig(FIELD_STAT_NB_VALIDATED_PROT_SETS_MULTI_SPE_PEP_SEQUENCES, "#valid_prot_sets_with_multiple_specific_pep_sequences")
    )
  }

}