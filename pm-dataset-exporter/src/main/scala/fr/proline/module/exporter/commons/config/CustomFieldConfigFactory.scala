package fr.proline.module.exporter.commons.config

import scala.collection.mutable.ArrayBuffer
import ExportConfigConstant._

object CustomFieldConfigFactory {

  // get all fields for information sheet
  def getAllInformationFieldsArray(): Array[CustomFieldConfig] = {
    var listFields: Array[CustomFieldConfig] = Array(
      CustomFieldConfig(FIELD_INFORMATION_PROJECT_NAME, "project_name"),
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
      CustomFieldConfig(FIELD_INFORMATION_DATABASE_SEQ_COUNT, "database_sequence_count"),
      CustomFieldConfig(FIELD_INFORMATION_TAXONOMY, "taxonomy"),
      CustomFieldConfig(FIELD_INFORMATION_ENZYMES, "enzymes"),
      CustomFieldConfig(FIELD_INFORMATION_MAX_MISSED_CLEAVAGES, "max_missed_cleavages"),
      CustomFieldConfig(FIELD_INFORMATION_FIXED_PTMS, "fixed_ptms"),
      CustomFieldConfig(FIELD_INFORMATION_VARIABLE_PTMS, "variable_ptms"),
      CustomFieldConfig(FIELD_INFORMATION_PEPTIDE_CHARGE_STATES, "peptide_charge_states"),
      CustomFieldConfig(FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE, "peptide_mass_error_tolerance"),
      CustomFieldConfig(FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE, "fragment_mass_error_tolerance"),
      CustomFieldConfig(FIELD_INFORMATION_IS_DECOY, "is_decoy")
    )

    return listFields
  }

  // get all fields for import sheet
  def getAllImportFieldsArray(): Array[CustomFieldConfig] = {
    var listFields: Array[CustomFieldConfig] = Array(
      CustomFieldConfig(FIELD_INFORMATION_RESULT_FILE_NAME, "result_file_name"),
      CustomFieldConfig(FIELD_IMPORT_PARAMS, "import_params"),
      CustomFieldConfig(FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR, "psm_filter_expected_fdr"),
      CustomFieldConfig(FIELD_IMPORT_PSM_FILTER, "psm_filter"), // incremental
      CustomFieldConfig(FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR, "prot_filter_expected_fdr"),
      CustomFieldConfig(FIELD_IMPORT_PROT_FILTER, "import_prot_filter") // incremental
    )

    return listFields
  }

  // get all fields for protein sets sheet
  def getAllProteinSetsFieldsArray(fromProtein: Boolean, fromXIC: Boolean, fromSC: Boolean): Array[CustomFieldConfig] = {
    val fieldId: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_ID, "protein_set_id")
    val fieldAcc: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_ACCESSION, "accession")
    val fieldDesc: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_DESCRIPTION, "description")
    val fieldScore: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_SCORE, "protein_set_score")
    val fieldIsVal: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_IS_VALIDATED, "is_validated")
    val fieldSelLevel: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_SELECTION_LEVEL, "selection_level")
    val fieldNbSameset: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES, "#sameset_protein_matches")
    val fieldNbSubset: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES, "#subset_protein_matches")
    val fieldCoverage: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_COVERAGE, "coverage")
    val fieldMw: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_MW, "MW")
    val fieldNbSeq: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences")
    val fieldNbSpecSeq: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences")
    val fieldNbPep: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides")
    val fieldNbSpecPep: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides")
    val fieldNbPepMatch: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches")
    val fieldNbSpecPepMatch: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches")

    fieldSelLevel.defaultDisplayed = fromXIC || fromSC
    fieldNbSameset.defaultDisplayed = fromProtein
    fieldNbSubset.defaultDisplayed = fromProtein
    fieldCoverage.defaultDisplayed = fromProtein
    fieldMw.defaultDisplayed = fromProtein
    fieldNbSeq.defaultDisplayed = fromProtein
    fieldNbSpecSeq.defaultDisplayed = fromProtein
    fieldNbPep.defaultDisplayed = fromProtein
    fieldNbSpecPep.defaultDisplayed = fromProtein
    fieldNbPepMatch.defaultDisplayed = fromProtein
    fieldNbSpecPepMatch.defaultDisplayed = fromProtein

    var listFields: ArrayBuffer[CustomFieldConfig] = ArrayBuffer(
      fieldId, fieldAcc, fieldDesc, fieldScore, fieldIsVal, fieldSelLevel,
      fieldNbSameset, fieldNbSubset, fieldCoverage, fieldMw, fieldNbSeq, fieldNbSpecSeq, fieldNbPep, fieldNbSpecPep, fieldNbPepMatch, fieldNbSpecPepMatch
    )

    if (fromXIC || fromSC) {
      val fieldRawAbundance: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE, if (fromXIC) "raw_abundance" else "Specific SC")
      val fieldAbundance: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE, if (fromXIC) "abundance" else "Weighted SC")
      val fieldPsmCount: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT, if (fromXIC) "psm_count" else "Basic SC")
      val fieldStatus: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_STATUS, "status")
      val fieldPeptideNumber: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_PEPTIDE_NUMBER, "peptide_number")
      fieldStatus.defaultDisplayed = fromSC
      fieldPeptideNumber.defaultDisplayed = fromSC
      listFields += fieldStatus
      listFields += fieldPeptideNumber
      listFields += fieldPsmCount
      listFields += fieldRawAbundance
      listFields += fieldAbundance
    }
    if (fromXIC) {
      val fieldRatio: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO, "ratio")
      val fieldTTest: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE, "t-test")
      val fieldZTest: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE, "z-test")
      val fieldZScore: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE, "z-score")
      listFields += fieldRatio
      listFields += fieldTTest
      listFields += fieldZTest
      listFields += fieldZScore
    }

    return listFields.toArray
  }

  // get all fields for protein match sheet
  def getAllProteinMatchFieldsArray(): Array[CustomFieldConfig] = {
    Array(
      CustomFieldConfig(FIELD_PROTEIN_SETS_ID, "protein_set_id"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_ACCESSION, "accession"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_DESCRIPTION, "description"),
      CustomFieldConfig(FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN, "is_representative"),
      CustomFieldConfig(FIELD_PROTEIN_MATCH_IS_SAMESET, "is_sameset"),
      CustomFieldConfig(FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE, "peptide_set_score"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_COVERAGE, "coverage"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_MW, "MW"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches")
    )
  }

  // get all fields for Best PSM sheet
  def getAllBestPSMFieldsArray(fromXIC: Boolean, fromSC: Boolean): Array[CustomFieldConfig] = {
    var listFields: ArrayBuffer[CustomFieldConfig] = ArrayBuffer(
      CustomFieldConfig(FIELD_PSM_PEPTIDE_ID, "peptide_id"),
      CustomFieldConfig(FIELD_PSM_SEQUENCE, "sequence"),
      CustomFieldConfig(FIELD_PSM_MODIFICATIONS, "modifications"),
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
      CustomFieldConfig(FIELD_PSM_NB_PROTEIN_SETS, "#protein_sets"),
      CustomFieldConfig(FIELD_PSM_NB_PROTEIN_MATCHES, "#protein_matches"),
      CustomFieldConfig(FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES, "#databank_protein_matches"),
      CustomFieldConfig(FIELD_PSM_START, "start"),
      CustomFieldConfig(FIELD_PSM_END, "end"),
      CustomFieldConfig(FIELD_PSM_RESIDUE_BEFORE, "residue_before"),
      CustomFieldConfig(FIELD_PSM_RESIDUE_AFTER, "residue_after"),
      CustomFieldConfig(FIELD_PSM_PTM_SCORE, "ptm_score"),
      CustomFieldConfig(FIELD_PSM_PTM_SITES_CONFIDENCE, "ptm_sites_confidence")
    )
    listFields = listFields ++ getAllProteinSetsFieldsArray(false, false, false)

    if (fromXIC || fromSC) {
      val fieldMasterQuantPeptideId: CustomFieldConfig = CustomFieldConfig(FIELD_PSM_QUANTI_MASTER_QUANT_PEPTIDE_ID, "master_quant_peptide_id")
      val fieldQuantiElutionTime: CustomFieldConfig = CustomFieldConfig(FIELD_PSM_QUANTI_ELUTION_TIME, "elution_time")
      val fieldQuantiSelectionLevel: CustomFieldConfig = CustomFieldConfig(FIELD_PSM_QUANTI_SELECTION_LEVEL, "master_quant_selection_level")
      val fieldRawAbundance: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE, if (fromXIC) "raw_abundance" else "Specific SC")
      val fieldAbundance: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE, "abundance")
      val fieldPsmCount: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT, if (fromXIC) "psm_count" else "Basic SC")
      fieldAbundance.defaultDisplayed = fromXIC
      listFields += fieldMasterQuantPeptideId
      listFields += fieldQuantiElutionTime
      listFields += fieldQuantiSelectionLevel
      listFields += fieldPsmCount
      listFields += fieldRawAbundance
      listFields += fieldAbundance
    }
    if (fromXIC) {
      val fieldRatio: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO, "ratio")
      val fieldTTest: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE, "t-test")
      val fieldZTest: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE, "z-test")
      val fieldZScore: CustomFieldConfig = CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE, "z-score")
      listFields += fieldRatio
      listFields += fieldTTest
      listFields += fieldZTest
      listFields += fieldZScore
    }
    return listFields.toArray
  }

  // get all fields for all PSM sheet
  def getAllPSMFieldsArray(fromXIC: Boolean, fromSC: Boolean): Array[CustomFieldConfig] = {
    return getAllBestPSMFieldsArray(fromXIC, fromSC)
  }

  // get all fields for masterQuantPeptideIon
  def getAllMasterQuantPeptideIon(): Array[CustomFieldConfig] = {
    
    val customPeptideFields = Array(
      CustomFieldConfig(FIELD_PSM_PEPTIDE_ID, "peptide_id"),
      CustomFieldConfig(FIELD_PSM_SEQUENCE, "sequence"),
      CustomFieldConfig(FIELD_PSM_MODIFICATIONS, "modifications")
    )
    
    val customProtSetFields = getAllProteinSetsFieldsArray(false, false, false)
    
    val customMqPepIonFields = Array(
      CustomFieldConfig(FIELD_PSM_QUANTI_MASTER_QUANT_PEPTIDE_ID, "master_quant_peptide_id"),
      CustomFieldConfig(FIELD_PSM_QUANTI_ELUTION_TIME, "elution_time"),
      CustomFieldConfig(FIELD_PSM_QUANTI_SELECTION_LEVEL, "master_quant_selection_level"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE, "raw_abundance"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE, "abundance"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT, "psm_count"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO, "ratio"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE, "t-test"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE, "z-test"),
      CustomFieldConfig(FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE, "z-score"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_ID, "master_quant_peptide_ion_id"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE, "master_quant_peptide_ion_charge"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME, "master_quant_peptide_ion_elution_time"),
      CustomFieldConfig(FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID, "master_quant_peptide_ion_feature_id")
    )
    
    customPeptideFields ++ customProtSetFields ++ customMqPepIonFields
  }

  // get all fields for stat sheet
  def getAllStatFieldsArray(): Array[CustomFieldConfig] = {
    var listFields: Array[CustomFieldConfig] = Array(
      CustomFieldConfig(FIELD_STAT_NB_PROTEIN_SETS, "#protein_sets"),
      CustomFieldConfig(FIELD_STAT_PSM_VALIDATION, "psm_validation"),
      CustomFieldConfig(FIELD_STAT_NB_TOTAL_PRECURSORS, "#total_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_PROTEIN_SETS_SINGLE_SPECIFIC_PEPTIDE, "#protein_sets_with_single_specific_peptide"),
      CustomFieldConfig(FIELD_STAT_NB_MODIFIED_PEPTIDES, "#modified_peptides"),
      CustomFieldConfig(FIELD_STAT_NB_Z3_PRECURSORS, "#z3_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_UNMODIFIED_PEPTIDES, "#unmodified_peptides"),
      CustomFieldConfig(FIELD_STAT_NB_PROTEIN_SETS_MULTI_SPECIFIC_PEPTIDE, "#protein_sets_with_multiple_specific_peptides"),
      CustomFieldConfig(FIELD_STAT_NB_Z2_PRECURSORS, "#z2_precursors"),
      CustomFieldConfig(FIELD_STAT_NB_PEPTIDES, "#peptides"),
      CustomFieldConfig(FIELD_STAT_NB_DISTINCT_SEQ, "#distinct_sequences"),
      CustomFieldConfig(FIELD_STAT_PROT_VALIDATION, "prot_validation")
    )

    return listFields
  }

}