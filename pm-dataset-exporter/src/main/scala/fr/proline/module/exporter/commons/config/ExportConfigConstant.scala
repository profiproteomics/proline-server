package fr.proline.module.exporter.commons.config

object ExportConfigConstant {

  // presentation of a sheet : by rows or columns
  final val PRESENTATION_SHEET_ROWS: String = "rows"
  final val PRESENTATION_SHEET_COLUMNS: String = "columns"
  final val PRESENTATION_SHEET_VALUES: Array[String] = Array(PRESENTATION_SHEET_ROWS, PRESENTATION_SHEET_COLUMNS)
  // format
  final val FORMAT_XLSX: String = "xlsx"
  final val FORMAT_TSV: String = "tsv"
  final val FORMAT_VALUES: Array[String] = Array(FORMAT_XLSX, FORMAT_TSV)

  // date format
  final val DATE_FORMAT_HOUR: String = "yyyy:MM:dd HH:mm:ss"
  final val DATE_FORMAT: String = "yyyy:MM:dd"
  final val DATE_FORMAT_VALUES: Array[String] = Array(DATE_FORMAT_HOUR, DATE_FORMAT)

  // separator title when the field is "incremental"
  final val SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE = "_"
  final val SEPARATOR_INCREMENTAL_TITLE_SPACE = " "
  final val SEPARATOR_INCREMENTAL_TITLE_VALUES: Array[String] = Array(SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE, SEPARATOR_INCREMENTAL_TITLE_SPACE)

  final val GROUP: String = "g"
  final val VERSUS: String = "vs"

  final val MODE_IDENT: String = "IDENT"
  final val MODE_QUANT_SC: String = "QUANT_SC"
  final val MODE_QUANT_XIC: String = "QUANT_XIC"

  // decimal separator
  final val DECIMAL_SEPARATOR_COMMA: Char = ','
  final val DECIMAL_SEPARATOR_DOT: Char = '.'
  final val DECIMAL_SPEARATOR_VALUES: Array[String] = Array(".", ",")

  // information sheet
  final val SHEET_INFORMATION: String = "information"

  final val FIELD_INFORMATION_PROJECT_NAME: String = "information_project_name"
  final val FIELD_INFORMATION_RESULT_SET_NAME: String = "information_result_set_name"
  final val FIELD_INFORMATION_SEARCH_TITLE: String = "information_search_title"
  final val FIELD_INFORMATION_SEARCH_DATE: String = "information_search_date"
  final val FIELD_INFORMATION_RAW_FILE_NAME: String = "information_raw_file_name"
  final val FIELD_INFORMATION_PEAKLIST_FILE_PATH: String = "information_peaklist_file_path"
  final val FIELD_INFORMATION_RESULT_FILE_NAME: String = "information_result_file_name"
  final val FIELD_INFORMATION_RESULT_FILE_DIRECTORY: String = "information_result_file_directory"
  final val FIELD_INFORMATION_JOB_NUMBER: String = "information_job_number"
  final val FIELD_INFORMATION_USER_NAME: String = "information_user_name"
  final val FIELD_INFORMATION_USER_EMAIL: String = "information_user_email"
  final val FIELD_INFORMATION_QUERIES_COUNT: String = "information_queries_count"
  final val FIELD_INFORMATION_SEARCHED_SEQUENCES_COUNT: String = "information_searched_sequences_count"
  final val FIELD_INFORMATION_SOFTWARE_NAME: String = "information_software_name"
  final val FIELD_INFORMATION_SOFTWARE_VERSION: String = "information_software_version"
  final val FIELD_INFORMATION_INSTRUMENT_CONFIG: String = "information_instrument_config"
  final val FIELD_INFORMATION_DATABASE_NAMES: String = "information_database_names"
  final val FIELD_INFORMATION_DATABASE_RELEASES: String = "information_database_releases"
  final val FIELD_INFORMATION_TAXONOMY: String = "information_taxonomy"
  final val FIELD_INFORMATION_ENZYMES: String = "information_enzymes"
  final val FIELD_INFORMATION_MAX_MISSED_CLEAVAGES: String = "information_max_missed_cleavages"
  final val FIELD_INFORMATION_FIXED_PTMS: String = "information_fixed_ptms"
  final val FIELD_INFORMATION_VARIABLE_PTMS: String = "information_variable_ptms"
  final val FIELD_INFORMATION_PEPTIDE_CHARGE_STATES: String = "information_peptide_charge_states"
  final val FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE: String = "information_peptide_mass_error_tolerance"
  final val FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE: String = "information_fragment_mass_error_tolerance"
  final val FIELD_INFORMATION_IS_DECOY: String = "information_is_decoy"

  // import sheet    
  final val SHEET_IMPORT: String = "import"

  final val FIELD_IMPORT_PARAMS: String = "import_params"
  final val FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR: String = "import_psm_filter_expected_fdr"
  final val FIELD_IMPORT_PSM_FILTER: String = "import_psm_filter" // incremental 
  final val FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR: String = "import_prot_filter_expected_fdr"
  final val FIELD_IMPORT_PROT_FILTER: String = "import_prot_filter" //incremental

  // protein set sheet
  final val SHEET_PROTEIN_SETS: String = "protein_sets"

  final val FIELD_PROTEIN_SETS_ID: String = "protein_sets_id"
  final val FIELD_PROTEIN_SETS_ACCESSION: String = "protein_sets_accession"
  final val FIELD_PROTEIN_SETS_DESCRIPTION: String = "protein_sets_description"
  final val FIELD_PROTEIN_SETS_SCORE: String = "protein_sets_score"
  final val FIELD_PROTEIN_SETS_IS_VALIDATED: String = "protein_sets_is_validated"
  final val FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES: String = "protein_sets_nb_sameset_protein_matches"
  final val FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES: String = "protein_sets_nb_subset_protein_matches"
  final val FIELD_PROTEIN_SETS_COVERAGE: String = "protein_sets_coverage"
  final val FIELD_PROTEIN_SETS_MW: String = "protein_sets_mw"
  final val FIELD_PROTEIN_SETS_NB_SEQUENCES: String = "protein_sets_nb_sequences"
  final val FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES: String = "protein_sets_nb_specific_sequences"
  final val FIELD_PROTEIN_SETS_NB_PEPTIDES: String = "protein_sets_nb_peptides"
  final val FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES: String = "protein_sets_nb_specific_peptides"
  final val FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES: String = "protein_sets_nb_peptide_matches"
  final val FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES: String = "protein_sets_nb_specific_peptide_matches"
  final val FIELD_PROTEIN_SETS_SELECTION_LEVEL: String = "protein_sets_selection_level"

  final val FIELD_PROTEIN_SETS_QUANTI_STATUS: String = "protein_sets_status" //incremental
  final val FIELD_PROTEIN_SETS_QUANTI_PEPTIDE_NUMBER: String = "protein_sets_qc_peptide_number" //incremental
  final val FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE: String = "raw_abundance" //incremental // SC Specific 
  final val FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE: String = "abundance" // incremental // SC Weighted
  final val FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT: String = "psm_count" // incremental  // SC Basic
  final val FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO: String = "ratio" // incremental
  final val FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE: String = "t-test_pvalue" // incremental
  final val FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE: String = "z-test_pvalue" // incremental
  final val FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE: String = "z-score" // incremental

  // best psm sheet
  final val SHEET_BEST_PSM: String = "best_psm"
  // see FIELD_PSM* + FIELD_PROTEIN_SETS

  // protein match sheet  
  final val SHEET_PROTEIN_MATCH: String = "protein_match"
  // see  FIELD_PROTEIN_SETS_*
  final val FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN = "protein_match_is_typical_protein"
  final val FIELD_PROTEIN_MATCH_IS_SAMESET = "protein_match_is_sameset"
  final val FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE = "protein_match_peptide_set_score"

  // all psm sheet  
  final val SHEET_ALL_PSM: String = "all_psm"
  // see  also FIELD_PROTEIN_SETS_*
  final val FIELD_PSM_PEPTIDE_ID: String = "psm_peptide_id"
  final val FIELD_PSM_SEQUENCE: String = "psm_sequence"
  final val FIELD_PSM_MODIFICATIONS: String = "psm_modifications"
  final val FIELD_PSM_SCORE: String = "psm_score"
  final val FIELD_PSM_CALCULATED_MASS: String = "psm_calculated_mass"
  final val FIELD_PSM_CHARGE: String = "psm_charge"
  final val FIELD_PSM_EXPERIMENTAL_MOZ: String = "psm_experimental_moz"
  final val FIELD_PSM_DELTA_MOZ: String = "psm_delta_moz"
  final val FIELD_PSM_RT: String = "psm_rt"
  final val FIELD_PSM_PEPTIDE_LENGTH: String = "psm_peptide_length"
  final val FIELD_PSM_INITIAL_QUERY_ID: String = "psm_initial_query_id"
  final val FIELD_PSM_MISSED_CLEAVAGES: String = "psm_missed_cleavages"
  final val FIELD_PSM_RANK: String = "psm_rank"
  final val FIELD_PSM_CD_PRETTY_RANK: String = "psm_cd_pretty_rank"
  final val FIELD_PSM_FRAGMENT_MATCHES_COUNT: String = "psm_fragment_matches_count"
  final val FIELD_PSM_SPECTRUM_TITLE: String = "psm_spectrum_title"
  final val FIELD_PSM_NB_PROTEIN_SETS: String = "psm_nb_protein_sets"
  final val FIELD_PSM_NB_PROTEIN_MATCHES: String = "psm_nb_protein_matches"
  final val FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES: String = "psm_nb_databank_protein_matches"
  final val FIELD_PSM_START: String = "psm_start"
  final val FIELD_PSM_END: String = "psm_end"
  final val FIELD_PSM_RESIDUE_BEFORE: String = "psm_residue_before"
  final val FIELD_PSM_RESIDUE_AFTER: String = "psm_residue_after"

  final val FIELD_PSM_QUANTI_MASTER_QUANT_PEPTIDE_ID = "psm_master_quant_peptide_id"
  final val FIELD_PSM_QUANTI_ELUTION_TIME = "psm_quanti_elution_time"
  final val FIELD_PSM_QUANTI_SELECTION_LEVEL = "psm_quanti_selection_level"

  // masterQuantPeptideIon sheet  
  final val SHEET_MASTER_QUANT_PEPTIDE_ION: String = "master_quant_peptide_ion"
  final val FIELD_MASTER_QUANT_PEPTIDE_ION_ID = "master_quant_peptide_ion_id"
  final val FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME = "master_quant_peptide_ion_elution_time"
  final val FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE = "master_quant_peptide_ion_charge"
  final val FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID = "master_quant_peptide_ion_feature_id"

  // statistics sheet
  final val SHEET_STAT: String = "stat"

  final val FIELD_STAT_NB_PROTEIN_SETS: String = "stat_nb_protein_sets"
  final val FIELD_STAT_PSM_VALIDATION: String = "stat_psm_validation"
  final val FIELD_STAT_NB_TOTAL_PRECURSORS: String = "stat_nb_total_precursors"
  final val FIELD_STAT_NB_PROTEIN_SETS_SINGLE_SPECIFIC_PEPTIDE: String = "stat_nb_protein_sets_single_specific_peptide"
  final val FIELD_STAT_NB_MODIFIED_PEPTIDES: String = "stat_nb_modified_peptides"
  final val FIELD_STAT_NB_Z3_PRECURSORS: String = "stat_nb_z3_precursors"
  final val FIELD_STAT_NB_UNMODIFIED_PEPTIDES: String = "stat_nb_unmodified_peptides"
  final val FIELD_STAT_NB_PROTEIN_SETS_MULTI_SPECIFIC_PEPTIDE: String = "stat_nb_protein_sets_multi_specific_peptide"
  final val FIELD_STAT_NB_Z2_PRECURSORS: String = "stat_nb_z2_precursors"
  final val FIELD_STAT_NB_PEPTIDES: String = "stat_nb_peptides"
  final val FIELD_STAT_NB_DISTINCT_SEQ: String = "stat_nb_distinct_seq"
  final val FIELD_STAT_PROT_VALIDATION: String = "stat_prot_validation"

}