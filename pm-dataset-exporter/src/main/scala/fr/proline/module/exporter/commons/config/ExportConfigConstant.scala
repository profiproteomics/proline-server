package fr.proline.module.exporter.commons.config

object ExportConfigConstant {

  // Presentation of a sheet: by rows or columns
  val PRESENTATION_SHEET_ROWS = "rows"
  val PRESENTATION_SHEET_COLUMNS = "columns"
  val PRESENTATION_SHEET_VALUES: Array[String] = Array(PRESENTATION_SHEET_ROWS, PRESENTATION_SHEET_COLUMNS)
  
  // Format
  val FORMAT_XLSX = "xlsx"
  val FORMAT_TSV = "tsv"
  val FORMAT_VALUES: Array[String] = Array(FORMAT_XLSX, FORMAT_TSV)

  // Date format
  val DATE_FORMAT_HOUR = "yyyy:MM:dd HH:mm:ss"
  val DATE_FORMAT = "yyyy:MM:dd"
  val DATE_FORMAT_VALUES: Array[String] = Array(DATE_FORMAT_HOUR, DATE_FORMAT)

  // Separator title when the field is "incremental"
  val SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE = "_"
  val SEPARATOR_INCREMENTAL_TITLE_SPACE = " "
  val SEPARATOR_INCREMENTAL_TITLE_VALUES: Array[String] = Array(SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE, SEPARATOR_INCREMENTAL_TITLE_SPACE)

  val GROUP = "g"
  val VERSUS = "vs"

  val MODE_IDENT = "IDENT"
  val MODE_QUANT_SC = "QUANT_SC"
  val MODE_QUANT_XIC = "QUANT_XIC"

  // Decimal separator
  val DECIMAL_SEPARATOR_COMMA: Char = ','
  val DECIMAL_SEPARATOR_DOT: Char = '.'
  val DECIMAL_SPEARATOR_VALUES: Array[String] = Array(".", ",")

  // Format
  val MODIFICATION_FORMAT_ROUNDED_MONOMASS = "monomass"
//  val MODIFICATION_FORMAT_FULLNAME = "fullname"
  val MODIFICATION_FORMAT_FIRST_THREE_LETTERS = "first_three_letters"
  val MODIFICATION_FORMAT_VALUES: Array[String] = Array(MODIFICATION_FORMAT_ROUNDED_MONOMASS, /*MODIFICATION_FORMAT_FULLNAME,*/ MODIFICATION_FORMAT_FIRST_THREE_LETTERS)
  
  // Information sheet
  val SHEET_INFORMATION = "information"

  val FIELD_INFORMATION_PROJECT_NAME = "information_project_name"
  val FIELD_INFORMATION_RESULT_SET_NAME = "information_result_set_name"
  val FIELD_INFORMATION_SEARCH_TITLE = "information_search_title"
  val FIELD_INFORMATION_SEARCH_DATE = "information_search_date"
  val FIELD_INFORMATION_RAW_FILE_NAME = "information_raw_file_name"
  val FIELD_INFORMATION_PEAKLIST_FILE_PATH = "information_peaklist_file_path"
  val FIELD_INFORMATION_RESULT_FILE_NAME = "information_result_file_name"
  val FIELD_INFORMATION_RESULT_FILE_DIRECTORY = "information_result_file_directory"
  val FIELD_INFORMATION_JOB_NUMBER = "information_job_number"
  val FIELD_INFORMATION_USER_NAME = "information_user_name"
  val FIELD_INFORMATION_USER_EMAIL = "information_user_email"
  val FIELD_INFORMATION_QUERIES_COUNT = "information_queries_count"
  val FIELD_INFORMATION_SEARCHED_SEQUENCES_COUNT = "information_searched_sequences_count"
  val FIELD_INFORMATION_SOFTWARE_NAME = "information_software_name"
  val FIELD_INFORMATION_SOFTWARE_VERSION = "information_software_version"
  val FIELD_INFORMATION_INSTRUMENT_CONFIG = "information_instrument_config"
  val FIELD_INFORMATION_DATABASE_NAMES = "information_database_names"
  val FIELD_INFORMATION_DATABASE_RELEASES = "information_database_releases"
  val FIELD_INFORMATION_DATABASE_SEQUENCES_COUNT = "information_database_sequences_count"
  val FIELD_INFORMATION_TAXONOMY = "information_taxonomy"
  val FIELD_INFORMATION_ENZYMES = "information_enzymes"
  val FIELD_INFORMATION_MAX_MISSED_CLEAVAGES = "information_max_missed_cleavages"
  val FIELD_INFORMATION_FIXED_PTMS = "information_fixed_ptms"
  val FIELD_INFORMATION_VARIABLE_PTMS = "information_variable_ptms"
  val FIELD_INFORMATION_PEPTIDE_CHARGE_STATES = "information_peptide_charge_states"
  val FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE = "information_peptide_mass_error_tolerance"
  val FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE = "information_fragment_mass_error_tolerance"
  val FIELD_INFORMATION_IS_DECOY = "information_is_decoy"

  // Import sheet    
  val SHEET_IMPORT = "import"

  val FIELD_IMPORT_PARAMS = "import_params"
  val FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR = "import_psm_filter_expected_fdr"
  val FIELD_IMPORT_PSM_FILTER = "import_psm_filter" // incremental 
  val FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR = "import_prot_filter_expected_fdr"
  val FIELD_IMPORT_PROT_FILTER = "import_prot_filter" //incremental

  // Protein set sheet
  val SHEET_PROTEIN_SETS = "protein_sets"

  val FIELD_PROTEIN_SETS_ID = "protein_sets_id"
  val FIELD_PROTEIN_SETS_ACCESSION = "protein_sets_accession"
  val FIELD_PROTEIN_SETS_DESCRIPTION = "protein_sets_description"
  val FIELD_PROTEIN_SETS_SCORE = "protein_sets_score"
  val FIELD_PROTEIN_SETS_IS_VALIDATED = "protein_sets_is_validated"
  val FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES = "protein_sets_nb_sameset_protein_matches"
  val FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES = "protein_sets_nb_subset_protein_matches"
  val FIELD_PROTEIN_SETS_COVERAGE = "protein_sets_coverage"
  val FIELD_PROTEIN_SETS_MW = "protein_sets_mw"
  val FIELD_PROTEIN_SETS_NB_SEQUENCES = "protein_sets_nb_sequences"
  val FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES = "protein_sets_nb_specific_sequences"
  val FIELD_PROTEIN_SETS_NB_PEPTIDES = "protein_sets_nb_peptides"
  val FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES = "protein_sets_nb_specific_peptides"
  val FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES = "protein_sets_nb_peptide_matches"
  val FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES = "protein_sets_nb_specific_peptide_matches"
  val FIELD_PROTEIN_SETS_SELECTION_LEVEL = "protein_sets_selection_level"

  val FIELD_PROTEIN_SETS_QUANT_STATUS = "protein_sets_status" //incremental
  val FIELD_PROTEIN_SETS_QUANT_PEPTIDE_NUMBER = "protein_sets_qc_peptide_number" //incremental
  
  // FIXME: this is badly named => this not always relative to protein sets
  val FIELD_PROTEIN_SETS_QUANT_RAW_ABUNDANCE = "raw_abundance" //incremental // SC Specific 
  val FIELD_PROTEIN_SETS_QUANT_ABUNDANCE = "abundance" // incremental // SC Weighted
  val FIELD_PROTEIN_SETS_QUANT_PSM_COUNT = "psm_count" // incremental  // SC Basic

  // Best psm sheet
  val SHEET_BEST_PSM = "best_psm"
  // see FIELD_PSM* + FIELD_PROTEIN_SETS

  // Protein match sheet  
  val SHEET_PROTEIN_MATCH = "protein_match"
  // see  FIELD_PROTEIN_SETS_*
  val FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN = "protein_match_is_typical_protein"
  val FIELD_PROTEIN_MATCH_IS_SAMESET = "protein_match_is_sameset"
  val FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE = "protein_match_peptide_set_score"

  // All psm sheet  
  val SHEET_ALL_PSM = "all_psm"
  // see  also FIELD_PROTEIN_SETS_*
  val FIELD_PSM_PEPTIDE_ID = "psm_peptide_id"
  val FIELD_PSM_SEQUENCE = "psm_sequence"
  val FIELD_PSM_MODIFICATIONS = "psm_modifications"
  val FIELD_PSM_ID = "psm_id"
  val FIELD_PSM_SCORE = "psm_score"
  val FIELD_PSM_CALCULATED_MASS = "psm_calculated_mass"
  val FIELD_PSM_CHARGE = "psm_charge"
  val FIELD_PSM_EXPERIMENTAL_MOZ = "psm_experimental_moz"
  val FIELD_PSM_DELTA_MOZ = "psm_delta_moz"
  val FIELD_PSM_RT = "psm_rt"
  val FIELD_PSM_PEPTIDE_LENGTH = "psm_peptide_length"
  val FIELD_PSM_INITIAL_QUERY_ID = "psm_initial_query_id"
  val FIELD_PSM_MISSED_CLEAVAGES = "psm_missed_cleavages"
  val FIELD_PSM_RANK = "psm_rank"
  val FIELD_PSM_CD_PRETTY_RANK = "psm_cd_pretty_rank"
  val FIELD_PSM_FRAGMENT_MATCHES_COUNT = "psm_fragment_matches_count"
  val FIELD_PSM_SPECTRUM_TITLE = "psm_spectrum_title"
  val FIELD_PSM_NB_PROTEIN_SETS = "psm_nb_protein_sets"
  val FIELD_PSM_NB_SAMESET_PROTEIN_MATCHES = "psm_nb_sameset_protein_matches"
  val FIELD_PSM_NB_PROTEIN_MATCHES = "psm_nb_protein_matches"
  val FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES = "psm_nb_databank_protein_matches"
  val FIELD_PSM_START = "psm_start"
  val FIELD_PSM_END = "psm_end"
  val FIELD_PSM_RESIDUE_BEFORE = "psm_residue_before"
  val FIELD_PSM_RESIDUE_AFTER = "psm_residue_after"
  val FIELD_PSM_PTM_SCORE = "psm_ptm_score"
  val FIELD_PSM_PTM_SITES_CONFIDENCE = "psm_ptm_sites_confidence"

  val FIELD_PSM_QUANT_MASTER_QUANT_PEPTIDE_ID = "psm_master_quant_peptide_id"
  val FIELD_PSM_QUANT_ELUTION_TIME = "psm_quant_elution_time"
  val FIELD_PSM_QUANT_SELECTION_LEVEL = "psm_quant_selection_level"

  // MasterQuantPeptideIon sheet  
  val SHEET_MASTER_QUANT_PEPTIDE_ION = "master_quant_peptide_ion"
  val FIELD_QUANT_PEPTIDE_ION_ELUTION_TIME = "quant_peptide_ion_elution_time" //incremental
  val FIELD_QUANT_PEPTIDE_ION_BEST_SCORE = "quant_peptide_ion_best_score" //incremental
  val FIELD_MASTER_QUANT_PEPTIDE_ION_ID = "master_quant_peptide_ion_id"
  val FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME = "master_quant_peptide_ion_elution_time"
  val FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE = "master_quant_peptide_ion_charge"
  val FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID = "master_quant_peptide_ion_feature_id"
  val FIELD_MASTER_QUANT_PEPTIDE_ION_MOZ = "master_quant_peptide_ion_moz"
  
  // Some properties shared between quantitation sheets
  val FIELD_PROFILIZER_RATIO = "ratio" // incremental
  val FIELD_PROFILIZER_TTEST_PVALUE = "t-test_pvalue" // incremental
  val FIELD_PROFILIZER_ZTEST_PVALUE = "z-test_pvalue" // incremental
  val FIELD_PROFILIZER_ZSCORE = "z-score" // incremental

  // Statistics sheet
  val SHEET_STAT = "stat"

  val FIELD_STAT_PSM_VALIDATION = "stat_psm_validation"
  val FIELD_STAT_NB_TOTAL_PSMS = "stat_nb_total_psms"
  val FIELD_STAT_NB_TOTAL_PRECURSORS = "stat_nb_total_precursors"
  val FIELD_STAT_NB_Z2_PRECURSORS = "stat_nb_z2_precursors"
  val FIELD_STAT_NB_Z3_PRECURSORS = "stat_nb_z3_precursors"
  val FIELD_STAT_NB_Z4PLUS_PRECURSORS = "stat_nb_z4+_precursors"
  val FIELD_STAT_NB_PEPTIDES = "stat_nb_peptides"
  val FIELD_STAT_NB_MODIFIED_PEPTIDES = "stat_nb_modified_peptides"
  val FIELD_STAT_NB_UNMODIFIED_PEPTIDES = "stat_nb_unmodified_peptides"
  val FIELD_STAT_NB_DISTINCT_SEQS = "stat_nb_distinct_seqs"
  val FIELD_STAT_PROT_VALIDATION = "stat_prot_validation"
  val FIELD_STAT_NB_PROTEIN_SETS = "stat_nb_protein_sets"
  val FIELD_STAT_NB_VALIDATED_PROT_SETS = "stat_nb_validated_prot_sets"
  val FIELD_STAT_NB_VALIDATED_PROT_SETS_SINGLE_PEPTIDE = "stat_nb_validated_prot_sets_single_peptide"
  val FIELD_STAT_NB_VALIDATED_PROT_SETS_MULTI_PEPTIDES = "stat_nb_validated_prot_sets_multi_peptides"
  val FIELD_STAT_NB_VALIDATED_PROT_SETS_SINGLE_SPE_PEP_SEQUENCE = "stat_nb_validated_prot_sets_single_spe_pep_sequence"
  val FIELD_STAT_NB_VALIDATED_PROT_SETS_MULTI_SPE_PEP_SEQUENCES = "stat_nb_validated_prot_sets_multi_spe_pep_sequences"

  // Statistics sheet
  val SHEET_QUANT_CONFIG = "quant_config"
}