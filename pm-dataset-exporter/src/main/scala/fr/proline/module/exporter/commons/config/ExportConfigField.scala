package fr.proline.module.exporter.commons.config


import scala.Array.canBuildFrom

/**
 * represents the configuration for a field in the customizable export
 */
class ExportConfigField (
    var id :String, 
    var title  :String
)  {
	// Plain constructor
	def this() = this("", "")
}

object ExportConfigField {
  
  // get all fields for information sheet
	def getAllInformationFieldsArray() :Array[ExportConfigField]={
	  var  listFields  : Array[ExportConfigField]= Array(
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_PROJECT_NAME, "project_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_RESULT_SET_NAME, "result_set_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_SEARCH_TITLE, "search_title"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_SEARCH_DATE, "search_date"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_RAW_FILE_NAME, "raw_file_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_PEAKLIST_FILE_PATH, "peaklist_file_path"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_RESULT_FILE_NAME, "result_file_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_RESULT_FILE_DIRECTORY, "result_file_directory"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_JOB_NUMBER, "job_number"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_USER_NAME, "user_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_USER_EMAIL, "user_email"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_QUERIES_COUNT, "queries_count"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_SUBMITTED_QUERIES_COUNT, "submitted_queries_count"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_SEARCHED_SEQUENCES_COUNT, "searched_sequences_count"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_SOFTWARE_NAME, "software_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_INSTRUMENT_CONFIG, "instrument_config"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_DATABASE_NAMES, "database_names"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_DATABASE_RELEASES, "database_releases"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_TAXONOMY, "taxonomy"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_ENZYMES, "enzymes"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_MAX_MISSED_CLEAVAGES, "max_missed_cleavages"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_FIXED_PTMS, "fixed_ptms"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_VARIABLE_PTMS, "variable_ptms"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_PEPTIDE_CHARGE_STATES, "peptide_charge_states"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_PEPTIDE_MASS_ERROR_TOLERANCE, "peptide_mass_error_tolerance"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_FRAGMENT_MASS_ERROR_TOLERANCE, "fragment_mass_error_tolerance"),
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_IS_DECOY, "is_decoy")
	  )
	  
	  return listFields
	}
	
	
	    
	// get all fields for import sheet
	def getAllImportFieldsArray() :Array[ExportConfigField]={
	  var  listFields  : Array[ExportConfigField]= Array(
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PARAMS, "import_params"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR, "psm_filter_expected_fdr"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PSM_FILTER, "psm_filter"), // incremental
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR, "prot_filter_expected_fdr"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PROT_FILTER, "import_prot_filter") // incremental
	  )
	  
	  return listFields
	}
	    
	// get all fields for protein sets sheet
	def getAllProteinSetsFieldsArray() :Array[ExportConfigField]={
	  var  listFields  : Array[ExportConfigField]= Array(
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_ID, "protein_set_id"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_ACCESSION, "accession"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_DESCRIPTION, "description"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_SCORE, "score"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_IS_VALIDATED, "is_validated"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES, "#sameset_protein_matches"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES, "#subset_protein_matches"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_COVERAGE, "coverage"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_MW, "MW"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches")
	  )
	  
	  return listFields
	}
	
	// get all fields for protein match sheet
	def getAllProteinMatchFieldsArray() :Array[ExportConfigField]={
	  var  listFields  : Array[ExportConfigField]= Array(
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_ID, "protein_set_id"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_ACCESSION, "accession"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_DESCRIPTION, "description"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN, "is_typical_protein"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_MATCH_IS_SAMESET, "is_sameset"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE, "peptide_set_score"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_COVERAGE, "coverage"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_MW, "MW"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches")
	  )
	  
	  return listFields
	}
	
	
	// get all fields for Best PSM sheet
	def getAllBestPSMFieldsArray() :Array[ExportConfigField]={
	  var  listFields  : Array[ExportConfigField]= Array(
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_PEPTIDE_ID, "peptide_id"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_SEQUENCE, "sequence"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_MODIFICATIONS, "modifications"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_SCORE, "score"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_CALCULATED_MASS, "calculated_mass"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_CHARGE, "charge"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_EXPERIMENTAL_MOZ, "experimental_moz"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_DELTA_MOZ, "delta_moz"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_RT, "rt"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_PEPTIDE_LENGTH, "peptide_length"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_INITIAL_QUERY_ID, "initial_query_id"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_MISSED_CLEAVAGES, "missed_cleavages"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_RANK, "rank"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_CD_PRETTY_RANK, "cd_pretty_rank"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_FRAGMENT_MATCHES_COUNT, "fragment_matches_count"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_SPECTRUM_TITLE, "spectrum_title"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_NB_PROTEIN_SETS, "#protein_sets"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_NB_PROTEIN_MATCHES, "#protein_matches"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES, "#databank_protein_matches"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_START, "start"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_END, "end"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_RESIDUE_BEFORE, "residue_before"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_RESIDUE_AFTER, "residue_after")
	  )
	  listFields = listFields ++  getAllProteinSetsFieldsArray()
	  return listFields
	}
        
        
	
	// get all fields for all PSM sheet
	def getAllPSMFieldsArray() :Array[ExportConfigField]={
	  return getAllBestPSMFieldsArray()
	}
        
	// get all fields for stat sheet
	def getAllStatFieldsArray() :Array[ExportConfigField]={
	  var  listFields  : Array[ExportConfigField]= Array(
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_PROTEIN_SETS, "#protein_sets"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_PSM_VALIDATION, "psm_validation"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_TOTAL_PRECURSORS, "#total_precursors"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_PROTEIN_SETS_SINGLE_SPECIFIC_PEPTIDE, "#protein_sets_with_single_specific_peptide"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_MODIFIED_PEPTIDES, "#modified_peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_Z3_PRECURSORS, "#z3_precursors"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_UNMODIFIED_PEPTIDES, "#unmodified_peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_PROTEIN_SETS_MULTI_SPECIFIC_PEPTIDE, "#protein_sets_with_multiple_specific_peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_Z2_PRECURSORS, "#z2_precursors"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_PEPTIDES, "#peptides"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_NB_DISTINCT_SEQ, "#distinct_sequences"),
	  new ExportConfigField(ExportConfigConstant.FIELD_STAT_PROT_VALIDATION, "prot_validation")
	  )
	  
	  return listFields
	}
	
  
}