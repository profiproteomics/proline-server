package fr.proline.module.exporter.commons.config


import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer

/**
 * represents the configuration for a field in the customizable export
 */
class ExportConfigField (
    var id :String, 
    var title  :String
)  {
	// Plain constructor
	def this() = this("", "")
	var defaultDisplayed: Boolean = true
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
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_SOFTWARE_VERSION, "software_version"),
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
	  new ExportConfigField(ExportConfigConstant.FIELD_INFORMATION_RESULT_FILE_NAME, "result_file_name"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PARAMS, "import_params"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PSM_FILTER_EXPECTED_FDR, "psm_filter_expected_fdr"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PSM_FILTER, "psm_filter"), // incremental
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PROT_FILTER_EXPECTED_FDR, "prot_filter_expected_fdr"),
	  new ExportConfigField(ExportConfigConstant.FIELD_IMPORT_PROT_FILTER, "import_prot_filter") // incremental
	  )
	  
	  return listFields
	}
	    
	// get all fields for protein sets sheet
	def getAllProteinSetsFieldsArray(fromProtein: Boolean, fromXIC: Boolean, fromSC: Boolean) :Array[ExportConfigField]={
	  val fieldId : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_ID, "protein_set_id")
	  val fieldAcc : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_ACCESSION, "accession")
	  val fieldDesc : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_DESCRIPTION, "description")
	  val fieldScore : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_SCORE, "protein_set_score")
	  val fieldIsVal : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_IS_VALIDATED, "is_validated")
	  val fieldSelLevel : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_SELECTION_LEVEL, "selection_level")
	  val fieldNbSameset : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES, "#sameset_protein_matches")
	  val fieldNbSubset : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES, "#subset_protein_matches")
	  val fieldCoverage : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_COVERAGE, "coverage")
	  val fieldMw : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_MW, "MW")
	  val fieldNbSeq : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SEQUENCES, "#sequences")
	  val fieldNbSpecSeq : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES, "#specific_sequences")
	  val fieldNbPep : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDES, "#peptides")
	  val fieldNbSpecPep : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES, "#specific_peptides")
	  val fieldNbPepMatch : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES, "#peptide_matches")
	  val fieldNbSpecPepMatch : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES, "#specific_peptide_matches")
	  
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
	  
	    
	  var  listFields  : ArrayBuffer[ExportConfigField]= ArrayBuffer(
	  fieldId, fieldAcc, fieldDesc, fieldScore, fieldIsVal,fieldSelLevel,
	  fieldNbSameset, fieldNbSubset, fieldCoverage, fieldMw, fieldNbSeq, fieldNbSpecSeq, fieldNbPep, fieldNbSpecPep, fieldNbPepMatch, fieldNbSpecPepMatch
	  )
	  
	  if (fromXIC || fromSC) {
	    val fieldRawAbundance : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE, if (fromXIC) "raw_abundance" else "Specific SC") 
	    val fieldAbundance : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE, if (fromXIC) "abundance" else "Weighted SC") 
	    val fieldPsmCount : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT, if (fromXIC) "psm_count" else "Basic SC") 
	    val fieldStatus : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_STATUS, "status")
	    val fieldPeptideNumber : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PEPTIDE_NUMBER, "peptide_number")
	    fieldStatus.defaultDisplayed = fromSC
	    fieldPeptideNumber.defaultDisplayed = fromSC
	    listFields += fieldStatus
	    listFields += fieldPeptideNumber
	    listFields += fieldPsmCount
	    listFields += fieldRawAbundance
	    listFields += fieldAbundance
	  }
	  if (fromXIC){
	    val fieldRatio: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO, "ratio")
	    val fieldTTest: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE, "t-test")
	    val fieldZTest: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE, "z-test")
	    val fieldZScore: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE, "z-score")
	    listFields += fieldRatio
	    listFields += fieldTTest
	    listFields += fieldZTest
	    listFields += fieldZScore
	  }
	  
	  return listFields.toArray
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
	def getAllBestPSMFieldsArray(fromXIC: Boolean, fromSC: Boolean) :Array[ExportConfigField]={
	  var  listFields  : ArrayBuffer[ExportConfigField]= ArrayBuffer(
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_PEPTIDE_ID, "peptide_id"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_SEQUENCE, "sequence"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_MODIFICATIONS, "modifications"),
	  new ExportConfigField(ExportConfigConstant.FIELD_PSM_SCORE, "psm_score"),
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
	  listFields = listFields ++  getAllProteinSetsFieldsArray(false, false, false)
	  
	  if (fromXIC || fromSC) {
	    val fieldMasterQuantPeptideId: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PSM_QUANTI_MASTER_QUANT_PEPTIDE_ID, "master_quant_peptide_id")
	    val fieldQuantiElutionTime: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PSM_QUANTI_ELUTION_TIME, "elution_time")
	    val fieldQuantiSelectionLevel: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PSM_QUANTI_SELECTION_LEVEL, "selection_level")
	    val fieldRawAbundance : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE, if (fromXIC) "raw_abundance" else "Specific SC") 
	    val fieldAbundance : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE, if (fromXIC) "abundance" else "Weighted SC") 
	    val fieldPsmCount : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT, if (fromXIC) "psm_count" else "Basic SC") 
	    listFields += fieldMasterQuantPeptideId
	    listFields += fieldQuantiElutionTime
	    listFields += fieldQuantiSelectionLevel
	    listFields += fieldPsmCount
	    listFields += fieldRawAbundance
	    listFields += fieldAbundance
	  }
	  if (fromXIC){
	    val fieldRatio: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO, "ratio")
	    val fieldTTest: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE, "t-test")
	    val fieldZTest: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE, "z-test")
	    val fieldZScore: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE, "z-score")
	    listFields += fieldRatio
	    listFields += fieldTTest
	    listFields += fieldZTest
	    listFields += fieldZScore
	  }
	  return listFields.toArray
	}
        
        
	
	// get all fields for all PSM sheet
	def getAllPSMFieldsArray(fromXIC: Boolean, fromSC: Boolean) :Array[ExportConfigField]={
	  return getAllBestPSMFieldsArray(fromXIC, fromSC)
	}
	
	// get all fields for masterQuantPeptideIon
	def getAllMasterQuantPeptideIon() :Array[ExportConfigField]={
	  var  listFields  : ArrayBuffer[ExportConfigField]= new ArrayBuffer()
	  listFields = listFields ++  getAllProteinSetsFieldsArray(false, false, false)
	  val fieldMasterQuantPeptideId: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PSM_QUANTI_MASTER_QUANT_PEPTIDE_ID, "master_quant_peptide_id")
	    val fieldQuantiElutionTime: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PSM_QUANTI_ELUTION_TIME, "elution_time")
	    val fieldQuantiSelectionLevel: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PSM_QUANTI_SELECTION_LEVEL, "selection_level")
	    val fieldRawAbundance : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE, "raw_abundance") 
	    val fieldAbundance : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE, "abundance" ) 
	    val fieldPsmCount : ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT, "psm_count") 
	    val fieldRatio: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO, "ratio")
	    val fieldTTest: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE, "t-test")
	    val fieldZTest: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE, "z-test")
	    val fieldZScore: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE, "z-score")
	    val fieldQuantPeptideIon: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_MASTER_QUANT_PEPTIDE_ION_ID, "master_quant_peptide_ion")
	    val fieldQuantPeptideIonCharge: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE, "master_quant_peptide_ion_charge")
	    val fieldQuantPeptideIonElutionTime: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME, "master_quant_peptide_ion_elution_time")
	    val fieldQuantPeptideIonFeatureId: ExportConfigField = new ExportConfigField(ExportConfigConstant.FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID, "master_quant_peptide_ion_feature_id")
	    listFields += fieldMasterQuantPeptideId
	    listFields += fieldQuantiElutionTime
	    listFields += fieldQuantiSelectionLevel
	    listFields += fieldPsmCount
	    listFields += fieldRawAbundance
	    listFields += fieldAbundance
	    listFields += fieldRatio
	    listFields += fieldTTest
	    listFields += fieldZTest
	    listFields += fieldZScore
	    listFields += fieldQuantPeptideIon
	    listFields += fieldQuantPeptideIonCharge
	    listFields += fieldQuantPeptideIonElutionTime
	    listFields += fieldQuantPeptideIonFeatureId
	  return listFields.toArray
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