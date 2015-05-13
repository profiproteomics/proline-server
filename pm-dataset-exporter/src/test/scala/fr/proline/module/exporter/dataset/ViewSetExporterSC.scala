package fr.proline.module.exporter.dataset


import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import org.junit._
import org.junit.Assert._
import java.util.ArrayList
import org.apache.commons.io.FileUtils
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.module.exporter.commons.ViewSetExporter
import fr.proline.repository.DriverType
import scala.io.Source
import java.io.File
import fr.proline.core.orm.uds.Dataset
import fr.proline.module.exporter.dataset.view.BuildDatasetViewSet


class ViewSetExporterSCTest extends AbstractMultipleDBTestCase with Logging {

  // Define some vars
  val driverType = DriverType.H2
  val fileName = "STR_F063442_F122817_MergedRSMs_SC"
  val projectId = 1
  val datasetId = 2
  val targetRSMId: Long = 34 // 2 to have info data

  var executionContext: IExecutionContext = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {
    
    java.util.Locale.setDefault(new java.util.Locale("en", "US"))

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    // Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")
    psDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/ps-db.xml")
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs successfully initialized !")

    executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }
  
  @Test
  def testExportViewSetSCToDir() {
    val configStr = """{"format":"xlsx","decimal_separator":".","date_format":"YYYY:MM:DD HH:mm:ss","title_separator":"_","data_export":{"all_protein_set":true,"best_profile":true},"sheets":[{"id":"information","title":"search settings and infos","presentation":"rows","fields":[{"id":"information_project_name","title":"project_name","default_displayed":true},{"id":"information_result_set_name","title":"result_set_name","default_displayed":true},{"id":"information_search_title","title":"search_title","default_displayed":true},{"id":"information_search_date","title":"search_date","default_displayed":true},{"id":"information_raw_file_name","title":"raw_file_name","default_displayed":true},{"id":"information_peaklist_file_path","title":"peaklist_file_path","default_displayed":true},{"id":"information_result_file_name","title":"result_file_name","default_displayed":true},{"id":"information_result_file_directory","title":"result_file_directory","default_displayed":true},{"id":"information_job_number","title":"job_number","default_displayed":true},{"id":"information_user_name","title":"user_name","default_displayed":true},{"id":"information_user_email","title":"user_email","default_displayed":true},{"id":"information_queries_count","title":"queries_count","default_displayed":true},{"id":"information_submitted_queries_count","title":"submitted_queries_count","default_displayed":true},{"id":"information_searched_sequences_count","title":"searched_sequences_count","default_displayed":true},{"id":"information_software_name","title":"software_name","default_displayed":true},{"id":"information_instrument_config","title":"instrument_config","default_displayed":true},{"id":"information_database_names","title":"database_names","default_displayed":true},{"id":"information_database_releases","title":"database_releases","default_displayed":true},{"id":"information_taxonomy","title":"taxonomy","default_displayed":true},{"id":"information_enzymes","title":"enzymes","default_displayed":true},{"id":"information_max_missed_cleavages","title":"max_missed_cleavages","default_displayed":true},{"id":"information_fixed_ptms","title":"fixed_ptms","default_displayed":true},{"id":"information_variable_ptms","title":"variable_ptms","default_displayed":true},{"id":"information_peptide_charge_states","title":"peptide_charge_states","default_displayed":true},{"id":"information_peptide_mass_error_tolerance","title":"peptide_mass_error_tolerance","default_displayed":true},{"id":"information_fragment_mass_error_tolerance","title":"fragment_mass_error_tolerance","default_displayed":true},{"id":"information_is_decoy","title":"is_decoy","default_displayed":true}],"default_displayed":true},{"id":"import","title":"import and filters","presentation":"rows","fields":[{"id":"information_result_file_name","title":"result_file_name","default_displayed":true},{"id":"import_params","title":"import_params","default_displayed":true},{"id":"import_psm_filter_expected_fdr","title":"psm_filter_expected_fdr","default_displayed":true},{"id":"import_psm_filter","title":"psm_filter","default_displayed":true},{"id":"import_prot_filter_expected_fdr","title":"prot_filter_expected_fdr","default_displayed":true},{"id":"import_prot_filter","title":"import_prot_filter","default_displayed":true}],"default_displayed":true},{"id":"protein_sets","title":"protein sets","presentation":"columns","fields":[{"id":"protein_sets_id","title":"protein_set_id","default_displayed":true},{"id":"protein_sets_accession","title":"accession","default_displayed":true},{"id":"protein_sets_description","title":"description","default_displayed":true},{"id":"protein_sets_score","title":"score","default_displayed":true},{"id":"protein_sets_is_validated","title":"is_validated","default_displayed":true},{"id":"protein_sets_selection_level","title":"selection_level","default_displayed":true},{"id":"protein_sets_nb_sameset_protein_matches","title":"#sameset_protein_matches","default_displayed":true},{"id":"protein_sets_nb_subset_protein_matches","title":"#subset_protein_matches","default_displayed":true},{"id":"protein_sets_coverage","title":"coverage","default_displayed":true},{"id":"protein_sets_mw","title":"MW","default_displayed":true},{"id":"protein_sets_nb_sequences","title":"#sequences","default_displayed":true},{"id":"protein_sets_nb_specific_sequences","title":"#specific_sequences","default_displayed":true},{"id":"protein_sets_nb_peptides","title":"#peptides","default_displayed":true},{"id":"protein_sets_nb_specific_peptides","title":"#specific_peptides","default_displayed":true},{"id":"protein_sets_nb_peptide_matches","title":"#peptide_matches","default_displayed":true},{"id":"protein_sets_nb_specific_peptide_matches","title":"#specific_peptide_matches","default_displayed":true},{"id":"protein_sets_status","title":"status","default_displayed":true},{"id":"protein_sets_qc_peptide_number","title":"peptide_number","default_displayed":true},{"id":"psm_count","title":"Basic SC","default_displayed":true},{"id":"raw_abundance","title":"Specific SC","default_displayed":true},{"id":"abundance","title":"Weighted SC","default_displayed":true}],"default_displayed":true},{"id":"best_psm","title":"best PSM from protein sets","presentation":"columns","fields":[{"id":"psm_peptide_id","title":"peptide_id","default_displayed":true},{"id":"psm_sequence","title":"sequence","default_displayed":true},{"id":"psm_modifications","title":"modifications","default_displayed":true},{"id":"psm_score","title":"score","default_displayed":true},{"id":"psm_calculated_mass","title":"calculated_mass","default_displayed":true},{"id":"psm_charge","title":"charge","default_displayed":true},{"id":"psm_experimental_moz","title":"experimental_moz","default_displayed":true},{"id":"psm_delta_moz","title":"delta_moz","default_displayed":true},{"id":"psm_rt","title":"rt","default_displayed":true},{"id":"psm_peptide_length","title":"peptide_length","default_displayed":true},{"id":"psm_initial_query_id","title":"initial_query_id","default_displayed":true},{"id":"psm_missed_cleavages","title":"missed_cleavages","default_displayed":true},{"id":"psm_rank","title":"rank","default_displayed":true},{"id":"psm_cd_pretty_rank","title":"cd_pretty_rank","default_displayed":true},{"id":"psm_fragment_matches_count","title":"fragment_matches_count","default_displayed":true},{"id":"psm_spectrum_title","title":"spectrum_title","default_displayed":true},{"id":"psm_nb_protein_sets","title":"#protein_sets","default_displayed":true},{"id":"psm_nb_protein_matches","title":"#protein_matches","default_displayed":true},{"id":"psm_nb_databank_protein_matches","title":"#databank_protein_matches","default_displayed":true},{"id":"psm_start","title":"start","default_displayed":true},{"id":"psm_end","title":"end","default_displayed":true},{"id":"psm_residue_before","title":"residue_before","default_displayed":true},{"id":"psm_residue_after","title":"residue_after","default_displayed":true},{"id":"protein_sets_id","title":"protein_set_id","default_displayed":true},{"id":"protein_sets_accession","title":"accession","default_displayed":true},{"id":"protein_sets_description","title":"description","default_displayed":true},{"id":"protein_sets_score","title":"score","default_displayed":true},{"id":"protein_sets_is_validated","title":"is_validated","default_displayed":true},{"id":"protein_sets_selection_level","title":"selection_level","default_displayed":false},{"id":"protein_sets_nb_sameset_protein_matches","title":"#sameset_protein_matches","default_displayed":false},{"id":"protein_sets_nb_subset_protein_matches","title":"#subset_protein_matches","default_displayed":false},{"id":"protein_sets_coverage","title":"coverage","default_displayed":false},{"id":"protein_sets_mw","title":"MW","default_displayed":false},{"id":"protein_sets_nb_sequences","title":"#sequences","default_displayed":false},{"id":"protein_sets_nb_specific_sequences","title":"#specific_sequences","default_displayed":false},{"id":"protein_sets_nb_peptides","title":"#peptides","default_displayed":false},{"id":"protein_sets_nb_specific_peptides","title":"#specific_peptides","default_displayed":false},{"id":"protein_sets_nb_peptide_matches","title":"#peptide_matches","default_displayed":false},{"id":"protein_sets_nb_specific_peptide_matches","title":"#specific_peptide_matches","default_displayed":false},{"id":"psm_master_quant_peptide_id","title":"master_quant_peptide_id","default_displayed":true},{"id":"psm_quanti_elution_time","title":"elution_time","default_displayed":true},{"id":"psm_quanti_selection_level","title":"selection_level","default_displayed":true},{"id":"psm_count","title":"Basic SC","default_displayed":true},{"id":"raw_abundance","title":"Specific SC","default_displayed":true},{"id":"abundance","title":"Weighted SC","default_displayed":true}],"default_displayed":false},{"id":"protein_match","title":"protein matches in protein set","presentation":"columns","fields":[{"id":"protein_sets_id","title":"protein_set_id","default_displayed":true},{"id":"protein_sets_accession","title":"accession","default_displayed":true},{"id":"protein_sets_description","title":"description","default_displayed":true},{"id":"protein_match_is_typical_protein","title":"is_typical_protein","default_displayed":true},{"id":"protein_match_is_sameset","title":"is_sameset","default_displayed":true},{"id":"protein_match_peptide_set_score","title":"peptide_set_score","default_displayed":true},{"id":"protein_sets_coverage","title":"coverage","default_displayed":true},{"id":"protein_sets_mw","title":"MW","default_displayed":true},{"id":"protein_sets_nb_sequences","title":"#sequences","default_displayed":true},{"id":"protein_sets_nb_specific_sequences","title":"#specific_sequences","default_displayed":true},{"id":"protein_sets_nb_peptides","title":"#peptides","default_displayed":true},{"id":"protein_sets_nb_specific_peptides","title":"#specific_peptides","default_displayed":true},{"id":"protein_sets_nb_peptide_matches","title":"#peptide_matches","default_displayed":true},{"id":"protein_sets_nb_specific_peptide_matches","title":"#specific_peptide_matches","default_displayed":true}],"default_displayed":true},{"id":"all_psm","title":"all PSMs from protein sets","presentation":"columns","fields":[{"id":"psm_peptide_id","title":"peptide_id","default_displayed":true},{"id":"psm_sequence","title":"sequence","default_displayed":true},{"id":"psm_modifications","title":"modifications","default_displayed":true},{"id":"psm_score","title":"score","default_displayed":true},{"id":"psm_calculated_mass","title":"calculated_mass","default_displayed":true},{"id":"psm_charge","title":"charge","default_displayed":true},{"id":"psm_experimental_moz","title":"experimental_moz","default_displayed":true},{"id":"psm_delta_moz","title":"delta_moz","default_displayed":true},{"id":"psm_rt","title":"rt","default_displayed":true},{"id":"psm_peptide_length","title":"peptide_length","default_displayed":true},{"id":"psm_initial_query_id","title":"initial_query_id","default_displayed":true},{"id":"psm_missed_cleavages","title":"missed_cleavages","default_displayed":true},{"id":"psm_rank","title":"rank","default_displayed":true},{"id":"psm_cd_pretty_rank","title":"cd_pretty_rank","default_displayed":true},{"id":"psm_fragment_matches_count","title":"fragment_matches_count","default_displayed":true},{"id":"psm_spectrum_title","title":"spectrum_title","default_displayed":true},{"id":"psm_nb_protein_sets","title":"#protein_sets","default_displayed":true},{"id":"psm_nb_protein_matches","title":"#protein_matches","default_displayed":true},{"id":"psm_nb_databank_protein_matches","title":"#databank_protein_matches","default_displayed":true},{"id":"psm_start","title":"start","default_displayed":true},{"id":"psm_end","title":"end","default_displayed":true},{"id":"psm_residue_before","title":"residue_before","default_displayed":true},{"id":"psm_residue_after","title":"residue_after","default_displayed":true},{"id":"protein_sets_id","title":"protein_set_id","default_displayed":true},{"id":"protein_sets_accession","title":"accession","default_displayed":true},{"id":"protein_sets_description","title":"description","default_displayed":true},{"id":"protein_sets_score","title":"score","default_displayed":true},{"id":"protein_sets_is_validated","title":"is_validated","default_displayed":true},{"id":"protein_sets_selection_level","title":"selection_level","default_displayed":false},{"id":"protein_sets_nb_sameset_protein_matches","title":"#sameset_protein_matches","default_displayed":false},{"id":"protein_sets_nb_subset_protein_matches","title":"#subset_protein_matches","default_displayed":false},{"id":"protein_sets_coverage","title":"coverage","default_displayed":false},{"id":"protein_sets_mw","title":"MW","default_displayed":false},{"id":"protein_sets_nb_sequences","title":"#sequences","default_displayed":false},{"id":"protein_sets_nb_specific_sequences","title":"#specific_sequences","default_displayed":false},{"id":"protein_sets_nb_peptides","title":"#peptides","default_displayed":false},{"id":"protein_sets_nb_specific_peptides","title":"#specific_peptides","default_displayed":false},{"id":"protein_sets_nb_peptide_matches","title":"#peptide_matches","default_displayed":false},{"id":"protein_sets_nb_specific_peptide_matches","title":"#specific_peptide_matches","default_displayed":false},{"id":"psm_master_quant_peptide_id","title":"master_quant_peptide_id","default_displayed":true},{"id":"psm_quanti_elution_time","title":"elution_time","default_displayed":true},{"id":"psm_quanti_selection_level","title":"selection_level","default_displayed":true},{"id":"psm_count","title":"Basic SC","default_displayed":true},{"id":"raw_abundance","title":"Specific SC","default_displayed":true},{"id":"abundance","title":"Weighted SC","default_displayed":true}],"default_displayed":false},{"id":"stat","title":"statistics","presentation":"rows","fields":[{"id":"stat_nb_protein_sets","title":"#protein_sets","default_displayed":true},{"id":"stat_psm_validation","title":"psm_validation","default_displayed":true},{"id":"stat_nb_total_precursors","title":"#total_precursors","default_displayed":true},{"id":"stat_nb_protein_sets_single_specific_peptide","title":"#protein_sets_with_single_specific_peptide","default_displayed":true},{"id":"stat_nb_modified_peptides","title":"#modified_peptides","default_displayed":true},{"id":"stat_nb_z3_precursors","title":"#z3_precursors","default_displayed":true},{"id":"stat_nb_unmodified_peptides","title":"#unmodified_peptides","default_displayed":true},{"id":"stat_nb_protein_sets_multi_specific_peptide","title":"#protein_sets_with_multiple_specific_peptides","default_displayed":true},{"id":"stat_nb_z2_precursors","title":"#z2_precursors","default_displayed":true},{"id":"stat_nb_peptides","title":"#peptides","default_displayed":true},{"id":"stat_nb_distinct_seq","title":"#distinct_sequences","default_displayed":true},{"id":"stat_prot_validation","title":"prot_validation","default_displayed":true}],"default_displayed":false}],"format_values":["xlsx","tsv"],"decimal_separator_values":[".",","],"date_format_values":["YYYY:MM:DD HH:mm:ss","YYYY:MM:DD"],"title_separator_values":["_"," "],"sheet_presentation_values":["rows","columns"]}"""
    val viewSet = BuildDatasetViewSet(
      executionContext,
      projectId,
      datasetId,
      targetRSMId,      
      fileName,
      configStr
    )
    
    // Create TEMP dir
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile
    //val exportLoc = new File("C:\\Users\\mbodin00\\Desktop\\test\\")
    
    var exception = Option.empty[Exception]
    try {
      
      // Export the RSM
      ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc )
      
      //println(exportLoc)
      //Thread.sleep(100000)
      
      // TODO: compare the content of the exported file with an expected content
      
    } catch {
      case e: Exception => exception = Some(e)
    }
    finally {
      // Remove temp dir
      FileUtils.deleteDirectory(exportLoc)
    }

    // Throw exception if catched
    exception.map( throw _ )

  }
  
  
  

}