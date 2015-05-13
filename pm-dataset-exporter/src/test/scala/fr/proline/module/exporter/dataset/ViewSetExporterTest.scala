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


class ViewSetExporterTest extends AbstractMultipleDBTestCase with Logging {

  // Define some vars
  val driverType = DriverType.H2
  val fileName = "STR_F063442_F122817_MergedRSMs"
  val projectId = 1
  val targetRSMId: Long = 33 // 2 to have info data

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
  def testExportViewSetToDir() {
    val configStr = """{"format":"xlsx","decimal_separator":".","date_format":"YYYY:MM:DD HH:mm:ss","data_export":{"all_protein_set":true},
      "sheets":[{"id":"information","title":"search settings and infos","presentation":"rows",
      "fields":[{"id":"information_project_name","title":"project_name"},{"id":"information_result_set_name","title":"result_set_name"},{"id":"information_search_title","title":"search_title"},{"id":"information_search_date","title":"search_date"},{"id":"information_raw_file_name","title":"raw_file_name"},{"id":"information_peaklist_file_path","title":"peaklist_file_path"},{"id":"information_result_file_name","title":"result_file_name"},{"id":"information_result_file_directory","title":"result_file_directory"},{"id":"information_job_number","title":"job_number"},{"id":"information_user_name","title":"user_name"},{"id":"information_user_email","title":"user_email"},{"id":"information_queries_count","title":"queries_count"},{"id":"information_submitted_queries_count","title":"submitted_queries_count"},{"id":"information_searched_sequences_count","title":"searched_sequences_count"},{"id":"information_software_name","title":"software_name"},{"id":"information_instrument_config","title":"instrument_config"},{"id":"information_database_names","title":"database_names"},{"id":"information_database_releases","title":"database_releases"},{"id":"information_taxonomy","title":"taxonomy"},{"id":"information_enzymes","title":"enzymes"},{"id":"information_max_missed_cleavages","title":"max_missed_cleavages"},{"id":"information_fixed_ptms","title":"fixed_ptms"},{"id":"information_variable_ptms","title":"variable_ptms"},{"id":"information_peptide_charge_states","title":"peptide_charge_states"},{"id":"information_peptide_mass_error_tolerance","title":"peptide_mass_error_tolerance"},{"id":"information_fragment_mass_error_tolerance","title":"fragment_mass_error_tolerance"},{"id":"information_is_decoy","title":"is_decoy"}]},
      {"id":"import","title":"import and filters","presentation":"rows",
      "fields":[{"id":"information_result_file_name","title":"result_file_name"},{"id":"import_params","title":"import_params"},{"id":"import_psm_filter_expected_fdr","title":"psm_filter_expected_fdr"},{"id":"import_psm_filter","title":"psm_filter"},{"id":"import_prot_filter_expected_fdr","title":"prot_filter_expected_fdr"},{"id":"import_prot_filter","title":"import_prot_filter"}]},
      {"id":"protein_sets","title":"protein sets","presentation":"columns",
      "fields":[{"id":"protein_sets_id","title":"protein_set_id"},{"id":"protein_sets_accession","title":"accession"},{"id":"protein_sets_description","title":"description"},{"id":"protein_sets_score","title":"score"},{"id":"protein_sets_is_validated","title":"is_validated"},{"id":"protein_sets_selection_level","title":"selection Level"},{"id":"protein_sets_nb_sameset_protein_matches","title":"#sameset_protein_matches"},{"id":"protein_sets_nb_subset_protein_matches","title":"#subset_protein_matches"},{"id":"protein_sets_coverage","title":"coverage"},{"id":"protein_sets_mw","title":"MW"},{"id":"protein_sets_nb_sequences","title":"#sequences"},{"id":"protein_sets_nb_specific_sequences","title":"#specific_sequences"},{"id":"protein_sets_nb_peptides","title":"#peptides"},{"id":"protein_sets_nb_specific_peptides","title":"#specific_peptides"},{"id":"protein_sets_nb_peptide_matches","title":"#peptide_matches"},{"id":"protein_sets_nb_specific_peptide_matches","title":"#specific_peptide_matches"}]},{"id":"best_psm","title":"best PSM from protein sets","presentation":"columns","fields":[{"id":"psm_peptide_id","title":"peptide_id"},{"id":"psm_sequence","title":"sequence"},{"id":"psm_modifications","title":"modifications"},{"id":"psm_score","title":"score"},{"id":"psm_calculated_mass","title":"calculated_mass"},{"id":"psm_charge","title":"charge"},{"id":"psm_experimental_moz","title":"experimental_moz"},{"id":"psm_delta_moz","title":"delta_moz"},{"id":"psm_rt","title":"rt"},{"id":"psm_peptide_length","title":"peptide_length"},{"id":"psm_initial_query_id","title":"initial_query_id"},{"id":"psm_missed_cleavages","title":"missed_cleavages"},{"id":"psm_rank","title":"rank"},{"id":"psm_cd_pretty_rank","title":"cd_pretty_rank"},{"id":"psm_fragment_matches_count","title":"fragment_matches_count"},{"id":"psm_spectrum_title","title":"spectrum_title"},{"id":"psm_nb_protein_sets","title":"#protein_sets"},{"id":"psm_nb_protein_matches","title":"#protein_matches"},{"id":"psm_nb_databank_protein_matches","title":"#databank_protein_matches"},{"id":"psm_start","title":"start"},{"id":"psm_end","title":"end"},{"id":"psm_residue_before","title":"residue_before"},{"id":"psm_residue_after","title":"residue_after"},{"id":"protein_sets_id","title":"protein_set_id"},{"id":"protein_sets_accession","title":"accession"},{"id":"protein_sets_is_validated","title":"is_validated"},{"id":"protein_sets_description","title":"description"},{"id":"protein_sets_score","title":"protein_set_score"}]},{"id":"protein_match","title":"protein matches in protein set","presentation":"columns","fields":[{"id":"protein_sets_id","title":"protein_set_id"},{"id":"protein_sets_accession","title":"accession"},{"id":"protein_sets_description","title":"description"},{"id":"protein_match_is_typical_protein","title":"is_typical_protein"},{"id":"protein_match_is_sameset","title":"is_sameset"},{"id":"protein_match_peptide_set_score","title":"peptide_set_score"},{"id":"protein_sets_coverage","title":"coverage"},{"id":"protein_sets_mw","title":"MW"},{"id":"protein_sets_nb_sequences","title":"#sequences"},{"id":"protein_sets_nb_specific_sequences","title":"#specific_sequences"},{"id":"protein_sets_nb_peptides","title":"#peptides"},{"id":"protein_sets_nb_specific_peptides","title":"#specific_peptides"},{"id":"protein_sets_nb_peptide_matches","title":"#peptide_matches"},{"id":"protein_sets_nb_specific_peptide_matches","title":"#specific_peptide_matches"}]},{"id":"all_psm","title":"all PSMs from protein sets","presentation":"columns","fields":[{"id":"psm_peptide_id","title":"peptide_id"},{"id":"psm_sequence","title":"sequence"},{"id":"psm_modifications","title":"modifications"},{"id":"psm_score","title":"score"},{"id":"psm_calculated_mass","title":"calculated_mass"},{"id":"psm_charge","title":"charge"},{"id":"psm_experimental_moz","title":"experimental_moz"},{"id":"psm_delta_moz","title":"delta_moz"},{"id":"psm_rt","title":"rt"},{"id":"psm_peptide_length","title":"peptide_length"},{"id":"psm_initial_query_id","title":"initial_query_id"},{"id":"psm_missed_cleavages","title":"missed_cleavages"},{"id":"psm_rank","title":"rank"},{"id":"psm_cd_pretty_rank","title":"cd_pretty_rank"},{"id":"psm_fragment_matches_count","title":"fragment_matches_count"},{"id":"psm_spectrum_title","title":"spectrum_title"},{"id":"psm_nb_protein_sets","title":"#protein_sets"},{"id":"psm_nb_protein_matches","title":"#protein_matches"},{"id":"psm_nb_databank_protein_matches","title":"#databank_protein_matches"},{"id":"psm_start","title":"start"},{"id":"psm_end","title":"end"},{"id":"psm_residue_before","title":"residue_before"},{"id":"psm_residue_after","title":"residue_after"},{"id":"protein_sets_id","title":"protein_set_id"},{"id":"protein_sets_accession","title":"accession"},{"id":"protein_sets_is_validated","title":"is_validated"},{"id":"protein_sets_description","title":"description"},{"id":"protein_sets_score","title":"protein_set_score"}]},{"id":"stat","title":"statistics","presentation":"rows","fields":[{"id":"stat_nb_protein_sets","title":"#protein_sets"},{"id":"stat_psm_validation","title":"psm_validation"},{"id":"stat_nb_total_precursors","title":"#total_precursors"},{"id":"stat_nb_protein_sets_single_specific_peptide","title":"#protein_sets_with_single_specific_peptide"},{"id":"stat_nb_modified_peptides","title":"#modified_peptides"},{"id":"stat_nb_z3_precursors","title":"#z3_precursors"},{"id":"stat_nb_unmodified_peptides","title":"#unmodified_peptides"},{"id":"stat_nb_protein_sets_multi_specific_peptide","title":"#protein_sets_with_multiple_specific_peptides"},{"id":"stat_nb_z2_precursors","title":"#z2_precursors"},{"id":"stat_nb_peptides","title":"#peptides"},{"id":"stat_nb_distinct_seq","title":"#distinct_sequences"},{"id":"stat_prot_validation","title":"prot_validation"}]}]}"""
     
    val viewSet = BuildDatasetViewSet(
      executionContext,
      projectId,
      -1,
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