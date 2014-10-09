package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.template.BasicXLSXTemplate
import fr.proline.module.exporter.commons.template.InfoXLSXTemplate
import fr.proline.module.exporter.msi.view.ProtSetToPepMatchViewFields
import fr.proline.module.exporter.msi.view.MsiSearchExtendedViewFields
import fr.proline.module.exporter.msi.view.ResultSummaryViewTypes

object ProlineViewSetTemplateAsXLSX extends IViewSetTemplate {

  // Create a generic XLSX template for views
  private val basicXlsxTemplate = new BasicXLSXTemplate()
  
  // TODO: rename InfoXLSXTemplate into VerticalXLSXTemplate
  private val verticalXlsxTemplate = new InfoXLSXTemplate()
  
  // Create an XLSX template specific to the infos view
  private val infoXlsxTemplate = new InfoXLSXTemplate(
    selectedFields = Some(
      Seq(
        MsiSearchExtendedViewFields.JOB_NUMBER,
        MsiSearchExtendedViewFields.USER_NAME,
        MsiSearchExtendedViewFields.USER_EMAIL,
        MsiSearchExtendedViewFields.QUERIES_COUNT,
        MsiSearchExtendedViewFields.SUBMITTED_QUERIES_COUNT,
        MsiSearchExtendedViewFields.SEARCHED_SEQUENCES_COUNT,
        MsiSearchExtendedViewFields.SOFTWARE_NAME,
        MsiSearchExtendedViewFields.SOFTWARE_VERSION,
        MsiSearchExtendedViewFields.INSTRUMENT_CONFIG,
        MsiSearchExtendedViewFields.DATABASES_NAMES,
        MsiSearchExtendedViewFields.DATABASES_RELEASES,
        MsiSearchExtendedViewFields.TAXONOMY,
        MsiSearchExtendedViewFields.ENZYMES,
        MsiSearchExtendedViewFields.MAX_MISSED_CLEAVAGES,
        MsiSearchExtendedViewFields.FIXED_PTMS,
        MsiSearchExtendedViewFields.VARIABLE_PTMS,
        MsiSearchExtendedViewFields.PEPTIDE_CHARGE_STATES,
        MsiSearchExtendedViewFields.PEPTIDE_MASS_ERROR_TOLERANCE,
        MsiSearchExtendedViewFields.FRAGMENT_MASS_ERROR_TOLERANCE,
        MsiSearchExtendedViewFields.IS_DECOY
        // option C13 is missing
      ).map(_.toString)
    )
  )
  
  // Create an XLSX template specific to the datasets view
  private val datasetsXlsxTemplate = new BasicXLSXTemplate(
    selectedFields = Some(
      Seq(
        MsiSearchExtendedViewFields.PROJECT_NAME,
        MsiSearchExtendedViewFields.RESULT_SET_NAME,
        MsiSearchExtendedViewFields.JOB_NUMBER,
        MsiSearchExtendedViewFields.RESULT_FILE_DIRECTORY,
        MsiSearchExtendedViewFields.RESULT_FILE_NAME,        
        MsiSearchExtendedViewFields.RAW_FILE_NAME,
        MsiSearchExtendedViewFields.PEAKLIST_FILE_PATH,
        MsiSearchExtendedViewFields.SEARCH_TITLE,
        MsiSearchExtendedViewFields.SEARCH_DATE
      ).map(_.toString)
    )
  )
  
  // Create an XLSX template specific to the peptides view
  private val bestPepMatchesXlsxTemplate = new BasicXLSXTemplate(
    selectedFields = Some(
      Seq(
        ProtSetToPepMatchViewFields.PEPTIDE_ID,
        ProtSetToPepMatchViewFields.SEQUENCE,        
        ProtSetToPepMatchViewFields.MODIFICATIONS,
        ProtSetToPepMatchViewFields.MISSED_CLEAVAGES,
        ProtSetToPepMatchViewFields.RANK,
        ProtSetToPepMatchViewFields.CD_PRETTY_RANK,
        ProtSetToPepMatchViewFields.PEPMATCH_SCORE,
        ProtSetToPepMatchViewFields.CALCULATED_MASS,
        ProtSetToPepMatchViewFields.CHARGE,
        ProtSetToPepMatchViewFields.EXPERIMENTAL_MOZ,
        ProtSetToPepMatchViewFields.DELTA_MOZ,
        //ProtSetToPepMatchViewFields.RT, not yet available
        ProtSetToPepMatchViewFields.PEPTIDE_LENGTH,
        ProtSetToPepMatchViewFields.INITIAL_QUERY_ID,
        ProtSetToPepMatchViewFields.FRAGMENT_MATCHES_COUNT,
        ProtSetToPepMatchViewFields.SPECTRUM_TITLE,
        ProtSetToPepMatchViewFields.PROTEIN_SETS_COUNT,
        ProtSetToPepMatchViewFields.PROTEIN_MATCHES_COUNT,
        ProtSetToPepMatchViewFields.PROTEIN_SET_ID,
        ProtSetToPepMatchViewFields.ACCESSION,
        ProtSetToPepMatchViewFields.IS_PROTEIN_SET_VALIDATED,
        ProtSetToPepMatchViewFields.START,
        ProtSetToPepMatchViewFields.END,
        ProtSetToPepMatchViewFields.RESIDUE_BEFORE,
        ProtSetToPepMatchViewFields.RESIDUE_AFTER
      ).map(_.toString)
    )
  )
  
  val templatedViewTypes: Seq[ViewTypeWithTemplate] = Seq(
    ViewTypeWithTemplate( ResultSummaryViewTypes.MSI_SEARCH_EXTENDED, infoXlsxTemplate, viewName = Some("search settings") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.MSI_SEARCH_EXTENDED, datasetsXlsxTemplate, viewName = Some("datasets") ),
    // TODO: implement a new kind of view for this template
    ViewTypeWithTemplate( ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS, verticalXlsxTemplate, viewName = Some("filters") ),
    // TODO: implement a new kind of view for this template
    ViewTypeWithTemplate( ResultSummaryViewTypes.STATISTICS, verticalXlsxTemplate, viewName = Some("stats") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH, basicXlsxTemplate, viewName = Some("protein sets") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH, basicXlsxTemplate, viewName = Some("protein matches") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH, bestPepMatchesXlsxTemplate, viewName = Some("peptides") ),
    ViewTypeWithTemplate( ResultSummaryViewTypes.PROT_SET_TO_ALL_PEPTIDE_MATCHES, basicXlsxTemplate, viewName = Some("psm") )
  )
  
}