package fr.proline.module.exporter.msi.template

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template._
import fr.proline.module.exporter.commons.template.BasicXLSXTemplate
import fr.proline.module.exporter.commons.template.InfoXLSXTemplate
import fr.proline.module.exporter.msi.view._
import fr.proline.module.exporter.msi.view.{ ProtSetToPepMatchViewFields => PeptideViewFields }

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
        PeptideViewFields.PEPTIDE_ID,
        PeptideViewFields.SEQUENCE,        
        PeptideViewFields.MODIFICATIONS,
        PeptideViewFields.MISSED_CLEAVAGES,
        PeptideViewFields.RANK,
        PeptideViewFields.CD_PRETTY_RANK,
        PeptideViewFields.PEPMATCH_SCORE,
        PeptideViewFields.CALCULATED_MASS,
        PeptideViewFields.CHARGE,
        PeptideViewFields.EXPERIMENTAL_MOZ,
        PeptideViewFields.DELTA_MOZ,
        //PeptideViewFields.RT, not yet available
        PeptideViewFields.PEPTIDE_LENGTH,
        PeptideViewFields.INITIAL_QUERY_ID,
        PeptideViewFields.FRAGMENT_MATCHES_COUNT,
        PeptideViewFields.SPECTRUM_TITLE,
        PeptideViewFields.PROTEIN_SETS_COUNT,
        PeptideViewFields.PROTEIN_MATCHES_COUNT,
        PeptideViewFields.PROTEIN_SET_ID,
        PeptideViewFields.ACCESSION,
        PeptideViewFields.IS_PROTEIN_SET_VALIDATED,
        PeptideViewFields.START,
        PeptideViewFields.END,
        PeptideViewFields.RESIDUE_BEFORE,
        PeptideViewFields.RESIDUE_AFTER
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
    ViewTypeWithTemplate( ResultSummaryViewTypes.ALL_PEPTIDE_MATCHES, basicXlsxTemplate, viewName = Some("psm") )
  )
  
}