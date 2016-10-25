package fr.proline.module.exporter.dataset.view

import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.text.SimpleDateFormat

import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.commons.config.view.DatasetViewType._
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset._

object BuildDatasetView {

  private def _builders(exportConfig: ExportConfig): Map[IViewTypeEnumeration#Value, IdentDataset => IDataView] = {
    
    val mapBuilder = Map.newBuilder[IViewTypeEnumeration#Value, IdentDataset => IDataView]

    val dateFormat = new SimpleDateFormat(exportConfig.dateFormat)
    val decimalFormat = new DecimalFormat()
    val decSep = new DecimalFormatSymbols()
    decSep.setDecimalSeparator(exportConfig.decimalSeparator)
    decimalFormat.setDecimalFormatSymbols(decSep)
    decimalFormat.setGroupingUsed(false)
    
    val smartDecimalFormat = new SmartDecimalFormat( decimalFormat )
    
    val titleSep = if (exportConfig.titleSeparator == null) ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE else exportConfig.titleSeparator
    val exportAllProteinSet = exportConfig.dataExport.allProteinSet
    val exportBestProfile = exportConfig.dataExport.bestProfile
    
    for (sheet <- exportConfig.sheets) {
      
      sheet.id match {
        case ExportConfigConstant.SHEET_INFORMATION => {
          mapBuilder += (MSI_SEARCH_EXTENDED -> { ds: IdentDataset => new MsiSearchExtendedView(ds, sheet, dateFormat, smartDecimalFormat) })
        }
        case ExportConfigConstant.SHEET_IMPORT => {
          mapBuilder += (IMPORT_AND_VALIDATION_PROPS -> { ds: IdentDataset => new ImportAndValidationPropsView(ds, sheet, dateFormat, smartDecimalFormat, titleSep) })
        }
        case ExportConfigConstant.SHEET_QUANT_CONFIG => {
          mapBuilder += (QUANT_CONFIG -> { ds: IdentDataset => new QuantConfigView(ds.asInstanceOf[QuantDataset], dateFormat, decimalFormat) })
        }
        case ExportConfigConstant.SHEET_PROTEIN_SETS => {
          mapBuilder += (PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: IdentDataset => new ProtSetToTypicalProtMatchView(ds, sheet, dateFormat, smartDecimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_BEST_PSM => {
          mapBuilder += (PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: IdentDataset => new ProtSetToBestPepMatchView(ds, sheet, dateFormat, smartDecimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_PROTEIN_MATCH => {
          mapBuilder += (PROT_SET_TO_PROT_MATCH -> { ds: IdentDataset => new ProtSetToProtMatchView(ds, sheet, dateFormat, smartDecimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_ALL_PSM => {
          mapBuilder += (PROT_SET_TO_ALL_PEPTIDE_MATCHES -> { ds: IdentDataset => new ProtSetToAllPepMatchesView(ds, sheet, dateFormat, smartDecimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_MASTER_QUANT_PEPTIDE_ION => {
          mapBuilder += (MASTER_QUANT_PEPTIDE_ION -> { ds: IdentDataset => new MasterQuantPeptideIonView(ds, sheet, dateFormat, smartDecimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_STAT => {
          mapBuilder += (STATISTICS -> { ds: IdentDataset => new StatisticsView(ds.resultSummary, sheet, dateFormat, decimalFormat) })
        }
        case _ => throw new Exception(s"Invalid sheet: ${sheet.id}")
      }
    }
    
    mapBuilder.result
  }

  def apply(identDS: IdentDataset, viewType: IViewTypeEnumeration#Value, exportConfig: ExportConfig): IDataView = {
    val builViewFn = _builders(exportConfig)(viewType)
    builViewFn(identDS)
  }
  
  //implicit def abstractDs2IdentDs( abstractDs: AbstractDataset ): IdentDataset = abstractDs.asInstanceOf[IdentDataset]
  //implicit def abstractDs2QuantDs( abstractDs: AbstractDataset ): QuantDataset = abstractDs.asInstanceOf[QuantDataset]

}