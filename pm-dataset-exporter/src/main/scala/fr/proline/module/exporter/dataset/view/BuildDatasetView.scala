package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.ProteinMatch
import com.typesafe.scalalogging.LazyLogging
import fr.proline.module.exporter.api.view.IFixedDatasetView
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import java.text.SimpleDateFormat
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import scala.collection.immutable.ListMap
import fr.proline.core.om.model.msq.QuantResultSummary
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.model.msq.RatioDefinition
import fr.proline.module.exporter.commons.config.view.DatasetViewTypes
import fr.proline.core.om.model.msi.Protein
import com.typesafe.scalalogging.LazyLogging

class IdentDataSet(
  var projectName: String,
  var resultSummary: ResultSummary,
  var childsResultSummarys: Array[ResultSummary],
  var childsResultSets: Array[ResultSet], 
  var bioSequenceByBioSeqId: Map[Long, Double]) extends LazyLogging {

  // Count the number of protein sets and proteins matches related to a given peptide match
  val validProtSetIdSetByPepMatchId = new HashMap[Long, HashSet[Long]]()
  val validProtMatchIdSetByPepMatchId = new HashMap[Long, HashSet[Long]]()

  // Init Maps 
  resultSummary.proteinSets.filter(_.isValidated).foreach(protSet => {

    protSet.peptideSet.getPeptideMatchIds.foreach(validProtSetIdSetByPepMatchId.getOrElseUpdate(_, new HashSet[Long]) += protSet.id)

    val protMatchIdByPepSet = protSet.getAllProteinMatchesIdByPeptideSet
    protMatchIdByPepSet.foreach(entry => {
      val pepSet = entry._1
      for (
        prMId <- entry._2;
        pepMatchId <- pepSet.getPeptideMatchIds
      ) {
        validProtMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long]) += prMId
      }
    })
  })

  lazy val pepMatchById = resultSummary.resultSet.get.getPeptideMatchById

  //   Create list of all ProtMatches for pepMatches (validated or not) 
  lazy val allProtMatchSetByPepId = {
    val resultBuilder = new HashMap[Long, HashSet[ProteinMatch]]
    for (protMatch <- resultSummary.resultSet.get.proteinMatches) {
      if (protMatch.sequenceMatches != null) {
        protMatch.sequenceMatches.foreach(seqMatch => {
          val pepId = seqMatch.getPeptideId
          resultBuilder.getOrElseUpdate(seqMatch.getPeptideId, new HashSet[ProteinMatch]) += protMatch
        }) //end go throudh protMatch sequenceMatches 
      }
    } //End go through ProtMarches
    resultBuilder
  }
}

class QuantiDataSet(
  projectName: String,
  resultSummary: ResultSummary,
  childsResultSummarys: Array[ResultSummary],
  childsResultSets: Array[ResultSet],
  bioSequenceByBioSeqId: Map[Long, Double],
  var masterQuantChannelId: Long,
  var quantRSM: QuantResultSummary,
  var qcIds: Array[Long],
  var expDesign: ExperimentalDesign,
  var ratioDefs: Array[RatioDefinition],
  var nameByQchId: Map[Long, String],
  var protMatchStatusByIdPepMatchByQCId: Map[Long, Map[Long, String]],
  var protMatchPeptideNumberByPepMatchIdByQCId: Map[Long, Map[Long, Int]]) extends IdentDataSet(projectName, resultSummary, childsResultSummarys, childsResultSets, bioSequenceByBioSeqId)

object BuildDatasetView {

  private def _builders(exportConfig: ExportConfig): Map[IViewTypeEnumeration#Value, IdentDataSet => IDataView] = {
    var buildMap: ListMap[IViewTypeEnumeration#Value, IdentDataSet => IDataView] = ListMap()

    val dateFormat: SimpleDateFormat = new SimpleDateFormat(exportConfig.dateFormat)
    val decimalFormat: DecimalFormat = new DecimalFormat()
    var decSep: DecimalFormatSymbols = new DecimalFormatSymbols()
    decSep.setDecimalSeparator(exportConfig.decimalSeparator)
    decimalFormat.setDecimalFormatSymbols(decSep)
    decimalFormat.setGroupingUsed(false)
    val titleSep: String = if (exportConfig.titleSeparator == null)  ExportConfigConstant.SEPARATOR_INCREMENTAL_TITLE_UNDERSCORE else exportConfig.titleSeparator
    val exportAllProteinSet: Boolean = exportConfig.dataExport.allProteinSet
    val exportBestProfile: Boolean = exportConfig.dataExport.bestProfile
    for (sheet <- exportConfig.sheets) {
      sheet.id match {
        case ExportConfigConstant.SHEET_INFORMATION => {
          buildMap += (DatasetViewTypes.MSI_SEARCH_EXTENDED -> { ds: IdentDataSet => new MsiSearchExtendedView(ds, sheet, dateFormat, decimalFormat) })
        }
        case ExportConfigConstant.SHEET_IMPORT => {
          buildMap += (DatasetViewTypes.IMPORT_AND_VALIDATION_PROPS -> { ds: IdentDataSet => new ImportAndValidationPropsView(ds, sheet, dateFormat, decimalFormat, titleSep) })
        }
        case ExportConfigConstant.SHEET_PROTEIN_SETS => {
          buildMap += (DatasetViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToTypicalProtMatchView(ds, sheet, dateFormat, decimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_BEST_PSM => {
          buildMap += (DatasetViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: IdentDataSet => new ProtSetToBestPepMatchView(ds, sheet, dateFormat, decimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_PROTEIN_MATCH => {
          buildMap += (DatasetViewTypes.PROT_SET_TO_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToProtMatchView(ds, sheet, dateFormat, decimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_ALL_PSM => {
          buildMap += (DatasetViewTypes.PROT_SET_TO_ALL_PEPTIDE_MATCHES -> { ds: IdentDataSet => new ProtSetToAllPepMatchesView(ds, sheet, dateFormat, decimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_MASTER_QUANT_PEPTIDE_ION => {
          buildMap += (DatasetViewTypes.MASTER_QUANT_PEPTIDE_ION -> { ds: IdentDataSet => new MasterQuantPeptideIonView(ds, sheet, dateFormat, decimalFormat, titleSep, exportAllProteinSet, exportBestProfile) })
        }
        case ExportConfigConstant.SHEET_STAT => {
          buildMap += (DatasetViewTypes.STATISTICS -> { ds: IdentDataSet => new StatisticsView(ds.resultSummary, sheet, dateFormat, decimalFormat) })
        }
        case other => {

        }
      }
    }
    return buildMap
  }

  def apply(identDS: IdentDataSet, viewType: IViewTypeEnumeration#Value, exportConfig: ExportConfig): IDataView = {
    _builders(exportConfig)(viewType)(identDS)
  }

}