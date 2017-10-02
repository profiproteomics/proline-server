package fr.proline.module.exporter.dataset.view

import java.text.{DecimalFormat, SimpleDateFormat}

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view.IFormLikeView
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.CustomViewFields

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

class StatisticsView(
  val rsm: LazyResultSummary,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: DecimalFormat
) extends IFormLikeView {

  var viewName = "stats"
  val fieldsTitles = sheetConfig.fields.map(_.title)
  val fields = new CustomViewFields(fieldsTitles)

  def getFieldValueMap() = _fieldValueMap
  def getFieldsNames() = fieldsTitles

  private val _fieldValueMap = {
    val rsmValResultsOpt = rsm.descriptor.properties.flatMap(_.getValidationProperties.map(_.getResults))
    val psmValResultsAsStr = _stringifyValResults(rsmValResultsOpt.flatMap(_.getPeptideResults))
    val protValResultsAsStr = _stringifyValResults(rsmValResultsOpt.flatMap(_.getProteinResults))

    // TODO: get validated ResultSet
    val rs = rsm.lazyResultSet
    val pepMatches = rs.peptideMatches
    //VD Workaround for #17405
    val nullMsQueryExist = (pepMatches.filter(_.msQuery==null).length >0)
    val allPrecursorCharges: List[Int] = if(!nullMsQueryExist) { pepMatches
      .groupBy(pm => pm.peptide.uniqueKey + '%' + pm.msQuery.charge)
      .map(_._2.head.msQuery.charge)
      .toList } else List.empty[Int]

    val allZCount = allPrecursorCharges.length
    val z2Count = allPrecursorCharges.count(_ == 2)
    val z3Count = allPrecursorCharges.count(_ == 3)
    val z4PlusCount = allZCount - (z2Count + z3Count)
    val distinctSeqCount = rs.peptides.map(_.sequence).distinct.length
    val pepCount = rs.peptides.length
    val modPepCount = rs.peptides.count(_.ptmString.isEmpty == false)
    val unmodPepCount = pepCount - modPepCount
    val protSetCount = rsm.proteinSets.length
    val validatedProtSets = rsm.proteinSets.filter(_.isValidated)
    val validatedProtSetCount = validatedProtSets.length
    val( singlePepProtSets, multiPepProtSets ) = validatedProtSets.partition(_.peptideSet.items.length == 1 )

    // --- Group specific peptide sequences by protein sets ---
    val speSeqsByProtSet = new mutable.LongMap[ArrayBuffer[String]](rsm.proteinSets.length)

    for (
      protSet <- validatedProtSets;
      itemsCount = protSet.peptideSet.items.length;
      item <- protSet.peptideSet.items
    ) {
      val pepInst = item.peptideInstance

      if (pepInst.isProteinSetSpecific()) {
        speSeqsByProtSet.getOrElseUpdate(protSet.id, new ArrayBuffer[String](itemsCount)) += pepInst.peptide.sequence
      }
    }

    // --- Count the number of protein sets having a single or multiple specific peptide sequences ---
    val specificSeqsCounts = for ((protSet, speSeqs) <- speSeqsByProtSet) yield speSeqs.distinct.length
    val singleSpeSeqProtSetCount = specificSeqsCounts.count(_ == 1)
    val multiSpeSeqsProtSetCount = specificSeqsCounts.size - singleSpeSeqProtSetCount

    val recordBuilder = Map.newBuilder[String,Any]

    for (fieldConfig <- sheetConfig.fields) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_STAT_PSM_VALIDATION => psmValResultsAsStr
        case FIELD_STAT_NB_TOTAL_PSMS => pepMatches.length
        case FIELD_STAT_NB_TOTAL_PRECURSORS => if(nullMsQueryExist) "NA" else allZCount
        case FIELD_STAT_NB_Z2_PRECURSORS => if(nullMsQueryExist) "NA" else z2Count
        case FIELD_STAT_NB_Z3_PRECURSORS => if(nullMsQueryExist) "NA" else z3Count
        case FIELD_STAT_NB_Z4PLUS_PRECURSORS => if(nullMsQueryExist) "NA" else z4PlusCount
        case FIELD_STAT_NB_PEPTIDES => pepCount
        case FIELD_STAT_NB_MODIFIED_PEPTIDES => modPepCount
        case FIELD_STAT_NB_UNMODIFIED_PEPTIDES => unmodPepCount
        case FIELD_STAT_NB_DISTINCT_SEQS => distinctSeqCount
        case FIELD_STAT_PROT_VALIDATION => protValResultsAsStr
        case FIELD_STAT_NB_PROTEIN_SETS => protSetCount
        case FIELD_STAT_NB_VALIDATED_PROT_SETS => validatedProtSetCount
        case FIELD_STAT_NB_VALIDATED_PROT_SETS_SINGLE_PEPTIDE => singlePepProtSets.length
        case FIELD_STAT_NB_VALIDATED_PROT_SETS_MULTI_PEPTIDES => multiPepProtSets.length
        case FIELD_STAT_NB_VALIDATED_PROT_SETS_SINGLE_SPE_PEP_SEQUENCE => singleSpeSeqProtSetCount
        case FIELD_STAT_NB_VALIDATED_PROT_SETS_MULTI_SPE_PEP_SEQUENCES => multiSpeSeqsProtSetCount
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }

    recordBuilder.result()
  }

  private def _stringifyValResults(valResultsOpt: Option[RsmValidationResultProperties]): String = {
    if (valResultsOpt.isEmpty) ""
    else {
      val valResults = valResultsOpt.get
      val resultsBuffer = new ArrayBuffer[String](3)

      resultsBuffer += "TARGET matches count: " + valResults.getTargetMatchesCount

      if (valResults.getDecoyMatchesCount.isDefined)
        resultsBuffer += "DECOY matches count: " + valResults.getDecoyMatchesCount.get

      if (valResults.getFdr.isDefined)
        resultsBuffer += "FDR: " + valResults.getFdr.get

      resultsBuffer.mkString("; ")
    }
  }
}