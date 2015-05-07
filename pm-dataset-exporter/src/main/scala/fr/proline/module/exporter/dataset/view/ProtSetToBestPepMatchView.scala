package fr.proline.module.exporter.dataset.view



import java.text.DecimalFormat
import fr.proline.module.exporter.commons.config.view.SheetViewFieldsConfig
import java.text.SimpleDateFormat
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.api.view.IFixedDatasetView
import scala.collection.immutable.ListMap
import fr.proline.module.exporter.commons.config.ExportConfigField
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant


class ProtSetToBestPepMatchView ( val identDS: IdentDataSet, val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat, val titleSep: String, val exportAllProteinSet: Boolean, val exportBestProfile: Boolean  )  extends AbstractProtSetToTypicalProtMatchView {
   var viewName = "prot_set_to_best_pep_match"
     
    // def onEachRecord is inherited from AbstractProtSetToTypicalProtMatchView
     
     // buildRecord  is inherited from AbstractProtSetToTypicalProtMatchConfigView
     
     override def formatRecord(
    buildingContext: IRecordBuildingContext,
    recordFormatter: Map[String, Any] => Unit): Unit = {

    val myBuildingCtx = buildingContext.asInstanceOf[ProtMatchBuildingContext]
    val pepMatchById = identDS.pepMatchById
    val validPepMatchIdSet = myBuildingCtx.protSet.peptideSet.getPeptideInstances.map { _.getPeptideMatchIds }.flatten.toSet

    myBuildingCtx.protMatch.sequenceMatches.foreach { seqMatch =>

      val pepMatchOpt = pepMatchById.get(seqMatch.bestPeptideMatchId)

      if (pepMatchOpt.isDefined && validPepMatchIdSet.contains(pepMatchOpt.get.id)) {

        val pepMatchBCtxt = new PepMatchBuildingContext(pepMatch = pepMatchOpt.get, protMatch = myBuildingCtx.protMatch, seqMatch = seqMatch, protMatchBuildingCtx = Some(myBuildingCtx))
        // Build the protein & peptide match record
        val pepMatchRecord = this.buildRecord(pepMatchBCtxt)

        recordFormatter(pepMatchRecord)
      }
    }
  }
}