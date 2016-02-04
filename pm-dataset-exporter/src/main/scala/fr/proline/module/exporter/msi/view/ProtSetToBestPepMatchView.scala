package fr.proline.module.exporter.msi.view

import java.io.File
import java.io.OutputStream
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.ITableLikeView
import fr.proline.module.exporter.api.view.IViewFieldEnumeration
import fr.proline.module.exporter.api.view.IRecordBuildingContext

class ProtSetToBestPepMatchView(val identDS: MsiIdentDataSet) extends AbstractPeptideMatchView {

  var viewName = "prot_set_to_best_pep_match"

  // def onEachRecord is inherited from AbstractProtSetToTypicalProtMatchView

  // buildRecord  is inherited from AbstractPeptideMatchView

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