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
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.MasterQuantPeptide

class ProtSetToAllPepMatchesView( val identDS: IdentDataSet, val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat, val titleSep: String, val exportAllProteinSet: Boolean , val exportBestProfile: Boolean )  extends AbstractProtSetToTypicalProtMatchView {
  override var viewName = "all_prot_set_peptide_matches"

  override def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    val pepMatchById = rs.getPeptideMatchById

    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]

    // Iterate over RSM protein sets
    for (protSet <- rsm.proteinSets) {
       if (exportAllProteinSet || protSet.isValidated){ // filter on validated proteinSet
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used

      // Typical Protein Match is put first
      val typicalProtMatchId = protSet.getRepresentativeProteinMatchId

      val typicalProtMatch = if (typicalProtMatchId != 0) {
        protMatchById(typicalProtMatchId)
      } else {
        protMatchById(protSet.getSameSetProteinMatchIds.head)
      }

      val seqMatchByPepId: Map[Long, SequenceMatch] = typicalProtMatch.sequenceMatches.map { seqMatch => (seqMatch.getPeptideId -> seqMatch) }toMap

      val protMatchBuildingCtx = new ProtMatchBuildingContext(
        protSet,
        protSet.peptideSet,
        typicalProtMatch
      )

       
      protSet.peptideSet.getPeptideInstances.foreach(pepI => {
        val allPepMatchIds = pepI.getPeptideMatchIds
        allPepMatchIds.foreach(pepMatchId => {
          var buildingContext = new PepMatchBuildingContext(
            pepMatch = pepMatchById(pepMatchId),
            protMatch = typicalProtMatch,
            seqMatch = seqMatchByPepId(pepMatchById(pepMatchId).peptideId),
            protMatchBuildingCtx = Some(protMatchBuildingCtx)
          )
          if (isQuanti){
            var masterQuantPeptide: MasterQuantPeptide = null
            for (mqPepSet <- quantiDS.quantRSM.masterQuantPeptides) {
              var pepId:  Long = -1
              if( mqPepSet.peptideInstance.isDefined) {
            	  	pepId = mqPepSet.peptideInstance.get.peptide.id
              }
              if (pepId == pepMatchById(pepMatchId).peptide.id) {
            	  masterQuantPeptide = mqPepSet
              }
            }
              buildingContext = new PepMatchQuantiBuildingContext(
                pepMatch = pepMatchById(pepMatchId),
                protMatch = typicalProtMatch,
                seqMatch = seqMatchByPepId(pepMatchById(pepMatchId).peptideId),
                protMatchBuildingCtx = Some(protMatchBuildingCtx), 
                masterQuantPeptide
                )
          }
          // Format this peptide match with protein set information
          this.formatRecord(buildingContext, recordFormatter)

        })
      })
    }
    }

  }
}