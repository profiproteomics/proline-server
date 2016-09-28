package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat
import java.util.NoSuchElementException

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.commons.config._
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset._

class ProtSetToTypicalProtMatchView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean,
  val exportBestProfile: Boolean
) extends AbstractProtSetToMQProtSetProfileView {
  
  var viewName = "prot_set_to_typical_prot_match"

  def onEachRecord(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet
    val protMatchById = rs.proteinMatchById

    // Go through protein sets
    for (protSet <- rsm.proteinSets.sortBy( - _.peptideSet.score ) ) {
      if (exportAllProteinSet || protSet.isValidated) { // filter on validated proteinSet
        // Note that we export only protein matches which are loaded with the RSM
        // The result will depend of provider which have been used

        // Typical Protein Match is put first
        val typicalProtMatchId = protSet.getRepresentativeProteinMatchId

        val typicalProtMatch = if (typicalProtMatchId != 0) {
          try {
            protMatchById(typicalProtMatchId)
          } catch {
            case e: NoSuchElementException => {
              // "old"SC, proteinSetSC.typicalProteinMatchId refers to identRS.proteinMatch, instead of quantiRS.proteinMatch cf Issue #12421  
              logger.error("Exception while retrieving typicalProteinMatchId on a Spectral Count. Spectral Count must be relaunched before export.")
              throw new Exception("Exception while retrieving typicalProteinMatchId on a Spectral Count. Spectral Count must be relaunched to be exported.")
            }
          }
        } else {
          protMatchById(protSet.getSameSetProteinMatchIds.head)
        }

        // If ident DS
        if (isQuantDs == false) {
          val protMatchBuildingContext = new ProtMatchBuildingContext(
            protSet,
            protSet.peptideSet,
            typicalProtMatch
          )
          this.formatRecord(protMatchBuildingContext, recordFormatter)
        }
        // Else if quant DS and this protein set has been quantified
        else if (quantDs.mqProtSetByProtSetId.contains(protSet.id) ) {
          
          val mqProtSet = quantDs.mqProtSetByProtSetId(protSet.id)
          
          // Export best profile
          if (exportBestProfile) {
            // Note: bestProfile is None for SC
            val bestProfile = mqProtSet.getBestProfile(groupSetupNumber)
            
            val buildingContext = new MasterQuantProteinSetProfileBuildingContext(
              protSet,
              protSet.peptideSet,
              typicalProtMatch,
              mqProtSet,
              bestProfile,
              quantChannelIds = qcIds
            )
            
            this.formatRecord(buildingContext, recordFormatter)
            
          } else {
            
            // Export all profiles
            for (
              props <- mqProtSet.properties;
              profileByGSNum <- props.getMqProtSetProfilesByGroupSetupNumber;
              profiles <- profileByGSNum.get(groupSetupNumber);
              profile <- profiles
            ) {
              
              val ctx = new MasterQuantProteinSetProfileBuildingContext(
                protSet,
                protSet.peptideSet,
                typicalProtMatch,
                mqProtSet,
                Some(profile),
                quantChannelIds = qcIds
              )
              
              this.formatRecord(ctx, recordFormatter)
            }
          }
        }
      }
    }
  }

}
