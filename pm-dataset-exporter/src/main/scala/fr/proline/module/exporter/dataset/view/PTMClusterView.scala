package fr.proline.module.exporter.dataset.view

import java.text.SimpleDateFormat

import fr.profi.util.collection._
import fr.proline.core.om.model.msi.ProteinSet
import fr.proline.core.om.model.msq.QuantPeptide
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.view.SmartDecimalFormat
import fr.proline.module.exporter.dataset.IdentDataset

import scala.collection.mutable

class PTMClusterView(
  val identDS: IdentDataset,
  val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: SmartDecimalFormat,
  val titleSep: String,
  val exportAllProteinSet: Boolean = true,
  val exportBestProfile: Boolean = false
) extends AbstractProtSetToMQPepView {

  var viewName = "ptm_cluster"

  protected val ptmViewFieldSet = Set(
    FIELD_PTM_CLUSTER_NB_PEPTIDES,
    FIELD_PTM_CLUSTER_NB_SITES,
    FIELD_PTM_CLUSTER_SITES_LOCALISATION,
    FIELD_PTM_CLUSTER_LOCALISATION_CONFIDENCE,
    FIELD_PTM_CLUSTER_IS_VALIDATED,
    FIELD_PTM_CLUSTER_VALIDATION_CONFIDENCE,
    FIELD_PTM_CLUSTER_VALIDATION_INFO

  )

  protected val ptmViewFieldsConfigs = sheetConfig.fields.filter( f => ptmViewFieldSet.contains(f.id) )

  // TODO: factorize this code with ProtSetToAllPepMatchesView in AbsractProtSetToPepMatchView (onEachPeptideMatch method)
  override def formatView(recordFormatter: Map[String, Any] => Unit) {

    val rsm = identDS.resultSummary
    val rs = rsm.lazyResultSet
    val protSetByRepresentativeProtMatchId = rsm.proteinSets.mapByLong(ps => ps.getRepresentativeProteinMatch().getOrElse(ps.samesetProteinMatches.get.head).id)

    val protMatchById = rs.proteinMatches.mapByLong(_.id)
    val ptmDatasetOpt = identDS.loadPtmDataset()

    if (ptmDatasetOpt.isDefined) {

      val pepMatchById = identDS.loadPepMatches(ptmDatasetOpt.get.ptmClusters.map(_.bestPeptideMatchId)).mapByLong(_.id)
      val allPepById = identDS.loadPeptides(ptmDatasetOpt.get.ptmClusters.flatMap(_.peptideIds)).mapByLong(_.id)

      val sitesById = ptmDatasetOpt.get.ptmSites.mapByLong(_.id)
      for (ptmCluster <- ptmDatasetOpt.get.ptmClusters) {
        val proteinMatchId = sitesById(ptmCluster.ptmSiteLocations.head).proteinMatchId
        if (protSetByRepresentativeProtMatchId.contains(proteinMatchId)) {
//          logger.info(s"exporting cluster ${ptmCluster.id} matched by ${ptmCluster.peptideIds}, best pepMatch = ${ptmCluster.bestPeptideMatchId}")
          val proteinMatch = protMatchById(proteinMatchId)
          val mqPeps = ptmCluster.peptideIds.map(peptideId => Option(quantDs).flatMap( _.mqPepByPepId.get(peptideId) ))
          val protSet = protSetByRepresentativeProtMatchId(proteinMatchId)
          val seqMatchByPepId = proteinMatch.sequenceMatches.toLongMapWith { seqMatch =>
            seqMatch.getPeptideId -> seqMatch
          }

          val protMatchBuildingCtx = new ProtMatchBuildingContext(
            protSet,
            protSet.peptideSet,
            protSet.getRepresentativeProteinMatch().getOrElse(protSet.samesetProteinMatches.get.head)
          )

          val pepMatch = pepMatchById(ptmCluster.bestPeptideMatchId)
          val allPeps = ptmCluster.peptideIds.map(id => allPepById(id))

          val recordBuildingCtx = { if (mqPeps.isEmpty || !mqPeps.head.isDefined) {
            new PTMClusterBuildingContext(
              ptmCluster,
              ptmCluster.ptmSiteLocations.map(sitesById(_)),
              pepMatch = pepMatch,
              allPeptides = allPeps,
              seqMatch = seqMatchByPepId(pepMatch.peptide.id),
              protMatchBuildingCtx = Some(protMatchBuildingCtx),
              ptmDatasetOpt.get.ptmIds
            )

          } else {

            val qPepsByQCId = mqPeps.filter(_.isDefined).map(_.get.quantPeptideMap).flatten.groupBy(_._1)
            var newQPeps = mutable.LongMap[QuantPeptide]()
            for (qcId <- qcIds) {
              if (qPepsByQCId.contains(qcId)) {
                val qPeps = qPepsByQCId(qcId).map(_._2)
                val quantPepIonWRawAb = qPeps.map(_.rawAbundance).filter(!_.equals(Float.NaN))
                val quantPepIonWAb = qPeps.map(_.abundance).filter(!_.equals(Float.NaN))
                val newRawAbundance = if (quantPepIonWRawAb.nonEmpty)quantPepIonWRawAb.sum else Float.NaN
                val newAbundance =  if (quantPepIonWAb.nonEmpty)quantPepIonWAb.sum else Float.NaN

                val aggregatedQPep = qPeps.head.copy(rawAbundance = newRawAbundance, abundance = newAbundance)
                newQPeps += (qcId -> aggregatedQPep)
              }
            }

            val mqPep = mqPeps.head.get.copy(quantPeptideMap = newQPeps)

            new QuantPTMClusterBuildingContext(
              ptmCluster = ptmCluster,
              ptmSites = ptmCluster.ptmSiteLocations.map(sitesById(_)),
              pepMatch = pepMatch,
              allPeptides = allPeps,
              seqMatch = seqMatchByPepId(pepMatch.peptide.id),
              protMatchBuildingCtx = Some(protMatchBuildingCtx),
              ptmDatasetOpt.get.ptmIds,
              mqPep,
              groupSetupNumber = groupSetupNumber
            )
          }}
          this.formatRecord(recordBuildingCtx, recordFormatter)
        }
      }
    } else {
      logger.warn("----> WARNING in PTMClusterView no PTM dataset defined ")
    }

  }

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {
    val pepMatchRecord = super.buildRecord(buildingContext)

    // If this building context is not a PTMClusterBuildingContext then we return the record as is
    if( !buildingContext.isInstanceOf[IPTMClusterBuildingContext]) {
      return pepMatchRecord
    }

    val ptmBuildingCtx = buildingContext.asInstanceOf[IPTMClusterBuildingContext]
    val ptmCluster = ptmBuildingCtx.ptmCluster
    val ptmSites = ptmBuildingCtx.ptmSites
    var siteCount = 1
    val ptmOfInterest = ptmBuildingCtx.ptmIds
    ptmBuildingCtx.allPeptides.foreach( peptide  =>  {
      var pepSitesCount = 0;
      pepSitesCount = peptide.ptms.filter(locPtm => ptmOfInterest.contains(locPtm.definition.ptmId)).length
      if(siteCount < pepSitesCount)
        siteCount = pepSitesCount
    })
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= pepMatchRecord
    val opSelectionLevel: Option[Integer] = Option(ptmCluster.selectionLevel)
    val isValidated  = if( opSelectionLevel.isEmpty || (opSelectionLevel.isDefined && opSelectionLevel.get>=2)) true else false


    for (fieldConfig <- ptmViewFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PTM_CLUSTER_NB_PEPTIDES => ptmCluster.peptideIds.length
        case FIELD_PTM_CLUSTER_NB_SITES => siteCount
        case FIELD_PTM_CLUSTER_SITES_LOCALISATION => ptmSites.map(_.seqPosition).mkString(";")
        case FIELD_PTM_CLUSTER_LOCALISATION_CONFIDENCE => ptmCluster.localizationConfidence
        case FIELD_PTM_CLUSTER_IS_VALIDATED => isValidated
        case FIELD_PTM_CLUSTER_VALIDATION_CONFIDENCE => ptmCluster.selectionConfidence
        case FIELD_PTM_CLUSTER_VALIDATION_INFO => ptmCluster.selectionInformation
      }
      if( fieldValue != null ) recordBuilder += fieldConfig.title -> fieldValue
    }

    recordBuilder.result()

  }

}