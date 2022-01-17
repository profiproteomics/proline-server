package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq._
import fr.proline.module.exporter.api.view.IRecordBuildingContext

class ProtMatchBuildingContext(
  var protSet: ProteinSet,
  var peptideSet: PeptideSet,
  var protMatch: ProteinMatch
) extends IRecordBuildingContext {

  val peptidesCount = peptideSet.items.length
  val sequencesCount = peptideSet.sequencesCount

  private val specificNonUniqSeqs = new ArrayBuffer[String](peptidesCount)

  private val specificPepInstances = new ArrayBuffer[PeptideInstance](peptidesCount)
  
  private var totalLeavesMatchCount = 0
  private var specificTotalLeavesMatchCount = 0

  for (item <- peptideSet.items) {
    val pepInst = item.peptideInstance

    totalLeavesMatchCount += pepInst.totalLeavesMatchCount
    
    if (protSet.isValidated && pepInst.isValidProteinSetSpecific) {
      specificNonUniqSeqs += pepInst.peptide.sequence
      specificPepInstances += pepInst
      specificTotalLeavesMatchCount += pepInst.totalLeavesMatchCount
    }
  }

  val specificSequencesCount = specificNonUniqSeqs.distinct.length
  val specificPeptidesCount = specificPepInstances.length
  val peptideMatchesCount = totalLeavesMatchCount
  val specificPeptideMatchesCount = specificTotalLeavesMatchCount

}

class PepMatchBuildingContext(
  var pepMatch: PeptideMatch,
  var isInSubset: Boolean,
  var seqMatch: SequenceMatch,
  var protMatchBuildingCtx: Option[ProtMatchBuildingContext] = None
) extends IRecordBuildingContext

trait IMasterQuantEntityBuildingContext {
  def getQuantComponentMap(): LongMap[_ <: QuantComponent]
  def getRatios(): Option[List[Option[ComputedRatio]]]
}

class MasterQuantProteinSetProfileBuildingContext(
  protSet: ProteinSet,
  peptideSet: PeptideSet,
  protMatch: ProteinMatch,
  var masterQuantProteinSet: MasterQuantProteinSet,
  var profile: Option[MasterQuantProteinSetProfile],
  quantChannelIds: Seq[Long]
) extends ProtMatchBuildingContext(
  protSet,
  peptideSet,
  protMatch
) with IMasterQuantEntityBuildingContext {
  def getQuantComponentMap(): LongMap[_ <: QuantComponent] = {
    //#17395 confusion between raw abundance in GUI and export. always use inital raw_abundance
    masterQuantProteinSet.getQuantComponentMap
  }
  def getRatios(): Option[List[Option[ComputedRatio]]] = profile.map(_.getRatios())
}

class MasterQuantPeptideBuildingContext(
  pepMatch: PeptideMatch,
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  var masterQuantPeptide: MasterQuantPeptide,
  val groupSetupNumber: Int
) extends PepMatchBuildingContext(
  pepMatch,
  false,
  seqMatch,
  protMatchBuildingCtx
) with IMasterQuantEntityBuildingContext {
  
  def getQuantComponentMap(): LongMap[_ <: QuantComponent] = masterQuantPeptide.quantPeptideMap
  def getRatios(): Option[List[Option[ComputedRatio]]] = Some(masterQuantPeptide.getRatios(groupSetupNumber))
}

class MasterQuantPeptideIonBuildingContext(
  pepMatch: PeptideMatch,
    seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  masterQuantPeptide: MasterQuantPeptide,
  var masterQuantPeptideIon: MasterQuantPeptideIon,
  val childPepMatchById: Map[Long, PeptideMatch],
  groupSetupNumber: Int
) extends MasterQuantPeptideBuildingContext(
  pepMatch,
  seqMatch,
  protMatchBuildingCtx,
  masterQuantPeptide,
  groupSetupNumber
) {
  override def getQuantComponentMap(): LongMap[_ <: QuantComponent] = masterQuantPeptideIon.quantPeptideIonMap
  override def getRatios(): Option[List[Option[ComputedRatio]]] = None
}

class MasterQuantReporterIonBuildingContext(
  pepMatch: PeptideMatch,
    seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  masterQuantPeptide: MasterQuantPeptide,
  masterQuantPeptideIon: MasterQuantPeptideIon,
  childPepMatchById : Map[Long,PeptideMatch],
  val masterQuantReporterIon: MasterQuantReporterIon,
  groupSetupNumber: Int
) extends MasterQuantPeptideIonBuildingContext(
  pepMatch,
  seqMatch,
  protMatchBuildingCtx,
  masterQuantPeptide,
  masterQuantPeptideIon,
  childPepMatchById,
  groupSetupNumber
) {
  override def getQuantComponentMap(): LongMap[_ <: QuantComponent] = masterQuantReporterIon.quantReporterIonMap
  override def getRatios(): Option[List[Option[ComputedRatio]]] = None
}

trait IPTMClusterBuildingContext {
  def ptmCluster : PtmCluster
  def ptmSites: Array[PtmSite2]
  def ptmIds: Array[Long]
  def allPeptides: Array[Peptide]
}

class PTMClusterBuildingContext(
  val ptmCluster : PtmCluster,
  val ptmSites: Array[PtmSite2],
  pepMatch: PeptideMatch,
  val allPeptides: Array[Peptide],
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  val ptmIds: Array[Long]
 ) extends PepMatchBuildingContext(
    pepMatch,
    false,
    seqMatch,
    protMatchBuildingCtx
) with IPTMClusterBuildingContext {

}

class QuantPTMClusterBuildingContext(
  val ptmCluster: PtmCluster,
  val ptmSites: Array[PtmSite2],
  pepMatch: PeptideMatch,
  val allPeptides: Array[Peptide],
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  val ptmIds: Array[Long],
  masterQuantPeptide: MasterQuantPeptide,
  groupSetupNumber: Int
) extends  MasterQuantPeptideBuildingContext(
  pepMatch,
  seqMatch,
  protMatchBuildingCtx,
  masterQuantPeptide,
  groupSetupNumber
) with IPTMClusterBuildingContext {

}