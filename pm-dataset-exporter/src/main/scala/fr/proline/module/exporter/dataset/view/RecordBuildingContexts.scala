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
  
  private val allSeqs = new ArrayBuffer[String](peptidesCount)
  private val specificNonUniqSeqs = new ArrayBuffer[String](peptidesCount)

  private val specificPepInstances = new ArrayBuffer[PeptideInstance](peptidesCount)
  
  private var totalLeavesMatchCount = 0
  private var specificTotalLeavesMatchCount = 0

  for (item <- protSet.peptideSet.items) {
    val pepInst = item.peptideInstance
    allSeqs += pepInst.peptide.sequence
    
    totalLeavesMatchCount += pepInst.totalLeavesMatchCount
    
    if (protSet.isValidated && pepInst.isValidProteinSetSpecific) {
      specificNonUniqSeqs += pepInst.peptide.sequence
      specificPepInstances += pepInst
      specificTotalLeavesMatchCount += pepInst.totalLeavesMatchCount
    }
  }
  
  val sequencesCount = allSeqs.distinct.length
  val specificSequencesCount = specificNonUniqSeqs.distinct.length
  val specificPeptidesCount = specificPepInstances.length
  val peptideMatchesCount = totalLeavesMatchCount
  val specificPeptideMatchesCount = specificTotalLeavesMatchCount

}

class PepMatchBuildingContext(
  var pepMatch: PeptideMatch,
  var isInSubset: Boolean,
  var protMatch: ProteinMatch,
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
    if( profile.isEmpty ) masterQuantProteinSet.getQuantComponentMap
    else masterQuantProteinSet.getProfileQuantComponentMap(profile.get, quantChannelIds)
  }
  def getRatios(): Option[List[Option[ComputedRatio]]] = profile.map(_.getRatios())
}

class MasterQuantPeptideBuildingContext(
  pepMatch: PeptideMatch,
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  var masterQuantPeptide: MasterQuantPeptide,
  val groupSetupNumber: Int
) extends PepMatchBuildingContext(
  pepMatch,
  false, // isSubset
  protMatch,
  seqMatch,
  protMatchBuildingCtx
) with IMasterQuantEntityBuildingContext {
  
  def getQuantComponentMap(): LongMap[_ <: QuantComponent] = masterQuantPeptide.quantPeptideMap
  def getRatios(): Option[List[Option[ComputedRatio]]] = Some(masterQuantPeptide.getRatios(groupSetupNumber))
}

class MasterQuantPeptideIonBuildingContext(
  pepMatch: PeptideMatch,
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  masterQuantPeptide: MasterQuantPeptide,
  var masterQuantPeptideIon: MasterQuantPeptideIon,
  val childPepMatchById: Map[Long, PeptideMatch],
  groupSetupNumber: Int
) extends MasterQuantPeptideBuildingContext(
  pepMatch,
  protMatch,
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
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext],
  masterQuantPeptide: MasterQuantPeptide,
  masterQuantPeptideIon: MasterQuantPeptideIon,
  childPepMatchById : Map[Long,PeptideMatch],
  val masterQuantReporterIon: MasterQuantReporterIon,
  groupSetupNumber: Int
) extends MasterQuantPeptideIonBuildingContext(
  pepMatch,
  protMatch,
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
