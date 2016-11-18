package fr.proline.module.exporter.dataset.view

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.ComputedRatio
import fr.proline.core.om.model.msq.MasterQuantPeptide
import fr.proline.core.om.model.msq.MasterQuantPeptideIon
import fr.proline.core.om.model.msq.MasterQuantProteinSet
import fr.proline.core.om.model.msq.MasterQuantProteinSetProfile
import fr.proline.core.om.model.msq.QuantComponent
import fr.proline.module.exporter.api.view.IRecordBuildingContext

class ProtMatchBuildingContext(
  var protSet: ProteinSet,
  var peptideSet: PeptideSet,
  var protMatch: ProteinMatch
) extends IRecordBuildingContext {

  val peptideCount = peptideSet.items.length
  val allSeqs = new ArrayBuffer[String](peptideCount)
  val specificSeqs = new ArrayBuffer[String](peptideCount)
  val specificPeps = new ArrayBuffer[Peptide](peptideCount)
  val specificPepMatchIds = new ArrayBuffer[Long](peptideCount)

  for (item <- protSet.peptideSet.items) {
    val pepInst = item.peptideInstance
    allSeqs += pepInst.peptide.sequence
    if (protSet.isValidated && pepInst.isValidProteinSetSpecific) {
      specificSeqs += pepInst.peptide.sequence
      specificPeps += pepInst.peptide
      specificPepMatchIds ++= pepInst.getPeptideMatchIds
    }
  }

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
    /*profile.map { p =>
      p.getQuantComponentMap(quantChannelIds)
    }.getOrElse( masterQuantProteinSet.getQuantComponentMap )*/
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
