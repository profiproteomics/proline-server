package fr.proline.module.exporter.dataset.view


import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.MasterQuantProteinSet


class ProtMatchBuildingContext(
  var protSet: ProteinSet,
  var peptideSet: PeptideSet,
  var protMatch: ProteinMatch
) extends IRecordBuildingContext {
  
  val peptideCount = peptideSet.items.length
  val allSeqs = new ArrayBuffer[String]( peptideCount )
  val specificSeqs = new ArrayBuffer[String]( peptideCount )
  val specificPeps = new ArrayBuffer[Peptide]( peptideCount )
  val specificPepMatchIds = new ArrayBuffer[Long]( peptideCount )
  
  for(item <- protSet.peptideSet.items ) {
    val pepInst = item.peptideInstance
    allSeqs += pepInst.peptide.sequence
    if( pepInst.isValidProteinSetSpecific) {
      specificSeqs += pepInst.peptide.sequence
      specificPeps += pepInst.peptide
      specificPepMatchIds ++= pepInst.getPeptideMatchIds
    }
  }

}

case class PepMatchBuildingContext(
  pepMatch: PeptideMatch,
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext] = None
) extends IRecordBuildingContext


class ProtMatchQuantiBuildingContext(
    protSet: ProteinSet,
    peptideSet: PeptideSet,
   protMatch: ProteinMatch,
   var masterQuantProteinSet: MasterQuantProteinSet,
   var qcIds: Array[Long], 
   var nameByQchId:  Map[Long,String]
   ) extends ProtMatchBuildingContext(protSet, peptideSet, protMatch) 
