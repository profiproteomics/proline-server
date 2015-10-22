package fr.proline.module.exporter.msi.view

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.core.om.model.msi._

case class ProtMatchBuildingContext(
  protSet: ProteinSet,
  peptideSet: PeptideSet,
  protMatch: ProteinMatch
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

case class SpectrumBuildingContext(
  protSet: ProteinSet,
  peptideSet: PeptideSet,
  peptideMatch: PeptideMatch
) extends IRecordBuildingContext {
  
  var specID = 0L 
  if(peptideMatch.getMs2Query() != null){
    specID = peptideMatch.getMs2Query().spectrumId
  } else{
    
  }

}



case class PepMatchBuildingContext(
  pepMatch: PeptideMatch,
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
  protMatchBuildingCtx: Option[ProtMatchBuildingContext] = None
) extends IRecordBuildingContext
