package fr.proline.module.quality.msdiag.msi

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import scala.collection.mutable.ArrayBuffer

class MSDiagResultSetManager(val rsTarget: ResultSet, val rsDecoyOpt: Option[ResultSet]) extends Logging {
  def getResultSet(wantDecoy: Boolean): ResultSet = {
    if(wantDecoy) rsDecoyOpt.getOrElse(null) else rsTarget
  }
  def getPeptideMatches(wantDecoy: Boolean): Array[PeptideMatch] = {
    if (wantDecoy) decoyPeptideMatches else targetPeptideMatches
//    if (wantDecoy) {
//      if (rsDecoyOpt.isDefined) rsDecoyOpt.get.peptideMatches else null
//    } else rsTarget.peptideMatches
  }
  def getAllPeptideMatches: Array[PeptideMatch] = getPeptideMatches(false) ++ getPeptideMatches(true)
  
  // extract the target PeptideMatches with rank=1
  // if the corresponding spectrum has a match (rank1) in the decoy RS, and if the score of this match is higher
  // then do not include this peptide match
  // else include this peptide match
  private val targetPeptideMatches: Array[PeptideMatch] = {
    if (!rsDecoyOpt.isDefined) rsTarget.peptideMatches
    else {
      val targets = new ArrayBuffer[PeptideMatch]()
      rsTarget.peptideMatches.foreach(pm => if(pm.rank == 1 && isRankOne(pm, rsDecoyOpt.get.peptideMatches)) { targets += pm } )
      targets.toArray
    }
  }
  private val decoyPeptideMatches: Array[PeptideMatch] = {
    if (!rsDecoyOpt.isDefined) null
    else {
      val decoys = new ArrayBuffer[PeptideMatch]()
      rsDecoyOpt.get.peptideMatches.foreach(pm => if(pm.rank == 1 && isRankOne(pm, rsTarget.peptideMatches)) { decoys += pm } )
      decoys.toArray
    }
  }
  private def isRankOne(pm: PeptideMatch, peptideMatches: Array[PeptideMatch]): Boolean = {
    // what to do if the scores matches ?
    (peptideMatches.filter(p => p.rank == 1 && p.msQueryId == pm.msQueryId && p.score > pm.score).size == 0)
  }
  
  def getMsiSearch = rsTarget.msiSearch.getOrElse(throw new Exception("Retention times could not be retrieved"))
}