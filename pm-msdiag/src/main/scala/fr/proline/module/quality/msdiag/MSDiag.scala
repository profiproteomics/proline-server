package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.module.quality.msdiag.msi._
import fr.proline.core.om.model.msi.SpectrumTitleParsingRule

/**
 * @author Alexandre Burel (LSMBO IPHC CNRS)
 * MSDiag is an software designed to run a diagnosis of a MS/MS search
 * It has been originally developped for MSDA (https://msda.unistra.fr) and was intended to generate an Excel report from a Mascot search
 * A standard MSDiag report is an Excel file that contains 3 sheets
 * - A spectrum report, with the main result for each spectra (first rank peptide, first protein given).
 *   The unassigned spectra are kept, with less information
 *   The redundant matches (same peptide) are highlighted
 * - A table listing the number of matches per charge and score window (ie. 20 to 40), or unassigned
 *   Values such as moz and intensity are given per charge state (min, max)
 *   The number of decoy matches per score window is given
 *   The total number of redundant matches is given
 * - A table listing the number of matches per retention time and score window (or unassigned)
 *   The matches are grouped by minute (ie. number of unassigned from minute 2.0 to minute 2.9)
 *   This table can be directly transformed into a chromatogram with Excel graphs
 *
 * This Proline module should be able to do the same thing, but from a Proline ResultSet
 * This module extracts all the information needed to generate a MSDiag report and gives functions to get a similar output
 * This module is meant to be called by a GUI that will create the report
 *
 * Add a comparison function between 2 or more msdiags ?
 *
 */
//object MSDiag extends Logging {
//  def getPeptideMatches(rs: ResultSet, wantDecoy: Boolean): Array[PeptideMatch] = {
//    if(wantDecoy) {
//      if(rs.decoyResultSet.isDefined) rs.decoyResultSet.get.peptideMatches
//      else null
//    } else rs.peptideMatches
//  }
////  def getAllPeptideMatches(rs: ResultSet): Array[PeptideMatch] = getPeptideMatches(rs, false) ++ getPeptideMatches(rs, true)
//  def getAllPeptideMatches(rs: ResultSet): Array[PeptideMatch] = {
//    val t = getPeptideMatches(rs, false)
//    logger.debug("Target peptide matches count : "+t.size)
//    val d = getPeptideMatches(rs, true)
//    if(d != null) {
//	    logger.debug("Decoy peptide matches count : "+d.size)
//	    t ++ d
//    } else t
//  }
//}
//
//class MSDiag(val rs: ResultSet) extends Logging {
//
//  // Main MSDiag results
//  def getMSDiagByCharge(scores: Array[Float]): Array[Array[MSDiagChargeMatch]] = GroupByCharge.sortByScoreAndCharge(rs, scores)
//
//  // Sort by retention time (used for generating histogram)
//  def getMSDiagByRetentionTimes(scores: Array[Float]): Array[Array[MSDiagRTMatch]] = GroupByRT.sortByScoreAndRT(rs, scores)
//
//  // Redundant matches
//  lazy val getTargetRedundantMatches: Array[PeptideMatch] = RedundancyExtractor.getRedundantMatches(MSDiag.getPeptideMatches(rs, false))
//  lazy val getDecoyRedundantMatches: Array[PeptideMatch] = RedundancyExtractor.getRedundantMatches(MSDiag.getPeptideMatches(rs, true))
//  def countAllRedundantMatches: Int = getTargetRedundantMatches.size + getDecoyRedundantMatches.size
//
//}

class MSDiag(val rsTarget: ResultSet, val rsDecoyOpt: Option[ResultSet]) extends Logging {

  val rs = new MSDiagResultSetManager(rsTarget, rsDecoyOpt)

  // Main MSDiag results
  def getMSDiagByCharge(scores: Array[Float]): Array[Array[MSDiagChargeMatch]] = GroupByCharge.sortByScoreAndCharge(rs, scores)
  def getCharges: Array[Int] = GroupByCharge.getCharges(rs)

  // Sort by retention time (used for generating histogram)
  def getMSDiagByRetentionTimes(scores: Array[Float], parsingRules: Option[SpectrumTitleParsingRule]): Array[Array[MSDiagRTMatch]] = GroupByRT.sortByScoreAndRT(rs, scores, parsingRules)

  // Redundant matches
  lazy val getTargetRedundantMatches: Array[PeptideMatch] = RedundancyExtractor.getRedundantMatches(rs.getPeptideMatches(false))
  lazy val getDecoyRedundantMatches: Array[PeptideMatch] = RedundancyExtractor.getRedundantMatches(rs.getPeptideMatches(true))
  def countAllRedundantMatches: Int = getTargetRedundantMatches.size + getDecoyRedundantMatches.size

}
