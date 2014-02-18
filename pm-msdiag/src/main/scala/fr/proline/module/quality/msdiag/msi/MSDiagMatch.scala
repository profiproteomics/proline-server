package fr.proline.module.quality.msdiag.msi

import fr.proline.core.om.model.msi.Protein
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Spectrum
import scala.math.{min, max}

// minScore == null && maxScore == null => look for unassigned spectra
// minScore == null && maxScore != null => lower bound (ie. 0 to 20)
// minScore != null && maxScore != null => inbetween bound (ie. 20 to 40)
// minScore != null && maxScore == null => higher bound (ie. above 40)
case class MSDiagMatch(
    val minScore: Float,
    val maxScore: Float,
    val targetMatches: Array[PeptideMatch],
    val decoyMatches: Array[PeptideMatch]
) extends MSDiagItem {
  
  def getLabel: String = {
    if(minScore == Float.NaN && maxScore == Float.NaN) "Unassigned spectra"
    else if(minScore.isNaN && !maxScore.isNaN) "Score <= "+maxScore
    else if(minScore.isNaN && !maxScore.isNaN) "Score <= "+maxScore
    else if(!minScore.isNaN && !maxScore.isNaN) minScore+" < score <= "+maxScore
    else "Score > "+minScore
  }

  def getNumberOfTargetMatches: Int = targetMatches.size
  def getNumberOfDecoyMatches: Int = decoyMatches.size
  def countItems: Int = getNumberOfTargetMatches + getNumberOfDecoyMatches

  def getLowestExperimentalMoz: Double = {
    val targetMinMoz = if(targetMatches.isEmpty) Double.NaN else targetMatches.minBy(_.getMs2Query.moz).getMs2Query.moz
    val decoyMinMoz = if(decoyMatches.isEmpty) Double.NaN else decoyMatches.minBy(_.getMs2Query.moz).getMs2Query.moz
    if(targetMinMoz.isNaN && decoyMinMoz.isNaN) Double.NaN
    else if(!targetMinMoz.isNaN && decoyMinMoz.isNaN) targetMinMoz
    else if(targetMinMoz.isNaN && !decoyMinMoz.isNaN) decoyMinMoz
    else min(targetMinMoz, decoyMinMoz)
//    min(targetMatches.minBy(_.getMs2Query.moz).getMs2Query.moz, decoyMatches.minBy(_.getMs2Query.moz).getMs2Query.moz)
  }
  def getHighestExperimentalMoz: Double = {
    val targetMaxMoz = if(targetMatches.isEmpty) Double.NaN else targetMatches.maxBy(_.getMs2Query.moz).getMs2Query.moz
    val decoyMaxMoz = if(decoyMatches.isEmpty) Double.NaN else decoyMatches.maxBy(_.getMs2Query.moz).getMs2Query.moz
    if(targetMaxMoz.isNaN && decoyMaxMoz.isNaN) Double.NaN
    else if(!targetMaxMoz.isNaN && decoyMaxMoz.isNaN) targetMaxMoz
    else if(targetMaxMoz.isNaN && !decoyMaxMoz.isNaN) decoyMaxMoz
    else max(targetMaxMoz, decoyMaxMoz)
  }
//  def getLowestExperimentalMoz: Double = min(targetMatches.minBy(_.getMs2Query.moz).getMs2Query.moz, decoyMatches.minBy(_.getMs2Query.moz).getMs2Query.moz)
//  def getHighestExperimentalMoz: Double = max(targetMatches.minBy(_.getMs2Query.moz).getMs2Query.moz, decoyMatches.minBy(_.getMs2Query.moz).getMs2Query.moz)
}

class MSDiagChargeMatch(val charge: Int,
    override val minScore: Float,
    override val maxScore: Float,
    override val targetMatches: Array[PeptideMatch],
    override val decoyMatches: Array[PeptideMatch]
) extends MSDiagMatch(minScore, maxScore, targetMatches, decoyMatches)

class MSDiagRTMatch(val rtmn: Int,
    override val minScore: Float,
    override val maxScore: Float,
    override val targetMatches: Array[PeptideMatch],
    override val decoyMatches: Array[PeptideMatch]
) extends MSDiagMatch(minScore, maxScore, targetMatches, decoyMatches)
