package fr.proline.module.fragmentmatch

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.ms.MassTolUnit
import fr.profi.util.ms.calcMozTolInDalton
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.FragmentMatchType
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch

case class Peak(moz: Double, intensity: Float)

trait PeptideSpectrumMatcher extends LazyLogging {

  val spectraByIds: Map[Long, Spectrum]
  val ms2ErrorTol: Double
  val ms2ErrorTolUnitStr: String
  
  def getUsedPeaks(peptideMatch: PeptideMatch): Array[Peak]
  def getPtmNeutralLosses(peptideMatch: PeptideMatch): Map[LocatedPtm, Double]
  def getSequence(peptideMatch: PeptideMatch): Array[Char]
  def getFragmentIonTypes(peptideMatch: PeptideMatch, charge: Int): FragmentIons
  
  def getSpectrumMatch(peptideMatch: PeptideMatch): SpectrumMatch = {
    val LabelRegex = """([^\+]+)([\w\+]*)""".r      
    
    var start = System.currentTimeMillis()

    val usedPeaks = getUsedPeaks(peptideMatch)
    val ptmNeutralLosses = getPtmNeutralLosses(peptideMatch)
    val aaSequence = getSequence(peptideMatch)
    
    val charge = scala.math.min(peptideMatch.msQuery.charge, 2)
    val currentFragmentIonTypes = this.getFragmentIonTypes(peptideMatch, charge)
    
    val theoreticalFragmentsTable = new FragmentIonTable(peptideMatch.peptide, currentFragmentIonTypes, Some(aaSequence), ptmNeutralLosses = if (ptmNeutralLosses.isEmpty) None else Some(ptmNeutralLosses.toMap))

    val theoFragments = theoreticalFragmentsTable.fragments.map(_._2).flatten
    val fragMatches = new ArrayBuffer[FragmentMatch]()

    for (fragment <- theoFragments) {

      val theoFragMoz = fragment.moz
      val mozTolInDa = calcMozTolInDalton(theoFragMoz, ms2ErrorTol, ms2ErrorTolUnitStr)
      var bestMatch: FragmentMatch = null

      breakable {
        for (ms2Peak <- usedPeaks) {

          val obsMoz = ms2Peak.moz
          val deltaMoz = obsMoz - theoFragMoz

          if (scala.math.abs(deltaMoz) <= mozTolInDa) {

            val LabelRegex(ionserie, chargeStr) = fragment.series.get
            val label = new StringBuilder().append(ionserie).append('(').append(fragment.position.toString).append(')').append(chargeStr)
            
            val fragMatch = new FragmentMatch(
              label = label.toString,
              `type` = fragment.fragmentType.collect { case x: FragmentMatchType.Value => x.toString },
              moz = obsMoz,
              calculatedMoz = theoFragMoz,
              intensity = ms2Peak.intensity)

            val nl = fragment.neutralLoss
            if (nl > 0.0) fragMatch.neutralLossMass = Some(nl)

            if ((bestMatch == null) || (bestMatch.intensity < fragMatch.intensity)) {
              bestMatch = fragMatch
            }

          } else if (scala.math.signum(deltaMoz) > 0) break
        }
      }

      if (bestMatch != null) fragMatches += bestMatch
    }


    new SpectrumMatch(peptideMatch.getMs2Query.initialId, peptideMatch.rank, theoreticalFragmentsTable.get, fragMatches.toArray)

  }

}