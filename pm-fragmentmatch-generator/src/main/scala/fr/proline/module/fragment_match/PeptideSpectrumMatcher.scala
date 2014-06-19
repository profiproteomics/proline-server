package fr.proline.module.fragment_match

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.StringBuilder
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.FragmentMatchType
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.profi.util.ms.MassTolUnit
import fr.profi.util.ms.calcMozTolInDalton
import fr.profi.util.MathUtils

case class Peak(moz: Double, intensity: Float)

class PeptideSpectrumMatcher(val spectraByIds: Map[Long, Spectrum],
  val ms2ErrorTol: Double,
  val ms2ErrorTolUnitStr: String) extends Logging {

  def getSpectrumMatch(peptideMatch: PeptideMatch): SpectrumMatch = {
    val ms2ErrorTolUnit = MassTolUnit.withName(ms2ErrorTolUnitStr)
    val LabelRegex = """([^\+]+)([\w\+]*)""".r      
    
    var start = System.currentTimeMillis()

    val spectrum = spectraByIds(peptideMatch.getMs2Query.spectrumId)
    val allPeaks = for ( (m,i) <- spectrum.mozList.get.zip(spectrum.intensityList.get)) yield new Peak(moz = m, intensity = i)
    val usedPeaksCount = {
      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.mascotProperties.isDefined) {
        peptideMatch.properties.get.mascotProperties.get.getUsedPeaksCount.getOrElse(allPeaks.length)
      } else { allPeaks.length }
    }

    var usedPeaks = mascotLikePeaksSelection(peptideMatch, allPeaks, usedPeaksCount)
    usedPeaks = usedPeaks.sortBy(_.moz)

    //TODO : configure ion series used
    val nlString = {
      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.mascotProperties.isDefined) {
        peptideMatch.properties.get.mascotProperties.get.getNlString.getOrElse("");
      } else { "" }
    }
    
    val aaSequence = {
      val seq = peptideMatch.peptide.sequence.toCharArray()
      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.mascotProperties.isDefined && peptideMatch.properties.get.mascotProperties.get.getAmbiguityString.isDefined) {
        val ambiguities = peptideMatch.properties.get.mascotProperties.get.getAmbiguityString.get.split(",");
        for (i <- 0 to (ambiguities.length-1) by 3) {
        	seq(ambiguities(i).toInt - 1) = ambiguities(i+2).charAt(0)
        } 
      } 
      seq
    }
    
    val ptmNeutralLosses = new HashMap[LocatedPtm, Double]()
    for (idx <- 0 until nlString.size) {
    	if(nlString.charAt(idx) != '0') {
    	  val ptm = peptideMatch.peptide.ptms.find(_.seqPosition == (idx))
    	  ptmNeutralLosses += (ptm.get -> ptm.get.definition.neutralLosses(nlString.charAt(idx).asDigit - 1).monoMass)
    	}
    }
    val charge = scala.math.min(peptideMatch.msQuery.charge, 2)
    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = charge, chargeForIonsY = charge)
    val theoreticalFragmentsTable = new FragmentIonTableV2(peptideMatch.peptide, currentFragmentIonTypes, Some(aaSequence), ptmNeutralLosses = if (ptmNeutralLosses.isEmpty) None else Some(ptmNeutralLosses.toMap))

    val theoFragments = theoreticalFragmentsTable.fragments.map(_._2).flatten
    val fragMatches = new ArrayBuffer[FragmentMatch]()

    for (fragment <- theoFragments) {

      val theoFragMoz = fragment.moz
      val mozTolInDa = calcMozTolInDalton(theoFragMoz, ms2ErrorTol, ms2ErrorTolUnit)
      var bestMatch: FragmentMatch = null

      breakable {
        for (ms2Peak <- usedPeaks) {

          val obsMoz = ms2Peak.moz
          val deltaMoz = obsMoz - theoFragMoz

          if (scala.math.abs(deltaMoz) <= mozTolInDa) {

            val LabelRegex(ionserie, chargeStr) = fragment.series.get
            val label = new StringBuilder().append(ionserie).append('(').append(fragment.position.toString).append(')').append(chargeStr)
            if (fragment.neutralLoss > 0.0) {
              label.append(" -"+scala.math.round(fragment.neutralLoss))
            }
            
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
  
  def mascotLikePeaksSelection(peptideMatch: PeptideMatch, allPeaks: Array[Peak], usedPeaksCount: Int): ArrayBuffer[Peak] = {
    
    var usedPeaks = new ArrayBuffer[Peak](usedPeaksCount + 2)
    val allPeaksSorted = allPeaks.sortBy(_.moz)

    if (usedPeaksCount != allPeaks.length) {

      var usedPeaks2 = new ArrayBuffer[Peak](usedPeaksCount)
      var allPeaksBinned = new ArrayBuffer[ArrayBuffer[Peak]]
      var refMz = allPeaksSorted.head.moz
      allPeaksBinned += new ArrayBuffer[Peak]
      for (i <- 0 until allPeaksSorted.length) {
        if (allPeaksSorted(i).moz < (refMz + 100)) {
          allPeaksBinned.last += allPeaksSorted(i)
        } else {
          allPeaksBinned(allPeaksBinned.size - 1) = allPeaksBinned.last.sortBy(p => (p.intensity, -p.moz)).reverse
          allPeaksBinned += new ArrayBuffer[Peak]
          allPeaksBinned.last += allPeaksSorted(i)
          do {
            refMz += 100
          } while ((refMz + 100) < allPeaksSorted(i).moz)
        }
      }
      // sort the last bin
      allPeaksBinned(allPeaksBinned.size -1) = allPeaksBinned.last.sortBy(p => (p.intensity, -p.moz)).reverse
      var rank = ArrayBuffer.fill[Int](allPeaksBinned.size)(0)
      while (usedPeaks2.length < usedPeaksCount) {
        for (i <- 0 to (allPeaksBinned.length -1)) {
          while (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz >= peptideMatch.peptide.calculatedMass) {
        	  rank(i) += 1            
          }
          if (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz < peptideMatch.peptide.calculatedMass) {
            usedPeaks2 += allPeaksBinned(i)(rank(i))
//            if ((rank(i) +1) < allPeaksBinned(i).length &&
//                allPeaksBinned(i)(rank(i)+1).moz < peptideMatch.peptide.calculatedMass &&
//                (allPeaksBinned(i)(rank(i)+1).intensity - allPeaksBinned(i)(rank(i)).intensity).abs < MathUtils.EPSILON_FLOAT) {
//              rank(i) += 1
//              usedPeaks2 += allPeaksBinned(i)(rank(i))
//            }
          }
          rank(i) += 1
        }
      }

      
      for (i <- 0 to (allPeaksBinned.length -1)) {
          while (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz >= peptideMatch.peptide.calculatedMass) {
        	  rank(i) += 1            
          }
          if (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz < peptideMatch.peptide.calculatedMass && (allPeaksBinned(i)(rank(i)-1).intensity - allPeaksBinned(i)(rank(i)).intensity).abs < MathUtils.EPSILON_FLOAT) {
              usedPeaks2 += allPeaksBinned(i)(rank(i))
          }
      }

//      usedPeaks += Peak(0.0, 0f)
      usedPeaks ++= usedPeaks2.sortBy(_.moz)
//      usedPeaks += Peak(Double.MaxValue, 0f)
//
//    
//    // Define some vars
//    var prevPeak: Peak = null
//    var j = 0
//
//    // Iterate over matching peaks
//    for (i <- 0 until usedPeaks.length) {
//      val peak = usedPeaks(i)
//      var intThresh = peak.intensity
//
//      // TODO: explain this computation
//      if ((i > 0) && (i < (usedPeaks.length - 1)) && (prevPeak.intensity < peak.intensity)) {
//        intThresh = prevPeak.intensity
//      }
//
//      while ((j < allPeaksSorted.length) && (allPeaksSorted(j).moz < peak.moz)) {
//        val newPeak = allPeaksSorted(j)
//        if (newPeak.intensity >= intThresh) {
//          usedPeaks += newPeak
//        }
//        j += 1
//      }
//      j += 1
//
//      prevPeak = peak
//    }
      
      } else {
      // Fill usedPeaks with firstSortedPeaks data  
//      usedPeaks += Peak(0.0, 0f)
      usedPeaks ++= allPeaksSorted
//      usedPeaks += Peak(Double.MaxValue, 0f)
    }
    //removes marker peaks at moz 0.0 and Double.MaxValue
//    usedPeaks.filter(p => (p.moz > 0.0 && p.moz < Double.MaxValue))
    usedPeaks
  }

}