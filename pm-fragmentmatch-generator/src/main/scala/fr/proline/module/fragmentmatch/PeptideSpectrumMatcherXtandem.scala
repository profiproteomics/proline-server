package fr.proline.module.fragmentmatch

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import fr.profi.util.MathUtils
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.InstrumentConfig

class PeptideSpectrumMatcherXtandem(
  val spectraByIds: Map[Long, Spectrum],
  val ms2ErrorTol: Double,
  val ms2ErrorTolUnitStr: String,
  val instrumentConfig: InstrumentConfig) extends PeptideSpectrumMatcher {

  logger.debug("Generation of spectrum match(es) for Xtandem data")
  if(!instrumentConfig.fragmentationRules.isDefined || instrumentConfig.fragmentationRules.get.isEmpty) {
    logger.warn("No fragmentation rules found for instrument '"+instrumentConfig.name+"', only 'b' and 'y' ion series will be considered")
  }
  
  def getUsedPeaks(peptideMatch: PeptideMatch): Array[Peak] = {
    val spectrum = spectraByIds(peptideMatch.getMs2Query.spectrumId)
    val allPeaks = for ( (m,i) <- spectrum.mozList.get.zip(spectrum.intensityList.get)) yield new Peak(moz = m, intensity = i)
    allPeaks.sortBy(_.moz)
//    val spectrum = spectraByIds(peptideMatch.getMs2Query.spectrumId)
//    val allPeaks = for ( (m,i) <- spectrum.mozList.get.zip(spectrum.intensityList.get)) yield new Peak(moz = m, intensity = i)
//    val usedPeaksCount = {
//      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.xtandemProperties.isDefined) {
//        peptideMatch.properties.get.xtandemProperties.get.getIonSeriesMatches.values.sum
////        peptideMatch.properties.get.xtandemProperties.get.getUsedPeaksCount.getOrElse(allPeaks.length)
//      } else { allPeaks.length }
//    }
//    var usedPeaks = xtandemLikePeaksSelection(peptideMatch, allPeaks, usedPeaksCount)
//    usedPeaks.sortBy(_.moz)
  }
  
  def getPtmNeutralLosses(peptideMatch: PeptideMatch): Map[LocatedPtm, Double] = {
    if(peptideMatch.peptide.ptms.exists(_.definition.neutralLosses.size > 1)) {
      peptideMatch.peptide.ptms.filter(_.definition.neutralLosses.size > 1).map(ptm => ptm -> ptm.definition.neutralLosses(1).monoMass).toMap
    } else {
      Map.empty
    }
  }
  
  def getSequence(peptideMatch: PeptideMatch): Array[Char] = {
    peptideMatch.peptide.sequence.toCharArray()
  }

  def getFragmentIonTypes(peptideMatch: PeptideMatch, charge: Int): FragmentIons = {
    if(instrumentConfig.fragmentationRules.isDefined && !instrumentConfig.fragmentationRules.get.isEmpty) {
      val currentFragmentIonTypes = new FragmentIons()
	  instrumentConfig.fragmentationRules.get.foreach(fr => currentFragmentIonTypes.setIonTypeAndCharge(xtandemFragmentationSeries(fr.description), charge))
	  currentFragmentIonTypes
    } else {
      new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = charge, chargeForIonsY = charge)
    }
  }
  
  private def xtandemFragmentationSeries(serie: String): String = {
    // all possible series : a ; a-NH3 ; a-H2O ; b ; b-NH3 ; b-H2O ; c ; d ; v ; w ; x ; y ; y-NH3 ; y-H2O ; z ; z+1 ; z+2 ; ya ; yb ; immonium
    // series not considered so far : d ; v ; w ; ya ; yb ; immonium
    if(Array("a", "a-NH3", "a-H2O").contains(serie)) return "a"
    else if(Array("b", "b-NH3", "b-H2O").contains(serie)) return "b"
    else if(Array("y", "y-NH3", "y-H2O").contains(serie)) return "y"
    else if(Array("c").contains(serie)) return "c"
    else if(Array("x").contains(serie)) return "x"
    else if(Array("z", "z+1", "z+2").contains(serie)) return "z"
    else {
      logger.info("Ion serie ["+serie+"] not considered")
      return ""
    }
  }
  
  private def xtandemLikePeaksSelection(peptideMatch: PeptideMatch, allPeaks: Array[Peak], usedPeaksCount: Int): Array[Peak] = {
    
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
    usedPeaks.toArray
  }

}