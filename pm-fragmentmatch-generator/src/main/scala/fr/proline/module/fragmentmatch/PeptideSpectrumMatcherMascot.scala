package fr.proline.module.fragmentmatch

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import fr.profi.util.MathUtils
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.model.msi.Peptide

class PeptideSpectrumMatcherMascot(
  val spectraByIds: Map[Long, Spectrum],
  val ms2ErrorTol: Double,
  val ms2ErrorTolUnitStr: String,
  val instrumentConfig: InstrumentConfig) extends PeptideSpectrumMatcher {

  logger.debug("Generation of spectrum match(es) for Mascot data")
  if(!instrumentConfig.fragmentationRules.isDefined || instrumentConfig.fragmentationRules.get.isEmpty) {
    logger.warn("No fragmentation rules found for instrument '"+instrumentConfig.name+"', only 'b' and 'y' ion series will be considered")
  }
  
  def getUsedPeaks(peptideMatch: PeptideMatch): Array[Peak] = {
    val spectrum = spectraByIds(peptideMatch.getMs2Query.spectrumId)
    val allPeaks = for ( (m,i) <- spectrum.mozList.get.zip(spectrum.intensityList.get)) yield new Peak(moz = m, intensity = i)
    val usedPeaksCount = {
      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.mascotProperties.isDefined) {
        peptideMatch.properties.get.mascotProperties.get.getUsedPeaksCount.getOrElse(allPeaks.length)
      } else { allPeaks.length }
    }
    var usedPeaks = mascotLikePeaksSelection(peptideMatch, allPeaks, usedPeaksCount)
    usedPeaks.sortBy(_.moz)
  }
  
  def getPtmNeutralLosses(peptideMatch: PeptideMatch): Map[LocatedPtm, Double] = {
    val nlString = {
      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.mascotProperties.isDefined) {
        peptideMatch.properties.get.mascotProperties.get.getNlString.getOrElse("");
      } else { "" }
    }
    val ptmNeutralLosses = new HashMap[LocatedPtm, Double]()
    for (idx <- 0 until nlString.size) {
    	if(nlString.charAt(idx) != '0') {
    	  val ptm = peptideMatch.peptide.ptms.find(_.seqPosition == (idx))
    	  ptmNeutralLosses += (ptm.get -> ptm.get.definition.neutralLosses(nlString.charAt(idx).asDigit - 1).monoMass)
    	}
    }
    ptmNeutralLosses.toMap
  }
  
  def getSequence(peptideMatch: PeptideMatch): Array[Char] = {
    val seq = peptideMatch.peptide.sequence.toCharArray()
    if (peptideMatch.properties.isDefined && peptideMatch.properties.get.mascotProperties.isDefined && peptideMatch.properties.get.mascotProperties.get.getAmbiguityString.isDefined) {
      val ambiguities = peptideMatch.properties.get.mascotProperties.get.getAmbiguityString.get.split(",")
      for (i <- 0 to (ambiguities.length-1) by 3) {
        seq(ambiguities(i).toInt - 1) = ambiguities(i+2).charAt(0)
      } 
    } 
    seq
  }

  def getFragmentIonTypes(peptideMatch: PeptideMatch, charge: Int): FragmentIons = {
    if(instrumentConfig.fragmentationRules.isDefined && !instrumentConfig.fragmentationRules.get.isEmpty) {
      val currentFragmentIonTypes = new FragmentIons()
	  instrumentConfig.fragmentationRules.get.foreach(fr => currentFragmentIonTypes.setIonTypeAndCharge(mascotFragmentationSeries(fr.description), charge))
	  currentFragmentIonTypes
    } else {
      new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = charge, chargeForIonsY = charge)
    }
  }
  
  private def mascotFragmentationSeries(serie: String): String = {
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
  
  private def mascotLikePeaksSelection(peptideMatch: PeptideMatch, allPeaks: Array[Peak], usedPeaksCount: Int): Array[Peak] = {
    
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
      
      //VDS Bug fix for #13371. To move into Provider ?!
      val calculatedMass = if(peptideMatch.peptide.calculatedMass > 0) peptideMatch.peptide.calculatedMass else Peptide.calcMass(peptideMatch.peptide.sequence, peptideMatch.peptide.ptms)
      var foundAtLeastOnePeaks = true
      while (usedPeaks2.length < usedPeaksCount &&foundAtLeastOnePeaks ) {
        foundAtLeastOnePeaks = false
                
        for (i <- 0 to (allPeaksBinned.length -1)) {
//          logger.info("  !!!!!!  pepMatch "+peptideMatch.id+" used peak count ="+usedPeaksCount+ "for i "+i+ " rank "+rank(i)+" allPeaksBinned lenght "+ allPeaksBinned(i).length)
          while (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz >= calculatedMass) {
        	  rank(i) += 1            
          }
          if (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz < calculatedMass) {
            usedPeaks2 += allPeaksBinned(i)(rank(i))
            foundAtLeastOnePeaks = true;
          }
          rank(i) += 1
        } //End go through allPeaksBinned
      } //End while go up to usedPeaks2.length 

      if(!foundAtLeastOnePeaks && usedPeaks2.length < usedPeaksCount){
        val sb : StringBuilder = new StringBuilder("id : "+peptideMatch.id)
        if(peptideMatch.msQuery != null)
          sb.append( " msQuery initial ID "+peptideMatch.msQuery.initialId)
        if(peptideMatch.peptide != null){
          sb.append( " pep seq "+peptideMatch.peptide.sequence+" - "+peptideMatch.peptide.readablePtmString)
          sb.append( " pep calc Mass db "+peptideMatch.peptide.calculatedMass+" new calc mass"+calculatedMass)
        }
        
        logger.warn(" **** Found only "+usedPeaks2.length+" of "+usedPeaksCount+" peaks for peptide "+ sb.toString())
      }
      
      
      for (i <- 0 to (allPeaksBinned.length -1)) {
          while (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz >= calculatedMass) {
        	  rank(i) += 1            
          }
          if (rank(i) < allPeaksBinned(i).length && allPeaksBinned(i)(rank(i)).moz < calculatedMass && (allPeaksBinned(i)(rank(i)-1).intensity - allPeaksBinned(i)(rank(i)).intensity).abs < MathUtils.EPSILON_FLOAT) {
              usedPeaks2 += allPeaksBinned(i)(rank(i))
          }
      }

      usedPeaks ++= usedPeaks2.sortBy(_.moz)
      
    } else {
      usedPeaks ++= allPeaksSorted
    }
    usedPeaks.toArray
  }

}