package fr.proline.module.fragment_match

import scala.Array.canBuildFrom
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.Spectrum

class PeptideSpectrumMatcherXtandem(
  val spectraByIds: Map[Long, Spectrum],
  val ms2ErrorTol: Double,
  val ms2ErrorTolUnitStr: String) extends PeptideSpectrumMatcher {
  
  logger.debug("Generation of spectrum match(es) for X!Tandem data")

  def getUsedPeaks(peptideMatch: PeptideMatch): Array[Peak] = {
    val spectrum = spectraByIds(peptideMatch.getMs2Query.spectrumId)
    val allPeaks = for ( (m,i) <- spectrum.mozList.get.zip(spectrum.intensityList.get)) yield new Peak(moz = m, intensity = i)
    allPeaks.sortBy(_.moz)
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
    val currentFragmentIonTypes = new FragmentIons()
    if (peptideMatch.properties.isDefined && peptideMatch.properties.get.xtandemProperties.isDefined) {
      peptideMatch.properties.get.xtandemProperties.get.ionSeries.foreach(currentFragmentIonTypes.setIonTypeAndCharge(_, charge))
    }
    currentFragmentIonTypes
  }

}