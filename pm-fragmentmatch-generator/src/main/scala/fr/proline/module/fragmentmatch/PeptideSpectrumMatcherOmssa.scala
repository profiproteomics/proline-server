package fr.proline.module.fragmentmatch

import fr.proline.core.om.model.msi.FragmentationRuleSet

import scala.Array.canBuildFrom
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.SearchSettings
import fr.proline.core.om.model.msi.Spectrum

case class PeptideSpectrumMatcherOmssa(
  spectraByIds: Map[Long, Spectrum],
  ms2ErrorTol: Double,
  ms2ErrorTolUnitStr: String,
  fragmentationRuleSet2Use: Option[FragmentationRuleSet],
  fragRuleSetSource : FragmentationRuleSetSource.Value) extends PeptideSpectrumMatcher {

  logger.debug("Generation of spectrum match(es) for OMSSA data")
  if(fragRuleSetSource.equals(FragmentationRuleSetSource.DEFAULT) || fragRuleSetSource.equals(FragmentationRuleSetSource.NONE))
    logger.warn("No fragmentation rule set specified , original ion series will be considered")

  override def isUpdateFragRuleSetNeeded(searchSettings: SearchSettings, fragmentationRuleSet: Option[FragmentationRuleSet], frsSource: FragmentationRuleSetSource.Value): Boolean = {
    if(frsSource.equals(FragmentationRuleSetSource.DEFAULT))
      false //Use original ion series
    else
      super.isUpdateFragRuleSetNeeded(searchSettings,fragmentationRuleSet,frsSource)
  }

  def getUsedPeaks(peptideMatch: PeptideMatch): Array[Peak] = {
    val spectrum = spectraByIds(peptideMatch.getMs2Query.spectrumId)
    val allPeaks = for ((m, i) <- spectrum.mozList.get.zip(spectrum.intensityList.get)) yield new Peak(moz = m, intensity = i)
    allPeaks.sortBy(_.moz)
  }

  def getPtmNeutralLosses(peptideMatch: PeptideMatch): Map[LocatedPtm, Double] = {
    if (peptideMatch.peptide.ptms.exists(_.definition.neutralLosses.size > 1)) {
      peptideMatch.peptide.ptms.filter(_.definition.neutralLosses.size > 1).map(ptm => ptm -> ptm.definition.neutralLosses(1).monoMass).toMap
    } else {
      Map.empty
    }
  }

  def getSequence(peptideMatch: PeptideMatch): Array[Char] = {
    peptideMatch.peptide.sequence.toCharArray()
  }

  def getFragmentIonTypes(peptideMatch: PeptideMatch, charge: Int): FragmentIons = {
    if(!fragRuleSetSource.equals(FragmentationRuleSetSource.NONE) &&  !fragRuleSetSource.equals(FragmentationRuleSetSource.DEFAULT)) {
      val currentFragmentIonTypes = new FragmentIons()
      fragmentationRuleSet2Use.get.fragmentationRules.foreach(fr => currentFragmentIonTypes.setIonTypeAndCharge(convertFragmentationSeries(fr.description), charge))
      currentFragmentIonTypes
    } else { //Use specific to OMSSA properties
      val currentFragmentIonTypes = new FragmentIons()
      if (peptideMatch.properties.isDefined && peptideMatch.properties.get.omssaProperties.isDefined) {
        peptideMatch.properties.get.omssaProperties.get.ionSeries.foreach(currentFragmentIonTypes.setIonTypeAndCharge(_, charge))
      }
      currentFragmentIonTypes
    }
  }

}
