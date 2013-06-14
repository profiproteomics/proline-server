package fr.proline.module.fragment_match

import java.lang.Math.abs
import scala.collection.mutable.ArrayBuffer
import com.compomics.util.experiment.biology.NeutralLoss
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.FragmentMatchType
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import com.weiglewilczek.slf4s.Logging

//class FragmentMatchManager(fragmentIonTable: HashMap[String, ArrayBuffer[Double]], neutralLossTable: TwoDimensionsMap[String, Int, Double]) {
class FragmentMatchManager extends Logging {

  private val fragmentMatches = new ArrayBuffer[FragmentMatch]
  private var fragmentIonTable: Array[TheoreticalFragmentSeries] = null
  private var spectrum: Spectrum = null

  def get: Array[FragmentMatch] = { if(fragmentIonTable == null || spectrum == null) null else fragmentMatches.toArray }
  def get(_fragmentIonTable: Array[TheoreticalFragmentSeries], _spectrum: Spectrum): Array[FragmentMatch] = {
    if (fragmentIonTable == null || spectrum == null) {
      fragmentIonTable = _fragmentIonTable
      spectrum = _spectrum
      fragmentMatches.foreach(fm => {
        fm.calculatedMoz = calculatedMoz(fragmentIonTable, fm.label)
        fm.intensity = intensity(spectrum, fm.moz)
      })
    }
    fragmentMatches.toArray
  }

  def add(ionType: String, charge: Int, position: Int, moz: Double) {
    var newIonType = ionType
    for(i <- 1 to charge) newIonType += "+"
    add(newIonType, position, moz)
  }
  def add(ionType: String, position: Int, moz: Double) {
    try {
      fragmentMatches += new FragmentMatch(
        label = label(ionType, position), // b(3)++ for a doubly charged "b" ion for the third amino acid of the peptide
        `type` = Some(fragmentType(ionType)), // none (regular), internal or immonium
        moz = moz,
        calculatedMoz = 0, // will be retreived later in the fragment ion table (cf. method get)
        intensity = 0, // will be retreived later in the spectrum (cf. method get)
        neutralLossMass = Some(neutralLossMass(ionType))
      )
    } catch {
      case e: Exception => logger.warn("Compomic FragmentMatch could not be created (ion type: " + ionType + ", position: " + position + ", moz: " + moz + ")")
    }
  }

  private def label(ion: String, position: Int): String = {
    try {
      // input may be : b, b++, b*, b*++, b0, b0++ (or any other ion type)
      val regex = """([a-zA-Z]+)[^a-zA-Z+]*([+]*).*""".r
      val regex(serie, chargeAsString) = ion
      // return b(3)++ for a doubly charged "b" ion in 3rd position
      return serie + "(" + position + ")" + chargeAsString
    } catch {
      case e: Exception => logger.warn("Ion serie could not be retreived from the ion type '" + ion + "', default value is ''")
    }
    ""
  }

  private def fragmentType(ion: String): String = {
    try {
      if (ion == "ya" || ion == "yb" || ion == "interal") return FragmentMatchType.INTERNAL.toString // ya/yb : mascot names
      else if (ion == "immonium") return FragmentMatchType.IMMONIUM.toString // same for mascot and omssa
      else return null
    } catch {
      case e: Exception => logger.warn("Fragment type could not be retreived from the ion type '" + ion + "', default value is 'null'")
    }
    return null
  }

  private def calculatedMoz(fragmentIonTable: Array[TheoreticalFragmentSeries], label: String): Double = {
    try {
      var calcMz: Double = 0
      // label shoulb be b(3)++ for a doubly charged "b" ion in 3rd position
      val regex = """(\w+)\((\d+)\).*""".r
      val regex(ionType, positionStr) = label
      val position = positionStr.toInt
      for (serie <- fragmentIonTable) {
        if (serie.ionSeries == ionType) calcMz = serie.masses(position - 1)
      }
      return calcMz
    } catch {
      case e: Exception => logger.warn("Calculated MoZ could not be retreived in the fragment ion table for the label '" + label + "', default value is '0'")
    }
    return 0
  }

  /**
   * intensity corresponding to the given Moz, searched in the given spectrum
   */
  private def intensity(spectrum: Spectrum, moz: Double): Float = {
    try {
      var intensity: Float = -1
      var lowestMozDifference: Double = -1
      var idOfTheClosestMoz: Int = -1
      // read all the masses in the spectrum
      if (spectrum.mozList != None && spectrum.intensityList != None) {
        val mozList = spectrum.mozList.get
        for (i <- 0 until mozList.size) {
          // get the id of the closest to the given moz
          if (lowestMozDifference == -1 || (abs(moz - mozList(i)) < lowestMozDifference)) {
            lowestMozDifference = abs(moz - mozList(i))
            idOfTheClosestMoz = i
          }
        }
        intensity = spectrum.intensityList.get(idOfTheClosestMoz)
      }
      return intensity
    } catch {
      case e: Exception => logger.warn("Intensity could not be retreived in the given spectrum for the moz '" + moz + "', default value is '0'")
    }
    return 0
  }

  /**
   * TODO when the full list of PTMs is loaded, maybe the mass will be given by the PeptideFragmentIon object from Compomics (stored in neutralLossTable, but commented)
   * @param ion
   * @return
   */
  private def neutralLossMass(ion: String): Double = {
    try {
    // input may be : b, b++, b*, b*++, b0, b0++ (or any other ion type)
    val regex = """[a-zA-Z]+([0\*]*)[+]*.*""".r
    val regex(nl) = ion
    if (nl == "*") return NeutralLoss.NH3.mass
    else if (nl == "0") return NeutralLoss.H2O.mass
    else return 0
    } catch {
      case e: Exception => logger.warn("Neutral loss mass could not be determined for the ion '" + ion + "', default value is '0'")
    }
    return 0
  }

}