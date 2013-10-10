package fr.proline.module.fragment_match

import com.compomics.util.experiment.biology.NeutralLoss
import com.compomics.util.experiment.identification.NeutralLossesMap
import com.weiglewilczek.slf4s.Logging

object NeutralLossType extends Enumeration {
  val CH4OS = "CH4OS" // 64.1075
  val H2O = "H2O" // 18.01532
  val H3PO4 = "H3PO4" // 97.99532
  val HPO3 = "HPO3" // 79.98000
  val NH3 = "NH3" // 17.03056
  def get(value: String): NeutralLoss = {
    value match {
      case "CH4OS" => NeutralLoss.CH4OS
      case "H2O"   => NeutralLoss.H2O
      case "H3PO4" => NeutralLoss.H3PO4
      case "HPO3"  => NeutralLoss.HPO3
      case "NH3"   => NeutralLoss.NH3
      case _       => null
    }
  }
  def get(value: Float): NeutralLoss = {
    if (scala.Math.abs(value - 64.1075) < 0.5) NeutralLoss.CH4OS
    else if (scala.Math.abs(value - 18.01532) < 0.5) NeutralLoss.H2O
    else if (scala.Math.abs(value - 97.99532) < 0.5) NeutralLoss.H3PO4
    else if (scala.Math.abs(value - 79.98000) < 0.5) NeutralLoss.HPO3
    else if (scala.Math.abs(value - 17.03056) < 0.5) NeutralLoss.NH3
    else null
  }
}

class NeutralLosses extends Logging {
  private val map = new NeutralLossesMap
  // use the mass of the neutral loss
  def addNeutralLossByMass(neutralLossMass: Float, bStart: Int, yStart: Int) {
    val nl = NeutralLossType.get(neutralLossMass)
    if (nl != null) addNL(nl, bStart, yStart)
    else logger.warn("No neutral loss found with a mass of " + neutralLossMass)
  }
  // use the name of the neutral loss
  def addNeutralLoss(neutralLossName: String, bStart: Int, yStart: Int) {
    val nl = NeutralLossType.get(neutralLossName)
    if (nl != null) addNL(nl, bStart, yStart)
    else logger.warn("No neutral loss found with the composition '" + neutralLossName + "'")
  }
  private def addNL(nl: NeutralLoss, bStart: Int, yStart: Int) {
    try {
      map.addNeutralLoss(nl, bStart, yStart)
    } catch {
      case e: Exception => logger.warn("Failed to create a new Compomic NeutralLoss  object (" + nl.name + ", " + bStart + ", " + yStart + ")")
    }
  }
  def get = map
}
