package fr.proline.module.fragment_match

import com.compomics.util.experiment.biology.NeutralLoss
import com.compomics.util.experiment.identification.NeutralLossesMap
import com.weiglewilczek.slf4s.Logging

object NeutralLossType extends Enumeration {
  val CH4OS = "CH4OS"
  val H2O = "H2O"
  val H3PO4 = "H3PO4"
  val HPO3 = "HPO3"
  val NH3 = "NH3"
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
}

class NeutralLosses extends Logging {
  private val map = new NeutralLossesMap
  def addNeutralLoss(neutralLoss: String, bStart: Int, yStart: Int) {
    try {
      map.addNeutralLoss(NeutralLossType.get(neutralLoss), bStart, yStart)
    } catch {
      case e: Exception => logger.warn("Failed to create a new Compomic NeutralLoss  object (" + neutralLoss + ", " + bStart + ", " + yStart + ")")
    }
  }
  //	def getNeutralLossMap = map
  def get = map
}
