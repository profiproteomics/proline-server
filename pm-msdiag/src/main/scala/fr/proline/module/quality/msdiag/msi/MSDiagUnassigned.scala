package fr.proline.module.quality.msdiag.msi

import fr.proline.core.om.model.msi.Spectrum

case class MSDiagUnassigned(val unassignedSpectra: Array[Spectrum]) extends MSDiagItem {
  def getLabel: String = "Unassigned spectra"
  def countItems: Int = unassignedSpectra.size
}

class MSDiagChargeUnassigned(val charge: Int, override val unassignedSpectra: Array[Spectrum]) extends MSDiagUnassigned(unassignedSpectra)
class MSDiagRTUnassigned(val rt: Int, override val unassignedSpectra: Array[Spectrum]) extends MSDiagUnassigned(unassignedSpectra)
