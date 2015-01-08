package fr.proline.module.fragment_match

case class FragmentIons(

  // the user can say for each type if it is present or not
  var ionTypeA: Boolean = false,
  var ionTypeB: Boolean = false,
  var ionTypeC: Boolean = false,
  var ionTypeX: Boolean = false,
  var ionTypeY: Boolean = false,
  var ionTypeZ: Boolean = false,

  // the default charge for each ion type
  var chargeForIonsA: Int = 1,
  var chargeForIonsB: Int = 1,
  var chargeForIonsC: Int = 1,
  var chargeForIonsX: Int = 1,
  var chargeForIonsY: Int = 1,
  var chargeForIonsZ: Int = 1) {

  def setIonTypeAndCharge(ion: String, charge: Int) {
    ion.toLowerCase() match {
      case "a" => {
        ionTypeA = true
        chargeForIonsA = charge
      }
      case "b" => {
        ionTypeB = true
        chargeForIonsB = charge
      }
      case "c" => {
        ionTypeC = true
        chargeForIonsC = charge
      }
      case "x" => {
        ionTypeX = true
        chargeForIonsX = charge
      }
      case "y" => {
        ionTypeY = true
        chargeForIonsY = charge
      }
      case "z" => {
        ionTypeZ = true
        chargeForIonsZ = charge
      }
      case _  => {
        
      }
    }
  }

  def contains(ionType: String): Boolean = {
    val ion = ionType.toLowerCase()
    if (ion == "a" && this.ionTypeA) true
    else if (ion == "b" && this.ionTypeB) true
    else if (ion == "c" && this.ionTypeC) true
    else if (ion == "x" && this.ionTypeX) true
    else if (ion == "y" && this.ionTypeY) true
    else if (ion == "z" && this.ionTypeZ) true
    else false
  }

  def getCharge(ionType: String): Int = {
    val ion = ionType.toLowerCase()
    if (ion == "a" && this.ionTypeA) this.chargeForIonsA
    else if (ion == "b" && this.ionTypeB) this.chargeForIonsB
    else if (ion == "c" && this.ionTypeC) this.chargeForIonsC
    else if (ion == "x" && this.ionTypeX) this.chargeForIonsX
    else if (ion == "y" && this.ionTypeY) this.chargeForIonsY
    else if (ion == "z" && this.ionTypeZ) this.chargeForIonsZ
    else 0
  }

}
