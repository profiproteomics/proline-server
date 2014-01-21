package fr.proline.module.fragment_match

import java.util.ArrayList
import com.compomics.util.experiment.biology.AminoAcidPattern
import com.compomics.util.experiment.biology.PTM
import com.compomics.util.experiment.biology.PTMFactory
import com.compomics.util.experiment.identification.matches.ModificationMatch
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmLocation
import com.typesafe.scalalogging.slf4j.Logging

class FragmentModificationMatch(theoreticPtm: String, isVariable: Boolean, modifiedSite: Int) {
  def toCompomicsObject: ModificationMatch = new ModificationMatch(theoreticPtm, isVariable, modifiedSite)
  override def toString: String = { "theoreticPtm: " + theoreticPtm + " ; isVariable: " + isVariable + " ; modifiedSite: " + modifiedSite }
}

/**
 * This object's purpose is to create an instance of PTMFactory and fill it with our PTMs
 * An instance of PTMFactory will be called on the creation of a Compomics Peptide object, so it needs to exist
 * Important note :
 * - The PTMFactory object seems to be made for OMSSA only, it is possible to import OMSSA's PTM files.
 * - The only way to add other PTMs is to use the method addUserPTM, but having only user PTMs should not be a problem
 * Another important note :
 * - The instance saves its content in a local file : System.getProperty("user.home")+"/.compomics/ptmFactory-3.10.32.cus")
 * - It seems impossible to avoid the writing of a file (the PTMFactory instance cannot be loaded from something else)
 * - The user should have the rights to write the file in this path
 *
 */
object PTMsManager extends Logging {

  def init(ptmDefinitions: Array[PtmDefinition]) {
    // get the instance of the factory
    val ptmFactory = PTMFactory.getInstance()
    // load each Proline PTM into the factory
    for (ptm <- ptmDefinitions) {
      try {
        ptmFactory.addUserPTM(
          new PTM(getPtmType(ptm),
            ptm.names.fullName,
            ptm.names.shortName,
            getPtmMass(ptm),
            getPtmResidues(ptm)))
      } catch {
        case e: Exception => logger.warn("PTM could not be added into the Compomic PtmFactory", e)
      }
    }
    // save the instance (it will be saved into : System.getProperty("user.home")+"/.compomics/ptmFactory-3.10.32.cus")
    ptmFactory.saveFactory()
  }

  private def getPtmType(ptmDefinition: PtmDefinition): Int = {
    try {
      if (ptmDefinition.location == PtmLocation.ANY_C_TERM.toString()) return PTM.MODCP
      else if (ptmDefinition.location == PtmLocation.ANY_N_TERM.toString()) return PTM.MODNP
      else if (ptmDefinition.location == PtmLocation.C_TERM.toString()) return PTM.MODCPAA
      else if (ptmDefinition.location == PtmLocation.N_TERM.toString()) return PTM.MODNPAA
      else if (ptmDefinition.location == PtmLocation.PROT_C_TERM.toString()) return PTM.MODC
      else if (ptmDefinition.location == PtmLocation.PROT_N_TERM.toString()) return PTM.MODN
      else return PTM.MODAA
    } catch {
      case e: Exception => logger.warn("PTM location could not be determined from PtmDefinition, default value is 'anywhere'", e)
    }
    return PTM.MODAA
  }

  private def getPtmMass(ptmDefinition: PtmDefinition): Double = {
    try {
      if (ptmDefinition.ptmEvidences.size > 0) return ptmDefinition.ptmEvidences(0).monoMass
      else return 0
    } catch {
      case e: Exception => logger.warn("PTM mass could not be determined from PtmDefinition, default value is '0'", e)
    }
    return 0
  }

  private def getPtmResidues(ptmDefinition: PtmDefinition): AminoAcidPattern = {
    try {
	    if (ptmDefinition.residue != '\0') {
	      val array = new ArrayList[String]
	      array.add(ptmDefinition.residue.toString)
	      return new AminoAcidPattern(array)
	    } else return new AminoAcidPattern()
    } catch {
      case e: Exception => logger.warn("PTM residue could not be determined from PtmDefinition, default value is 'no residue'", e)
    }
    return new AminoAcidPattern()
  }

  override def toString: String = toString(PTMFactory.getInstance())
  def toString(ptmFactory: PTMFactory): String = {
    var string = ""
    for (i <- 0 until ptmFactory.getPTMs.size()) {
      val ptm = ptmFactory.getPTM(ptmFactory.getPTMs.get(i))
      var ptmResidues = ""
      for (j <- 0 until ptm.getResidues().size()) ptmResidues += "\"" + ptm.getResidues().get(j) + "\" "
      string += "- " + ptm.getName() + " (" + ptm.getShortName() + "): " + ptm.getMass() + "Da ; Residues=[ " + ptmResidues + "] ; Type=" + ptm.getType() + "\n"
    }
    return string
  }

}