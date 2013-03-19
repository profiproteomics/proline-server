package fr.proline.module.parser.omssa

import java.io.IOException
import java.util.ArrayList
import com.compomics.util.experiment.biology.Ion
import com.compomics.util.experiment.biology.ions.PeptideFragmentIon
import com.compomics.util.experiment.biology.IonFactory
import com.compomics.util.experiment.biology.Peptide
import com.compomics.util.experiment.identification.matches.ModificationMatch
import com.weiglewilczek.slf4s.Logging
import scala.collection.mutable.ArrayBuffer
import fr.proline.context.DatabaseConnectionContext

/**
 * @author Alexandre Burel
 * This class is designed to record the information necessary to calculate the FragmentIons
 * It has to be used on the fly :
 * 1/ read the omx file and record the information
 * 2/ at the end of peptide match, calculate the fragment ions
 * 3/ reset everything and go on to the next peptide match
 */
class OmssaFragmentationTable() extends Logging {

  private val fragmentFactory: IonFactory = IonFactory.getInstance()
  /**
   * The current peptide sequence
   * It's content is reseted after Fragment
   */
  private var currentPeptideSequence: String = ""
  /**
   * The current protein accession numbers, related to the peptide match
   * It's content is reseted after Fragment
   */
  private var currentPeptideProteinAccessionNumbers: ArrayList[String] = new ArrayList[String]
  /**
   * The current protein definition, each corresponding to a protein accession number for the same id
   * It's content is reseted after Fragment
   */
  private var currentPeptideProteinDefinition: ArrayList[String] = new ArrayList[String]
  /**
   * The current peptide PTM matches
   * It's content is reseted after Fragment
   */
  private var currentPeptideModificationMatches: ArrayList[ModificationMatch] = new ArrayList[ModificationMatch]

  /**
   * @param peptideSequence the peptide sequence to record
   */
  def setPeptideSequence(peptideSequence: String) { currentPeptideSequence = peptideSequence }
  /**
   * @param accessionNumber the accession number to record
   */
  def addProteinAccessionNumber(accessionNumber: String) { currentPeptideProteinAccessionNumbers.add(accessionNumber) }
  /**
   * @param definition the definition to record
   */
  def addProteinDefinition(defition: String) { currentPeptideProteinDefinition.add(defition) }
  /**
   * @param theoreticPtm the name of the ptm
   * @param isVariable true if the ptm is considered variable, otherwise it is fixed
   * @param modifiedSite the modification site
   */
  def addModificationMatch(theoreticPtm: String, isVariable: Boolean, modifiedSite: Int) { currentPeptideModificationMatches.add(new ModificationMatch(theoreticPtm, isVariable, modifiedSite)) }

  /**
   * Resets the information to record
   * This method should be called before/after each peptide match
   */
  def reset() {
    currentPeptideSequence = ""
    currentPeptideProteinAccessionNumbers = new ArrayList[String]
    currentPeptideProteinDefinition = new ArrayList[String]
    currentPeptideModificationMatches = new ArrayList[ModificationMatch]
  }

  /**
   * @param wantDecoy indicates if the peptide must match to a target or a decoy entry
   * @return true if the first protein is decoy AND wantDecoy=true, or if the first protein is target AND wantDecoy=false ; returns false otherwise
   */
  def matchesToTargetOrDecoyProteins(wantDecoy: Boolean): Boolean = {
    var doMatch = false
    if (currentPeptideProteinDefinition != null && currentPeptideProteinDefinition.size() > 0) {
      // FIXME the following condition should be temporary
      val firstMatchIsDecoy = currentPeptideProteinDefinition.get(0).matches("^Reverse sequence, was .*")
      if (wantDecoy == firstMatchIsDecoy) doMatch = true
    }
    return doMatch
  }
  /**
   * @return Returns a list of FragmentIon objects
   */
  def getFragmentIons(): Array[PeptideFragmentIon] = {
    // check if all the information are available
    if (currentPeptideSequence == "") {
      throw new IOException("Peptide sequence is missing")
    }
    if (currentPeptideProteinAccessionNumbers == null || currentPeptideProteinAccessionNumbers.size() == 0) {
      throw new IOException("No protein accession numbers")
    }
    // calculate the fragments
    var fragments: ArrayList[Ion] = null
    try {
      // getFragmentIons returns an array of Ion objects, with different types (PrecursorIon, PeptideFragmentIon, etc)
      fragments = fragmentFactory.getFragmentIons(new Peptide(currentPeptideSequence, currentPeptideProteinAccessionNumbers, currentPeptideModificationMatches))
    } catch {
      case e => throw new Exception("Failed to calculate the fragment ions for peptide " + currentPeptideSequence + "(" + e.getMessage() + ")")
    }
    // return the result, but only the PeptideFragmentIon objects
    var peptideFragments: ArrayBuffer[PeptideFragmentIon] = new ArrayBuffer[PeptideFragmentIon]
    for (i <- 0 until fragments.size()) {
      fragments.get(i) match {
        case ion: PeptideFragmentIon => peptideFragments += ion
        case _ =>
      }
    }
    return peptideFragments.toArray[PeptideFragmentIon]
  }
}
