package fr.proline.module.fragment_match

import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions.asJavaList
import com.compomics.util.experiment.biology.Ion
import com.compomics.util.experiment.biology.IonFactory
import com.compomics.util.experiment.biology.NeutralLoss
import com.compomics.util.experiment.biology.Peptide
import com.compomics.util.experiment.biology.ions.ElementaryIon
import com.compomics.util.experiment.biology.ions.PeptideFragmentIon
import com.compomics.util.experiment.identification.NeutralLossesMap
import com.compomics.util.experiment.identification.matches.ModificationMatch
import com.compomics.util.experiment.biology.PTMFactory
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.proline.core.om.model.msi.PtmDefinition
import com.weiglewilczek.slf4s.Logging

object FragmentTable {
  def fragmentIonTableAsString(_table: Array[TheoreticalFragmentSeries]): String = {
    var text = "Fragment ion table:\n"
    _table.foreach(serie => {
      text += "Serie " + serie.ionSeries + " ("
      for (mass <- serie.masses) text += mass + ";"
      text += ")\n"
    })
    text
  }
}

//case class TwoDimensionsMap[K1, K2, V]() {
//  val wrapped = new scala.collection.mutable.HashMap[(K1, K2), V]
//  def update(k1: K1, k2: K2, v: V) = wrapped.put((k1, k2), v)
//  def apply(k1: K1, k2: K2) = wrapped((k1, k2))
//}

class FragmentIonTable(peptideSequence: String,
                       proteinAccessionNumbers: Array[String],
                       ptmDefinitions: Array[PtmDefinition],
                       modificationMatches: Array[FragmentModificationMatch],
                       currentFragmentIonTypes: FragmentIons,
                       neutralLosses: NeutralLossesMap) extends Logging {

  // Set the current peptide
  private val proteins: ArrayList[String] = new ArrayList(asJavaList(proteinAccessionNumbers))
  private val ptms: ArrayList[ModificationMatch] = new ArrayList[ModificationMatch]()
  modificationMatches.foreach(ptm => ptms.add(ptm.toCompomicsObject))
  PTMsManager.init(ptmDefinitions)
  private val currentPeptide: Peptide = new Peptide(peptideSequence, proteins, ptms)

  // the following object might be needed in FragmentMatchManager.scala to retreive the mass for a neutral loss in a PTM
  // currently only the mass of a NH3 or a H2O can be used if the ion serie indicates it (b* for instance)
  //  private val neutralLossTable = new TwoDimensionsMap[String, Int, Double] // ion type, position, mass
  //  def getNeutralLossTable = neutralLossTable

  // create the fragmentation table
  private val table: Array[TheoreticalFragmentSeries] = {
    val _table = new HashMap[String, ArrayBuffer[Double]]

    // get all fragmentions for the peptide
    val fragmentFactory: IonFactory = IonFactory.getInstance();
    val fragmentIons: ArrayList[Ion] = fragmentFactory.getFragmentIons(currentPeptide);
    // add the theoretical masses to the table
    for (i <- 0 until fragmentIons.size()) {
      val ion = fragmentIons.get(i)
      // @TODO: implement neutral losses
      if (ion.getType() == Ion.IonType.PEPTIDE_FRAGMENT_ION && ion.getNeutralLosses().isEmpty()) {
        ion match {
          case fragmentIon: PeptideFragmentIon => {
            val fragmentMz: Double = (ion.getTheoreticMass() + 1 * ElementaryIon.proton.getTheoreticMass())

            if (currentFragmentIonTypes.contains(fragmentIon.getSubTypeAsString())) {
              fragmentIon.getSubType() match {
                case PeptideFragmentIon.A_ION => addSeries(_table, "a", currentFragmentIonTypes.getCharge("a"), fragmentMz, fragmentIon)
                case PeptideFragmentIon.B_ION => {
                  addSeries(_table, "b", currentFragmentIonTypes.getCharge("b"), fragmentMz, fragmentIon)
                  if (neutralLosses.containsLoss(NeutralLoss.H2O)) addSeries(_table, "b0", currentFragmentIonTypes.getCharge("b"), fragmentMz - NeutralLoss.H2O.mass, fragmentIon)
                  if (neutralLosses.containsLoss(NeutralLoss.NH3)) addSeries(_table, "b*", currentFragmentIonTypes.getCharge("b"), fragmentMz - NeutralLoss.NH3.mass, fragmentIon)
                }
                case PeptideFragmentIon.C_ION => addSeries(_table, "c", currentFragmentIonTypes.getCharge("c"), fragmentMz, fragmentIon)
                // should the mz for the following ions be added backwards ?
                case PeptideFragmentIon.X_ION => addSeries(_table, "x", currentFragmentIonTypes.getCharge("x"), fragmentMz, fragmentIon)
                case PeptideFragmentIon.Y_ION => {
                  addSeries(_table, "y", currentFragmentIonTypes.getCharge("y"), fragmentMz, fragmentIon)
                  if (neutralLosses.containsLoss(NeutralLoss.H2O)) addSeries(_table, "y0", currentFragmentIonTypes.getCharge("y"), fragmentMz - NeutralLoss.H2O.mass, fragmentIon)
                  if (neutralLosses.containsLoss(NeutralLoss.NH3)) addSeries(_table, "y*", currentFragmentIonTypes.getCharge("y"), fragmentMz - NeutralLoss.NH3.mass, fragmentIon)
                }
                case PeptideFragmentIon.Z_ION => addSeries(_table, "z", currentFragmentIonTypes.getCharge("x"), fragmentMz, fragmentIon)
              }
            }
          }
          case _ => null
        }
      }
    }
    // convert the table to an array of Proline TheoreticalFragmentSeries objects
    val fragmentIonTable = new ArrayBuffer[TheoreticalFragmentSeries]
    try {
      for (item <- _table) fragmentIonTable += new TheoreticalFragmentSeries(item._1, item._2.toArray)
    } catch {
      case e: Exception => logger.warn("The fragment ion table could not be generated, default value is 'new ArrayBuffer[TheoreticalFragmentSeries]'")
    }
    fragmentIonTable.toArray
  }
  def get: Array[TheoreticalFragmentSeries] = table

  private def addFragmentMz(_table: HashMap[String, ArrayBuffer[Double]], serie: String, mz: Double) {
    if (!_table.contains(serie)) _table.put(serie, new ArrayBuffer[Double])
    _table(serie) += mz
  }

  private def addSeries(_table: HashMap[String, ArrayBuffer[Double]], seriePrefix: String, charge: Int, mz: Double, fragmentIon: PeptideFragmentIon) {
    var serie = seriePrefix
    addFragmentMz(_table, serie, mz)
    // neutralLossTable.update(serie, fragmentIon.getNumber(), sumNeutralLossMasses(fragmentIon))
    if (charge > 1) {
      serie += "+" // "+" is not indicated for singly charged ion (implicit)
      // for each charge in the serie (ie. b, b++, b+++), add the corresponding moz
      for (c <- 2 to charge) {
        serie += "+"
        addFragmentMz(_table, serie, mz / c) // divide the mass by the charge to get the moz
        // neutralLossTable.update(serie, fragmentIon.getNumber(), sumNeutralLossMasses(fragmentIon))
      }
    }
  }

  //  private def sumNeutralLossMasses(fragmentIon: PeptideFragmentIon): Double = {
  //    var mass: Double = 0
  //    for(i <- 0 until fragmentIon.getNeutralLosses().size()) {
  //      mass += fragmentIon.getNeutralLosses().get(i).mass
  //    }
  //    mass
  //  }

  override def toString: String = FragmentTable.fragmentIonTableAsString(table)

}