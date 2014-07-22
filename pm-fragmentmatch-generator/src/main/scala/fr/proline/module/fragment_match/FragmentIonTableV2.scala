package fr.proline.module.fragment_match

import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions.asJavaList
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.proline.core.om.model.msi.PtmDefinition
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.Peptide
import fr.profi.chemistry.model.MolecularConstants
import fr.profi.chemistry.model.AminoAcidTable
import fr.profi.chemistry.model.HumanAminoAcidTable
import fr.proline.core.om.model.msi.LocatedPtm
import fr.profi.chemistry.model.AminoAcidResidue
import fr.profi.chemistry.model.AtomTable
import fr.profi.chemistry.model.BiomoleculeAtomTable
import fr.proline.core.om.model.msi.PtmEvidence
import fr.proline.core.om.model.msi.FragmentMatchType
import fr.proline.core.om.model.msi.FragmentIonType
import fr.proline.core.om.model.msi.Fragmentation
import fr.profi.util.MathUtils


case class Fragment(moz: Double,
				    position: Int,
				    neutralLoss: Double = 0.0,
				    fragmentType: Option[FragmentMatchType.Value] = None,
				    series: Option[String] = None,
				    charge: Int = 1,
				    isAlternativeNL: Boolean = false)

object FragmentIonTableV2 {

  def fragmentIonTableAsString(_table: Array[TheoreticalFragmentSeries]): String = {
    var text = "Fragment ion table:\n"
    _table.foreach(serie => {
      text += "Serie " + serie.ionSeries + List.fill(serie.charge)("+").mkString + " ("
      text += serie.masses.map("%1.4f".format(_)).mkString(";")
      text += ")\n"
    })
    text
  }

  val aminoAcids = {
    val hmBuilder = HashMap.newBuilder[Char, AminoAcidResidue]
    val aa = AminoAcidTable(HumanAminoAcidTable.aminoAcids)
    for (entity <- HumanAminoAcidTable.aminoAcids) hmBuilder += (entity.code1 -> entity)
    hmBuilder += ('X' -> AminoAcidResidue(
      code1 = 'X',
      code3 = "Xaa",
      name = "Unknown",
      formula = "",
      monoMass = MolecularConstants.AVERAGE_AA_MASS,
      averageMass = MolecularConstants.AVERAGE_AA_MASS,
      occurrence = 0.0f))
    hmBuilder += ('B' -> AminoAcidResidue(
      code1 = 'B',
      code3 = "Asx",
      name = "Asn or Asp",
      formula = "",
      monoMass = (aa.getAminoAcid("N").monoMass + aa.getAminoAcid("D").monoMass) / 2.0,
      averageMass = (aa.getAminoAcid("N").averageMass + aa.getAminoAcid("D").averageMass) / 2.0,
      occurrence = 0.0f))
    hmBuilder += ('Z' -> AminoAcidResidue(
      code1 = 'Z',
      code3 = "Glx",
      name = "Glu or Gln",
      formula = "",
      monoMass = (aa.getAminoAcid("E").monoMass + aa.getAminoAcid("Q").monoMass) / 2.0,
      averageMass = (aa.getAminoAcid("E").averageMass + aa.getAminoAcid("Q").averageMass) / 2.0,
      occurrence = 0.0f))

    hmBuilder.result
  }

  val atoms = AtomTable(BiomoleculeAtomTable.atoms)
  val H2O = 2 * atoms.getAtom("H").monoMass + atoms.getAtom("O").monoMass
  val NH3 = 3 * atoms.getAtom("H").monoMass + atoms.getAtom("N").monoMass
  val CO = atoms.getAtom("C").monoMass + atoms.getAtom("O").monoMass

}

class FragmentIonTableV2(peptide: Peptide,
  currentFragmentIonTypes: FragmentIons,
  sequence: Option[Array[Char]] = None,
  ptmNeutralLosses: Option[Map[LocatedPtm, Double]] = None) extends Logging {
  
  val fragments: HashMap[String, ArrayBuffer[Fragment]] = {
    val _table = new HashMap[String, ArrayBuffer[Fragment]]
    val ptmByPosition = new HashMap[Int, LocatedPtm]
    
    if (peptide.ptms != null)
      ptmByPosition ++= peptide.ptms.map { ptm => (ptm.seqPosition -> ptm) }

    var mass = MolecularConstants.WATER_MONO_MASS
    val nlResidues = Map("H2O" -> ArrayBuffer(0, 0), "NH3" -> ArrayBuffer(0, 0))
    val aaSequence = if (sequence.isDefined) sequence.get else peptide.sequence.toCharArray()
    for (aa <- aaSequence) {
      mass += FragmentIonTableV2.aminoAcids(aa).monoMass
      aa match {
        case 'R' | 'K' | 'N' | 'Q' => nlResidues("NH3")(1) += 1
        case 'S' | 'T' | 'E' | 'D' => nlResidues("H2O")(1) += 1
        case _ =>
      }
    }
   
    var nterMass = 0.0
    var cterPtmNLMasse = 0.0
    
    if (peptide.ptms != null) {
      for (ptm <- peptide.ptms) {
        val nlMass = {
          if (ptmNeutralLosses.isDefined && ptmNeutralLosses.get.contains(ptm)) {
            ptmNeutralLosses.get(ptm)
          } else 0
        }
        mass = mass + ptm.monoMass - nlMass
        cterPtmNLMasse += nlMass
        if (ptm.isNTerm) nterMass += ptm.monoMass
      }
    }
        
        
    var bFragmentMz = MolecularConstants.PROTON_MASS + nterMass
    var position = 0
    var nterPtmlNLMasse = 0.0
    var reversePosition = aaSequence.length
    
    // initialize last reverse ions fragments to 0.0
    if (currentFragmentIonTypes.contains("y")) {
      addSeries(_table, "y", currentFragmentIonTypes.getCharge("y"), new Fragment(0.0, reversePosition, cterPtmNLMasse))
      addSeries(_table, "y*", currentFragmentIonTypes.getCharge("y"), new Fragment(0.0, reversePosition, cterPtmNLMasse))
      addSeries(_table, "y0", currentFragmentIonTypes.getCharge("y"), new Fragment(0.0, reversePosition, cterPtmNLMasse))
    }
    
    if (currentFragmentIonTypes.contains("x"))  addSeries(_table, "x", currentFragmentIonTypes.getCharge("x"), new Fragment(0.0, reversePosition, cterPtmNLMasse))
    if (currentFragmentIonTypes.contains("z"))  addSeries(_table, "z", currentFragmentIonTypes.getCharge("z"), new Fragment(0.0, reversePosition, cterPtmNLMasse))
    
    position += 1
    reversePosition -= 1
    
    for (aa <- aaSequence.dropRight(1)) {
      val residue = FragmentIonTableV2.aminoAcids(aa)
      val (ptmMass, ptmNLMass)  =  { 
        if (!ptmByPosition.contains(position)) 
          (0.0, 0.0)
        else {
          val ptm = ptmByPosition.get(position).get
          (ptm.monoMass, if (ptmNeutralLosses.isDefined && ptmNeutralLosses.isDefined && ptmNeutralLosses.get.contains(ptm)) ptmNeutralLosses.get(ptm) else 0.0)
        }
      }
    
      bFragmentMz = bFragmentMz + residue.monoMass + ptmMass - ptmNLMass
      nterPtmlNLMasse += ptmNLMass
      cterPtmNLMasse -= ptmNLMass
      val yFragmentMz = mass - bFragmentMz + 2 * MolecularConstants.PROTON_MASS
      aa match {
        case 'R' | 'K' | 'N' | 'Q' =>
          nlResidues("NH3")(0) += 1
          nlResidues("NH3")(1) -= 1
        case 'S' | 'T' | 'E' | 'D' =>
          nlResidues("H2O")(0) += 1
          nlResidues("H2O")(1) -= 1
        case _ =>
      }

      if (currentFragmentIonTypes.contains("a")) { 
        addSeries(_table, "a", currentFragmentIonTypes.getCharge("a"), new Fragment(bFragmentMz - FragmentIonTableV2.CO, position, nterPtmlNLMasse))
        addSeries(_table, "a*", currentFragmentIonTypes.getCharge("a"), new Fragment(bFragmentMz - FragmentIonTableV2.CO - FragmentIonTableV2.NH3, position, nterPtmlNLMasse))
        addSeries(_table, "a0", currentFragmentIonTypes.getCharge("a"), new Fragment(bFragmentMz - FragmentIonTableV2.CO - FragmentIonTableV2.H2O, position, nterPtmlNLMasse))
      }

      if (currentFragmentIonTypes.contains("b")) {
        addSeries(_table, "b", currentFragmentIonTypes.getCharge("b"), new Fragment(bFragmentMz, position, nterPtmlNLMasse))
        addSeries(_table, "b*", currentFragmentIonTypes.getCharge("b"), new Fragment(if (nlResidues("NH3")(0) > 0) bFragmentMz - FragmentIonTableV2.NH3 else 0.0, position, nterPtmlNLMasse))
        addSeries(_table, "b0", currentFragmentIonTypes.getCharge("b"), new Fragment(if (nlResidues("H2O")(0) > 0) bFragmentMz - FragmentIonTableV2.H2O else 0.0, position, nterPtmlNLMasse))
      }
      
      if (currentFragmentIonTypes.contains("c")) addSeries(_table, "c", currentFragmentIonTypes.getCharge("c"), new Fragment(bFragmentMz + FragmentIonTableV2.NH3, position, nterPtmlNLMasse))

      if (currentFragmentIonTypes.contains("y")) {
        addSeries(_table, "y", currentFragmentIonTypes.getCharge("y"), new Fragment(yFragmentMz, reversePosition, cterPtmNLMasse))
        addSeries(_table, "y*", currentFragmentIonTypes.getCharge("y"), new Fragment(if (nlResidues("NH3")(1) > 0) yFragmentMz - FragmentIonTableV2.NH3 else 0.0, reversePosition, cterPtmNLMasse))
        addSeries(_table, "y0", currentFragmentIonTypes.getCharge("y"), new Fragment(if (nlResidues("H2O")(1) > 0) yFragmentMz - FragmentIonTableV2.H2O else 0.0, reversePosition, cterPtmNLMasse))
      }

      if (currentFragmentIonTypes.contains("x"))  addSeries(_table, "x", currentFragmentIonTypes.getCharge("x"), new Fragment(yFragmentMz + FragmentIonTableV2.CO, reversePosition, cterPtmNLMasse))
      if (currentFragmentIonTypes.contains("z"))  addSeries(_table, "z", currentFragmentIonTypes.getCharge("z"), new Fragment(yFragmentMz - FragmentIonTableV2.NH3, reversePosition, cterPtmNLMasse))

      position += 1 
      reversePosition -= 1
    } // and AA iteration loop

    position += 1
    // initialize last forward ions fragments to 0.0
    if (currentFragmentIonTypes.contains("b")) {
      addSeries(_table, "b", currentFragmentIonTypes.getCharge("b"), new Fragment(0.0, position, nterPtmlNLMasse))
      addSeries(_table, "b*", currentFragmentIonTypes.getCharge("b"), new Fragment(0.0, position, nterPtmlNLMasse))
      addSeries(_table, "b0", currentFragmentIonTypes.getCharge("b"), new Fragment(0.0, position, nterPtmlNLMasse))
    }
    if (currentFragmentIonTypes.contains("a")) {
      addSeries(_table, "a", currentFragmentIonTypes.getCharge("a"), new Fragment(0.0, position, cterPtmNLMasse))
      addSeries(_table, "a*", currentFragmentIonTypes.getCharge("a"), new Fragment(0.0, position, cterPtmNLMasse))
      addSeries(_table, "a0", currentFragmentIonTypes.getCharge("a"), new Fragment(0.0, position, cterPtmNLMasse))
    }
    if (currentFragmentIonTypes.contains("c")) addSeries(_table, "c", currentFragmentIonTypes.getCharge("c"), new Fragment(0.0, position, cterPtmNLMasse))

    // generate alternate NL fragments
    if (peptide.ptms != null) { 
    for (seriesName <- _table.keys) {
      val isReverse = Fragmentation.isReverseSeries(seriesName)
      for (fragment <- _table(seriesName).filter(_.moz > 0.0)) {
        val includedPtms = if (!isReverse) peptide.ptms.filter(_.seqPosition <= fragment.position) else peptide.ptms.filter(aaSequence.length - _.seqPosition <= (fragment.position-1))
        val includedPtmsByDefinition = includedPtms.groupBy(_.definition)
        for (ptmDef <- includedPtmsByDefinition.keys) {
          for (nl <- ptmDef.neutralLosses) {
            if ((includedPtmsByDefinition(ptmDef).size*nl.monoMass - fragment.neutralLoss).abs > MathUtils.EPSILON_FLOAT) {
              // Add a new fragment. WARNING : this alternative fragments are added at the end of the serie
              addFragment(_table, seriesName, new Fragment( 
            		  moz = fragment.moz - (includedPtmsByDefinition(ptmDef).size*nl.monoMass - fragment.neutralLoss)/fragment.charge.toDouble , 
            		  position = fragment.position, 
            		  neutralLoss = includedPtmsByDefinition(ptmDef).size*nl.monoMass,
            		  fragmentType = fragment.fragmentType,
            		  series = fragment.series, 
            		  charge = fragment.charge,
            		  isAlternativeNL = true) )
            }
          }
        }
      }
    }
    }
    
    _table
  }

  private val table: Array[TheoreticalFragmentSeries] = {
    val fragmentIonTable = new ArrayBuffer[TheoreticalFragmentSeries]
    try {
      for (item <- fragments) {
        if (item._2.exists { _.moz > 0.0 }) fragmentIonTable += new TheoreticalFragmentSeries(item._1, item._2.filter(!_.isAlternativeNL).map(_.moz).toArray)
      }
    } catch {
      case e: Exception => logger.warn("The fragment ion table could not be generated, default value is 'new ArrayBuffer[TheoreticalFragmentSeries]'")
    }
    fragmentIonTable.toArray
  }

  def get: Array[TheoreticalFragmentSeries] = table

  private def addFragment(_table: HashMap[String, ArrayBuffer[Fragment]], serie: String, fragment: Fragment) {
    if (!_table.contains(serie)) _table.put(serie, new ArrayBuffer[Fragment])
    _table(serie) += fragment
  }

  private def addSeries(_table: HashMap[String, ArrayBuffer[Fragment]], seriePrefix: String, charge: Int, fragment: Fragment) {
    var serie = seriePrefix
    addFragment(_table, serie, new Fragment( 
            moz = fragment.moz, 
            position = fragment.position, 
            neutralLoss = fragment.neutralLoss,
            fragmentType = fragment.fragmentType,
            series = Some(serie),
            charge = 1))
    // neutralLossTable.update(serie, fragmentIon.getNumber(), sumNeutralLossMasses(fragmentIon))
    if (charge > 1) {
      serie += "+" // "+" is not indicated for singly charged ion (implicit)
      // for each charge in the serie (ie. b, b++, b+++), add the corresponding moz
      for (c <- 2 to charge) {
        serie += "+"
        addFragment(_table, serie, new Fragment( 
            moz = { if (fragment.moz > 0) (fragment.moz + (c - 1) * MolecularConstants.PROTON_MASS) / c else 0.0 }, 
            position = fragment.position, 
            neutralLoss = fragment.neutralLoss,
            fragmentType = fragment.fragmentType,
            series = Some(serie),
            charge = 2))
      }
    }
  }

  override def toString: String = FragmentIonTableV2.fragmentIonTableAsString(table)

}