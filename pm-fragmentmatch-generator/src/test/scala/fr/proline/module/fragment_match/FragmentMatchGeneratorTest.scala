package fr.proline.module.fragment_match

import scala.collection.mutable.ArrayBuffer

import org.junit.Test

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.module.parser.provider.fake.PTMFakeProvider

@Test
class FragmentMatchGeneratorTest extends Logging {

  @Test
  def test1() {
    try {
      // prepare the information for the creation of the objects
      val mozList: Array[Double] = Array(225.762, 256.918, 304.042, 336.552, 344.929, 348.653, 354.928, 357.59, 374.053, 391.13, 393.579, 397.679, 402.552, 411.001, 418.952, 430.236, 437.177, 442.052, 454.552, 461.174, 470.927, 482.097, 487.634, 493.053, 502.778, 506.353, 507.802, 511.428, 511.922, 513.222, 517.122, 518.673, 521.537, 523.552, 525.554, 526.165, 530.418, 535.845, 539.172, 541.179, 544.099, 552.148, 555.18, 557.218, 559.156, 559.6, 561.428, 562.699, 564.408, 593.075, 595.752, 599.553, 604.43, 611.744, 621.303, 631.177, 643.427, 655.303, 677.053, 682.46, 688.071, 693.552, 703.6, 704.038, 707.802, 713.177, 725.575, 726.901, 747.181, 763.677, 765.183, 770.302, 783.302, 791.185, 791.691, 796.677, 798.307, 805.178, 833.052, 844.543, 860.181, 863.426, 889.053, 893.303, 1018.179, 1020.178, 1097.305)
      val intensities: Array[Float] = Array(90582, 139774, 137480, 247383, 92722, 462061, 175899, 208096, 145688, 318027, 178776, 93182, 250389, 339414, 117284, 217186, 97447, 127176, 160208, 140506, 134782, 527915, 316761, 134030, 623833, 198280, 325789, 113315, 162768, 327293, 251391, 208098, 223256, 226933, 289826, 130192, 311329, 661649, 564437, 448228, 269927, 109183, 130159, 719668, 586538, 402670, 270546, 670199, 1883224, 230397, 386134, 126083, 385802, 490822, 103728, 123214, 221649, 101715, 345230, 120308, 147145, 189606, 142145, 185805, 317295, 128187, 255841, 136029, 420654, 109122, 340285, 95793, 92487, 232317, 169438, 105377, 98772, 140412, 100677, 249585, 183424, 94494, 124880, 122912, 116571, 121320, 206212)
      val spectrum = new Spectrum(
        id = 11,
        title = "Cmpd 11, +MSn(417.182190), 1.4 min",
        precursorMoz = 417.182190,
        precursorCharge = 2,
        mozList = Some(mozList),
        intensityList = Some(intensities),
        peaksCount = mozList.length,
        instrumentConfigId = 7,
        peaklistId = 1)
      val peptideSequence = "APGFGDNR"
      val msQueryId = 11
      val peptideMatchRank = 1
      val modificationMatches: Array[FragmentModificationMatch] = Array( // the name of a FragmentModificationMatch must match a ptm name in 'ptms: ArrayBuffer[PtmDefinition]' !
      )
      val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
      val neutralLosses: NeutralLosses = new NeutralLosses
      neutralLosses.addNeutralLoss(NeutralLossType.H2O, 6, 1)
      neutralLosses.addNeutralLoss(NeutralLossType.NH3, 5, 3)

      // create the fragment ion table
      val table = new FragmentIonTable(	peptideSequence,
								        modificationMatches,
								        currentFragmentIonTypes, // The list of currently selected fragment ion types
								        neutralLosses.get) // The list of the currently selected neutral loss types

      // create the fragment matches
      val fragmentMatches = new FragmentMatchManager
      fragmentMatches.add("b", 1, 72.0444)
      fragmentMatches.add("b", 2, 169.0972)
      fragmentMatches.add("b++", 4, 187.0972)
      fragmentMatches.add("b0", 6, 527.2249)

      // create the spectrum match with the previous information
      val spectrumMatch = new SpectrumMatch(msQueryId, peptideMatchRank, table.get, fragmentMatches.get(table.get, /*table.getNeutralLossTable,*/ spectrum))

      // print the content of the spectrum match
      logger.debug(table.toString)
      logger.debug("SpectrumMatch: msQueryInitialId:" + spectrumMatch.msQueryInitialId + " peptideMatchRank:" + spectrumMatch.peptideMatchRank)
      for (sm <- fragmentMatches.get) logger.debug("FragmentMatch " + sm.label + " : type:" + sm.`type`.toString() + " moz:" + sm.moz + " theoMoz:" + sm.calculatedMoz + " intensity:" + sm.intensity + " neutralLossMass:" + sm.neutralLossMass)
    } catch {
      case e: Exception => {
        logger.error("Error somewhere... ", e)
        throw e
      }
    }
  }

  @Test
  def test2() {
    try {
      // prepare the information for the creation of the objects
      val mozList: Array[Double] = Array(225.762, 256.918, 304.042, 336.552, 344.929, 348.653, 354.928, 357.59, 374.053, 391.13, 393.579, 397.679, 402.552, 411.001, 418.952, 430.236, 437.177, 442.052, 454.552, 461.174, 470.927, 482.097, 487.634, 493.053, 502.778, 506.353, 507.802, 511.428, 511.922, 513.222, 517.122, 518.673, 521.537, 523.552, 525.554, 526.165, 530.418, 535.845, 539.172, 541.179, 544.099, 552.148, 555.18, 557.218, 559.156, 559.6, 561.428, 562.699, 564.408, 593.075, 595.752, 599.553, 604.43, 611.744, 621.303, 631.177, 643.427, 655.303, 677.053, 682.46, 688.071, 693.552, 703.6, 704.038, 707.802, 713.177, 725.575, 726.901, 747.181, 763.677, 765.183, 770.302, 783.302, 791.185, 791.691, 796.677, 798.307, 805.178, 833.052, 844.543, 860.181, 863.426, 889.053, 893.303, 1018.179, 1020.178, 1097.305)
      val intensities: Array[Float] = Array(90582, 139774, 137480, 247383, 92722, 462061, 175899, 208096, 145688, 318027, 178776, 93182, 250389, 339414, 117284, 217186, 97447, 127176, 160208, 140506, 134782, 527915, 316761, 134030, 623833, 198280, 325789, 113315, 162768, 327293, 251391, 208098, 223256, 226933, 289826, 130192, 311329, 661649, 564437, 448228, 269927, 109183, 130159, 719668, 586538, 402670, 270546, 670199, 1883224, 230397, 386134, 126083, 385802, 490822, 103728, 123214, 221649, 101715, 345230, 120308, 147145, 189606, 142145, 185805, 317295, 128187, 255841, 136029, 420654, 109122, 340285, 95793, 92487, 232317, 169438, 105377, 98772, 140412, 100677, 249585, 183424, 94494, 124880, 122912, 116571, 121320, 206212)
      val spectrum = new Spectrum(
        id = 11,
        title = "Cmpd 11, +MSn(417.182190), 1.4 min",
        precursorMoz = 417.182190,
        precursorCharge = 2,
        mozList = Some(mozList),
        intensityList = Some(intensities),
        peaksCount = mozList.length,
        instrumentConfigId = 7,
        peaklistId = 1)
      val peptideSequence = "APGFGDNR"
      val msQueryId = 11
      val peptideMatchRank = 1
      val modificationMatches: Array[FragmentModificationMatch] = Array( // the name of a FragmentModificationMatch must match a ptm name in 'ptms: ArrayBuffer[PtmDefinition]' !
      )
      val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
      val neutralLosses: NeutralLosses = new NeutralLosses
      neutralLosses.addNeutralLoss(NeutralLossType.H2O, 6, 1)
      neutralLosses.addNeutralLoss(NeutralLossType.NH3, 5, 3)

      // create the fragment ion table
      val table = new FragmentIonTable(	peptideSequence,
								        modificationMatches,
								        currentFragmentIonTypes, // The list of currently selected fragment ion types
								        neutralLosses.get) // The list of the currently selected neutral loss types

      // create the fragment matches
      val fragmentMatches = new FragmentMatchManager
      fragmentMatches.add("b", 1, 72.0444)
      fragmentMatches.add("b", 2, 169.0972)
      fragmentMatches.add("b++", 4, 187.0972)
      fragmentMatches.add("b0", 6, 527.2249)

      // create the spectrum match with the previous information
      val spectrumMatch = new SpectrumMatch(msQueryId, peptideMatchRank, table.get, fragmentMatches.get(table.get, /*table.getNeutralLossTable,*/ spectrum))

      // print the content of the spectrum match
      logger.debug(table.toString)
      logger.debug("SpectrumMatch: msQueryInitialId:" + spectrumMatch.msQueryInitialId + " peptideMatchRank:" + spectrumMatch.peptideMatchRank)
      for (sm <- fragmentMatches.get) logger.debug("FragmentMatch " + sm.label + " : type:" + sm.`type`.toString() + " moz:" + sm.moz + " theoMoz:" + sm.calculatedMoz + " intensity:" + sm.intensity + " neutralLossMass:" + sm.neutralLossMass)
    } catch {
      case e: Exception => {
        logger.error("Error somewhere... ", e)
        throw e
      }
    }
  }

  @Test
  def testIonTable() {
    val peptideSequence = "GQVLAKPGTIK"
           // TODO add all the modifications (it should contain all the ptms, not just one !!)
      var ptms: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]()
      ptms += PTMFakeProvider.getPtmDefinition("Oxydation of M", 'M', PtmLocation.ANYWHERE).get
      val modificationMatches: Array[FragmentModificationMatch] = Array( // the name of a FragmentModificationMatch must match a ptm name in 'ptms: ArrayBuffer[PtmDefinition]' !
      //        new FragmentModificationMatch("deamidation of N and Q", true, 3),
//              new FragmentModificationMatch("Carbamidomethyl (C)", true, 7)
      )
      val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
      val neutralLosses: NeutralLosses = new NeutralLosses
      neutralLosses.addNeutralLoss(NeutralLossType.H2O, 6, 1)
      neutralLosses.addNeutralLoss(NeutralLossType.NH3, 5, 3)

      // create the fragment ion table
      val table = new FragmentIonTable(	peptideSequence,
//    		  							proteinAccessionNumbers,
//								        ptms.toArray,
								        modificationMatches,
								        currentFragmentIonTypes, // The list of currently selected fragment ion types
//								        neutralLosses.getNeutralLossMap) // The list of the currently selected neutral loss types
								        neutralLosses.get) // The list of the currently selected neutral loss types
//								        2)
    
    logger.debug(table.toString)
  }
}
