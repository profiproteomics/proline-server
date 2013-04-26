package fr.proline.module.parser.omssa

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import java.io.File
import javax.xml.stream.XMLInputFactory
import org.codehaus.staxmate.in.{ SMHierarchicCursor, SMInputCursor }
import org.codehaus.staxmate.SMInputFactory
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.proline.core.om.model.msi.{ Spectrum, SpectrumMatch, SearchSettings }
import java.lang.Math.abs
import java.util.ArrayList
import com.compomics.util.experiment.biology.Ion
import com.compomics.util.experiment.biology.ions.PeptideFragmentIon
//import fr.proline.context.DatabaseConnectionContext

import com.weiglewilczek.slf4s.Logging

/**
 * @author abu
 *
 */
class OmssaSpectrumMatcher(omxFile: File, wantDecoy: Boolean, spectrumList: ArrayBuffer[Spectrum], omssaLoader: OmssaMandatoryFilesLoader, searchSettings: SearchSettings, currentFileMzScale: Int, onEachSpectrumMatch: SpectrumMatch => Unit) extends Logging {

  _parseOmxFile()

  private def _parseOmxFile() {
    // open an input factory
    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
    // get the root cursor and advance to the MSSearch_response element
    val MSSearch: SMHierarchicCursor = inf.rootElementCursor(omxFile)
    MSSearch.setElementTracking(SMInputCursor.Tracking.PARENTS)
    MSSearch.advance // MSSearch
    val MSSearch_request = MSSearch.childElementCursor().advance()
    val MSSearch_response = MSSearch_request.advance()
    if (MSSearch_response.getPrefixedName() != "MSSearch_response") {
      // this should never happen, because the exception would have already been throwed (in OmssaReadFile)
      throw new UnexpectedOmxFormatException("MSSearch_response", MSSearch_response.getPrefixedName())
    }
    val fragmentationTableGenerator = new OmssaFragmentationTable()
    val MSResponse = MSSearch_response.childElementCursor().advance()
    val MSResponse_hitsets = MSResponse.childElementCursor().advance()
    val MSHitSets = MSResponse_hitsets.childElementCursor().advance()
    while (MSHitSets.getCurrEvent() != null) {
      val currentSpectrum = spectrumList(0)
      MSHitSets.getPrefixedName() match {
        // for each MSHitSet
        case "MSHitSet" =>
          // read the MSHitSet children
          val MSHitSet = MSHitSets.childElementCursor().advance()
          var msQueryId: Int = 0
          while (MSHitSet.getCurrEvent() != null) {
            MSHitSet.getPrefixedName() match {
              case "MSHitSet_number" => msQueryId = MSHitSet.collectDescendantText(false).toInt
              case "MSHitSet_hits" =>
                val MSHits = MSHitSet.childElementCursor().advance()
                // for each PeptideMatch (MSHits)
                while (MSHits.getCurrEvent() != null) {
                  val chargePerSerie: HashMap[String, Int] = new HashMap[String, Int]
                  val localFragmentMatches: ArrayBuffer[LocalFragmentMatch] = new ArrayBuffer[LocalFragmentMatch]
                  MSHits.getPrefixedName() match {
                    case "MSHits" =>
                      val MSHit = MSHits.childElementCursor().advance()
                      while (MSHit.getCurrEvent() != null) {
                        MSHit.getPrefixedName() match {
                          case "MSHits_pephits" =>
                            // get the proteins (MSPepHit_accession)
                            val MSPepHit = MSHit.childElementCursor().advance()
                            val MSPepHit_items = MSPepHit.childElementCursor().advance()
                            var proteinAccessionNumber: String = ""
                            var proteinIsDecoy: Boolean = false
                            while (MSPepHit_items.getCurrEvent() != null) {
                              MSPepHit_items.getPrefixedName() match {
                                case "MSPepHit_accession" => fragmentationTableGenerator.addProteinAccessionNumber(MSPepHit_items.collectDescendantText(false))
                                case "MSPepHit_defline" => fragmentationTableGenerator.addProteinDefinition(MSPepHit_items.collectDescendantText(false))
                                //                                case "MSPepHit_accession" => proteinAccessionNumber = MSPepHit_items.collectDescendantText(false)
                                //                                case "MSPepHit_defline" => proteinIsDecoy = MSPepHit_items.collectDescendantText(false).matches("^Reverse sequence, was .*")
                                case _ =>
                              }
                              MSPepHit_items.advance()
                            }
                          case "MSHits_mzhits" =>
                            // loop for each MSMZHit, record the charge for each ion type
                            val MSMZHits = MSHit.childElementCursor().advance()
                            while (MSMZHits.getCurrEvent() != null) {
                              var currentIonType = -1
                              var currentCharge = -1
                              var currentMz: Double = -1
                              var currentPosition = -1
                              val MSMZHit = MSMZHits.childElementCursor().advance()
                              while (MSMZHit.getCurrEvent() != null) {
                                MSMZHit.getPrefixedName() match {
                                  case "MSMZHit_ion" => currentIonType = MSMZHit.childElementCursor().advance().collectDescendantText(false).toInt
                                  case "MSMZHit_charge" => currentCharge = MSMZHit.collectDescendantText(false).toInt
                                  case "MSMZHit_number" => currentPosition = MSMZHit.collectDescendantText(false).toInt + 1
                                  case "MSMZHit_mz" => currentMz = MSMZHit.collectDescendantText(false).toInt / currentFileMzScale
                                  case _ =>
                                }
                                MSMZHit.advance()
                              }
                              if (currentIonType == -1 || !omssaLoader.ionTypes.contains(currentIonType) || omssaLoader.ionTypes.get(currentIonType) == None) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (ion type unknown)") }
                              else if (currentCharge == -1) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (charge unknown)") }
                              else if (currentMz == -1) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (Mz unknown)") }
                              else if (currentPosition == -1) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (position unknown)") }
                              else {
                                chargePerSerie.put(omssaLoader.ionTypes.get(currentIonType).get, currentCharge)
                                localFragmentMatches += new LocalFragmentMatch(omssaLoader.ionTypes.get(currentIonType).get, currentCharge, currentPosition, currentMz)
                              }
                              MSMZHits.advance()
                            }
                          case "MSHits_mods" =>
                            // get all the ptms
                            val MSModHit = MSHit.childElementCursor().advance()
                            while (MSModHit.getCurrEvent() != null) {
                              MSModHit.getPrefixedName() match {
                                case "MSModHit" =>
                                  val MSModHit_items = MSModHit.childElementCursor().advance()
                                  var modificationSite = -1
                                  var ptmId = -1
                                  while (MSModHit_items.getCurrEvent() != null) {
                                    MSModHit_items.getPrefixedName() match {
                                      case "MSModHit_site" => modificationSite = MSModHit_items.collectDescendantText(false).toInt
                                      case "MSModHit_modtype" => ptmId = MSModHit_items.childElementCursor().advance().collectDescendantText(false).toInt
                                      case _ =>
                                    }
                                    // get the ptm from the list read in the mandatory files, check if the ptm is variable of fixed in the search settings
                                    val ptm = omssaLoader.ptmDefinitions.get(ptmId)
                                    if (ptm != None) {
                                      val isVariable = searchSettings.variablePtmDefs.contains(ptm.get)
                                      fragmentationTableGenerator.addModificationMatch(ptm.get.names.shortName, isVariable, modificationSite)
                                    }
                                    MSModHit_items.advance()
                                  }
                                case _ =>
                              }
                              MSModHit.advance()
                            }
                          // get the peptide sequence
                          case "MSHits_pepstring" => fragmentationTableGenerator.setPeptideSequence(MSHit.collectDescendantText(false))
                          case _ =>
                        }
                        MSHit.advance()
                      }
                      try {
                        // filter with wantDecoy value (if true, first protein must be decoy ; if false, first protein must be target)
                        if (fragmentationTableGenerator.matchesToTargetOrDecoyProteins(wantDecoy)) {
                          // get the fragment ion list from the peptide sequence, the protein accession numbers and the ptms
                          val fragmentIons: Array[PeptideFragmentIon] = fragmentationTableGenerator.getFragmentIons()
                          // in this list, separate the ions by series to create the fragmentationTable
                          val fragmentationTableMasses: HashMap[String, ArrayBuffer[Double]] = new HashMap[String, ArrayBuffer[Double]]
                          for (ion <- fragmentIons) {
                            if (fragmentationTableMasses.get(ion.getSubTypeAsString()) == None) { fragmentationTableMasses.put(ion.getSubTypeAsString(), new ArrayBuffer[Double]) }
                            fragmentationTableMasses.get(ion.getSubTypeAsString()).get += ion.getTheoreticMass()
                          }
                          val fragmentationTable: ArrayBuffer[TheoreticalFragmentSeries] = new ArrayBuffer[TheoreticalFragmentSeries]
                          for (item <- fragmentationTableMasses) {
                            var serie = item._1 // y++ for 'y series with charge=2'
                            // series not in the results (ie. c, a, x, z in a CID search) will have a simple label (just the letter)
                            for (i <- 0 until chargePerSerie.getOrElse(item._1, 0)) { serie += "+" }
                            fragmentationTable += new TheoreticalFragmentSeries(serie, item._2.toArray[Double])
                          }
                          // create the fragment matches
                          val fragmentMatches: ArrayBuffer[FragmentMatch] = new ArrayBuffer[FragmentMatch]
                          try {
                            for (fragmentMatch <- localFragmentMatches) {
                              fragmentMatches += fragmentMatch.getFragmentMatch(currentSpectrum, fragmentIons)
                            }
                          } catch {
                            case e => logger.warn("Fragment matches could not be generated for spectrum '" + currentSpectrum.title + "' (" + e.getMessage() + ")")
                          }
                          // call the onEachSpectrumMatch function with the spectrumMatch object in argument
                          onEachSpectrumMatch(
                            new SpectrumMatch(
                              msQueryInitialId = msQueryId,
                              fragmentationTable = fragmentationTable.toArray[TheoreticalFragmentSeries],
                              fragmentMatches = fragmentMatches.toArray[FragmentMatch]))
                        }
                      } catch {
                        case e =>
                          logger.warn("Fragmentation table could not be generated for spectrum '" + currentSpectrum.title + "' (" + e.getMessage() + ")")
                          e.printStackTrace()
                      } finally {
                        // clear the temporary information corresponding to the current peptide match
                        fragmentationTableGenerator.reset()
                      }
                    case _ =>
                  }
                  MSHits.advance()
                }
              case _ =>
            }
            MSHitSet.advance()
          }
        case _ =>
      }
      // remove the corresponding spectrum from spectrumList
      spectrumList.remove(0)
      // advance to the next MSHitSet
      MSHitSets.advance()
    }

    // close the file
    logger.debug("Closing file")
    MSSearch.getStreamReader().closeCompletely()
    ()
  }

  /**
   * @author Alexandre Burel
   *
   *
   */
  private class LocalFragmentMatch(ionType: String, charge: Int, position: Int, moz: Double) {

    private val _ionType: String = ionType
    private val _charge: Int = charge
    private val _position: Int = position
    private val _moz: Double = moz
    private var _fragmentIons: Array[PeptideFragmentIon] = null
    private var _spectrum: Spectrum = null

    /**
     * @param spectrum The spectrum that gave the current ion
     * @param fragmentIons the fragmentation table
     * @return a FragmentMatch object calculated with the given values
     */
    def getFragmentMatch(spectrum: Spectrum, fragmentIons: Array[PeptideFragmentIon]): FragmentMatch = {
      _fragmentIons = fragmentIons
      _spectrum = spectrum
      return new FragmentMatch(label = _label,
        `type` = fr.proline.core.om.model.msi.FragmentMatchType.REGULAR.toString,
        moz = _moz,
        calculatedMoz = _calculatedMoz,
        intensity = _intensity,
        neutralLossMass = Some(_neutralLoss))
    }
    /**
     * label that represents the ion type, the position and the charge (for instance y(3)++ stands for 'third match for ion type y and doubly charged')
     */
    private lazy val _label: String = {
      var label = ionType + "(" + position + ")"
      for (i <- 0 until charge) { label += "+" }
      label
    }
    /**
     * theoretical moz corresponding to the ion with the current position in the current ion type
     */
    private lazy val _calculatedMoz: Double = {
      var theoreticalMoz: Double = 0
      for (ion <- _fragmentIons) {
        if (ionType == ion.getSubTypeAsString() && ion.getNumber() == position) { theoreticalMoz = ion.getTheoreticMass() }
      }
      theoreticalMoz
    }
    /**
     * neutral loss of the current ion, as calculated in the fragmentation table
     */
    private lazy val _neutralLoss: Double = {
      var neutralLoss: Double = 0
      for (ion <- _fragmentIons) {
        //    	logger.debug("@@@ "+ion.getName()+" neutralLoss="+ion.getNeutralLossesAsString())
        if (ionType == ion.getSubTypeAsString() && ion.getNumber() == position) {
          val neutralLosses = ion.getNeutralLosses()
          for (i <- 0 until neutralLosses.size()) { neutralLoss += neutralLosses.get(i).mass } // TODO check if this is correct
          //          logger.debug("@@@ neutralLoss="+neutralLoss)
        }
      }
      neutralLoss
    }
    /**
     * intensity corresponding to the given Moz, searched in the given spectrum
     */
    private lazy val _intensity: Float = {
      var intensity: Float = 0
      var lowestMozDifference: Double = -1
      var idOfTheClosestMoz: Int = -1
      // read all the masses in the spectrum
      if (_spectrum.mozList != None && _spectrum.intensityList != None) {
        val mozList = _spectrum.mozList.get
        for (i <- 0 until mozList.size) {
          // get the id of the closest to the given moz
          if (lowestMozDifference == -1 || (abs(this.moz - mozList(i)) < lowestMozDifference)) {
            lowestMozDifference = abs(this.moz - mozList(i))
            idOfTheClosestMoz = i
          }
        }
        intensity = _spectrum.intensityList.get(idOfTheClosestMoz)
      }
      if (intensity == -1) { logger.warn("Spectrum '" + _spectrum.title + "' could not return the corresponding intensity for the Mz '" + moz + "'") }
      intensity
    }
  }
}
