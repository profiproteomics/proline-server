package fr.proline.module.parser.omssa

import java.io.File
import scala.collection.mutable.ArrayBuffer
import org.codehaus.staxmate.SMInputFactory
import org.codehaus.staxmate.in.SMHierarchicCursor
import org.codehaus.staxmate.in.SMInputCursor
import com.typesafe.scalalogging.LazyLogging
import fr.proline.core.om.model.msi.SearchSettings
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.module.fragment_match.FragmentIonTable
import fr.proline.module.fragment_match.FragmentIons
//import fr.proline.module.fragment_match.FragmentMatchManager
import javax.xml.stream.XMLInputFactory
import fr.proline.core.om.model.msi.Peptide
import fr.proline.core.om.model.msi.LocatedPtm

/**
 * @author abu
 *
 */
class OmssaSpectrumMatcher(omxFile: File, 
						   wantDecoy: Boolean, 
						   spectrumList: ArrayBuffer[Spectrum], 
						   omssaLoader: OmssaMandatoryFilesLoader, 
						   searchSettings: SearchSettings, 
						   currentFileMzScale: Int, 
						   onEachSpectrumMatch: SpectrumMatch => Unit) extends LazyLogging {

  _parseOmxFile()

  private def _parseOmxFile() {
    // get the root cursor and advance to the MSSearch_response element
    val MSSearch: SMHierarchicCursor = OmssaReadFile.openOmxFile(new SMInputFactory(XMLInputFactory.newInstance()), omxFile)
    MSSearch.setElementTracking(SMInputCursor.Tracking.PARENTS)
    MSSearch.advance // MSSearch
    val MSSearch_request = MSSearch.childElementCursor().advance()
    val MSSearch_response = MSSearch_request.advance()
    if (MSSearch_response.getPrefixedName() != "MSSearch_response") {
      // this should never happen, because the exception would have already been throwed (in OmssaReadFile)
      throw new UnexpectedOmxFormatException("MSSearch_response", MSSearch_response.getPrefixedName())
    }
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
                var peptideMatchRank: Int = 0
                val MSHits = MSHitSet.childElementCursor().advance()
                // for each PeptideMatch (MSHits)
                while (MSHits.getCurrEvent() != null) {
                  val currentFragmentIonTypes = new FragmentIons()
                  MSHits.getPrefixedName() match {
                    case "MSHits" =>
                      peptideMatchRank += 1
                      var peptideSequence = ""
                      var calculatedMass: Double = 0
                      val proteinAccessionNumbers = new ArrayBuffer[String]
//                      val fragmentMatches = new FragmentMatchManager
                      val ptms = new ArrayBuffer[LocatedPtm]
                      val MSHit = MSHits.childElementCursor().advance()
                      while (MSHit.getCurrEvent() != null) {
                        MSHit.getPrefixedName() match {
                          case "MSHits_pephits" =>
                            // get the proteins (MSPepHit_accession)
                            val MSPepHit = MSHit.childElementCursor().advance()
                            val MSPepHit_items = MSPepHit.childElementCursor().advance()
                            var proteinIsDecoy: Boolean = false
                            while (MSPepHit_items.getCurrEvent() != null) {
                              MSPepHit_items.getPrefixedName() match {
                                case "MSPepHit_accession" => proteinAccessionNumbers += MSPepHit_items.collectDescendantText(false)
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
                                  case "MSMZHit_mz" => currentMz = MSMZHit.collectDescendantText(false).toDouble / currentFileMzScale.toDouble
//                                  case "MSMZHit_moreion" => {
//                                    // untested case, because no omssa file with this area could be generated
//                                    val MSIon = MSMZHit.childElementCursor().advance()
//                                    while (MSIon.getCurrEvent() != null) {
//                                      MSIon.getPrefixedName() match {
//	                                      case "MSIon_neutralloss" => {
//	                                        val node = MSIon.childElementCursor().advance()
//	                                        if(node.getAttrCount() > 0) {
//	                                          node.getAttrValue(0).toInt match {
//	                                            case 0 => /* water => mass - 18 Da*/
//	                                            case 1 => /* ammonia => mass - 17 Da*/
//	                                            case _ => /* should never happen */
//	                                          }
//	                                        }
//	                                      }
//	                                      case "MSIonIsotopicType" => {
//	                                        val node = MSIon.childElementCursor().advance()
//	                                        if(node.getAttrCount() > 0) {
//	                                          node.getAttrValue(0).toInt match {
//	                                            case 0 => /* monoisotopic : no c13s in molecule */
//	                                            case 1 => /* c13 : one c13 in molecule */
//	                                            case 2 => /* c13two : two c13s in molecule */
//	                                            case 3 => /* c13three : three c13s in molecule */
//	                                            case 4 => /* c13four : four c13s in molecule */
//	                                            case _ => /* should never happen */
//	                                          }
//	                                        }
//	                                      }
//	                                      case "MSImmonium" => {
//	                                        val MSImmonium = MSMZHit.childElementCursor().advance()
//		                                    while (MSImmonium.getCurrEvent() != null) {
//		                                      MSImmonium.getPrefixedName() match {
//			                                      case "MSImmonium_parent" => /* parent amino acid*/
//			                                      case "MSImmonium_product" => /* product ion code */
//			                                      case _ => /* should never happen */
//		                                      }
//		                                      MSImmonium.advance()
//		                                    }
//	                                      }
//	                                      case "MSIon_internal" => val internalSequence = MSIon.collectDescendantText(false)
//	                                      case _ =>
//                                      }
//                                      MSIon.advance()
//                                    }
//                                  }
                                  case _ =>
                                }
                                MSMZHit.advance()
                              }
                              if (currentIonType == -1 || !omssaLoader.ionTypes.contains(currentIonType) || omssaLoader.ionTypes.get(currentIonType) == None) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (ion type unknown)") }
                              else if (currentCharge == -1) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (charge unknown)") }
                              else if (currentMz == -1) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (Mz unknown)") }
                              else if (currentPosition == -1) { logger.warn("FragmentMatch error on spectrum '" + currentSpectrum.title + "' (position unknown)") }
                              else {
                                currentFragmentIonTypes.setIonTypeAndCharge(omssaLoader.ionTypes.get(currentIonType).get, currentCharge)
//                                fragmentMatches.add(omssaLoader.ionTypes.get(currentIonType).get, currentCharge, currentPosition, currentMz)
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
                                    MSModHit_items.advance()
                                  }
                                // get the ptm from the list read in the mandatory files, check if the ptm is variable of fixed in the search settings
                                val ptm = omssaLoader.getPtmDefinition(ptmId, peptideSequence.charAt(modificationSite))
                                if (ptm != None) {
                                  val isVariable = searchSettings.variablePtmDefs.contains(ptm.get)
                                  ptm.get.ptmEvidences.foreach(e => {
                                    ptms += new LocatedPtm(definition = ptm.get, seqPosition = modificationSite, monoMass = e.monoMass, averageMass = e.averageMass, composition = e.composition)
                                  })
                                }
                                case _ =>
                              }
                              MSModHit.advance()
                            }
                          // get the peptide sequence
                          case "MSHits_pepstring" => peptideSequence = MSHit.collectDescendantText(false)
                          case "MSHits_theomass" => calculatedMass = MSHit.collectDescendantText(false).toDouble / currentFileMzScale.toDouble
                          case _ =>
                        }
                        MSHit.advance()
                      }
                      try {
                          // create the fragmentation table
                        var ptmsNL: Option[Map[LocatedPtm, Double]] = None
                        if(ptms.exists(_.definition.neutralLosses.size > 1)) {
                          ptmsNL = Some(ptms.filter(_.definition.neutralLosses.size > 1).map(ptm => ptm -> ptm.definition.neutralLosses(1).monoMass).toMap)
                        }
                          val table = new FragmentIonTable(
                              new Peptide(-1, peptideSequence, "", ptms.toArray, calculatedMass), 
                              currentFragmentIonTypes,
                              ptmNeutralLosses = ptmsNL).get
//                              ptmNeutralLosses = Some(ptms.filter(_.definition.neutralLosses.size > 1).map(ptm => ptm -> ptm.definition.neutralLosses(1).monoMass).toMap)).get
                          // call the onEachSpectrumMatch function with the spectrumMatch object in argument
//                          onEachSpectrumMatch(new SpectrumMatch(msQueryId, peptideMatchRank, table, fragmentMatches.get(table, currentSpectrum)))
                      } catch {
                        case ex: Exception => 
                          logger.error("Fragmentation table could not be generated for spectrum '" + currentSpectrum.title + "'", ex)
                          throw ex
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

}
