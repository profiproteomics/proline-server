package fr.proline.module.parser.omssa

import com.weiglewilczek.slf4s.Logging
import scala.collection.mutable.ArrayBuffer
import java.io.File
import javax.xml.stream.XMLInputFactory
import org.codehaus.staxmate.in.{ SMHierarchicCursor, SMInputCursor }
import org.codehaus.staxmate.SMInputFactory
import fr.proline.core.om.model.msi.{ Spectrum, InstrumentConfig }
//import fr.proline.repository.DatabaseContext

class OmssaListSpectrum(omxFile: File, peaklistId: Long, instrumentConfig: InstrumentConfig, onEachSpectrum: Spectrum => Unit) extends Logging {

  _parseOmxFile()

  private def _parseOmxFile() {
    
    // prepare variable
    var allSpectraParsed: Boolean = false
    // open an input factory
//    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
    // get the root cursor
//    val MSSearch: SMHierarchicCursor = inf.rootElementCursor(omxFile)
    val MSSearch: SMHierarchicCursor = OmssaReadFile.openOmxFile(new SMInputFactory(XMLInputFactory.newInstance()), omxFile)
    MSSearch.setElementTracking(SMInputCursor.Tracking.PARENTS)
    MSSearch.advance
    // advance the cursor to the first child of <MSSearch>
    val MSSearch_firstChild = MSSearch.childElementCursor().advance()
    while (MSSearch_firstChild.getCurrEvent() != null && !allSpectraParsed) {
      MSSearch_firstChild.getPrefixedName() match {
        case "MSSearch_request" => // this part contains the spectra and the settings
          // advance the cursor to the first child of MSSearch_request (MSRequest)
          val MSRequest = MSSearch_firstChild.childElementCursor().advance()
          // advance the cursor to the first child of MSRequest (which appears only once)
          val MSRequest_firstChild = MSRequest.childElementCursor().advance()
          while (MSRequest_firstChild.getCurrEvent() != null && !allSpectraParsed) {
            MSRequest_firstChild.getPrefixedName() match {
              case "MSRequest_spectra" => // list the spectra given for the omssa search
                // advance the cursor to the first child of MSRequest_spectra (MSSpectrumset)
                val MSSpectrumset = MSRequest_firstChild.childElementCursor().advance()
                // advance the cursor to the first child of MSSpectrumset (which appears only once)
                val MSSpectrumset_firstChild = MSSpectrumset.childElementCursor().advance()
                while (MSSpectrumset_firstChild.getCurrEvent() != null && !allSpectraParsed) {
                  MSSpectrumset_firstChild.getPrefixedName() match {
                    case "MSSpectrum" => // this contains a full spectrum
                      // preparing the values to save for the current spectrum
                      var spectrumNumber = 0
                      var spectrumTitle = ""
                      var precursorMoz: Double = 0
                      var precursorCharge = 0
                      val mozList = new ArrayBuffer[Double]
                      val intensityList = new ArrayBuffer[Float]
                      var scale: Float = 1 // the fragment intensities must be divided by this value
                      // advance the cursor to the first child of MSSpectrum
                      val MSSpectrum_children = MSSpectrumset_firstChild.childElementCursor().advance()
                      // loop on the children of MSSpectrum
                      while (MSSpectrum_children.getCurrEvent() != null) {
                        MSSpectrum_children.getPrefixedName() match {
                          case "MSSpectrum_number" => spectrumNumber = MSSpectrum_children.collectDescendantText(false).toInt
                          case "MSSpectrum_charge" => precursorCharge = MSSpectrum_children.childElementCursor().advance().collectDescendantText(false).toInt
                          case "MSSpectrum_precursormz" => precursorMoz = MSSpectrum_children.collectDescendantText(false).toDouble / 1000
                          case "MSSpectrum_mz" =>
                            val MSSpectrum_mz_E = MSSpectrum_children.childElementCursor().advance()
                            while (MSSpectrum_mz_E.getCurrEvent() != null) {
                              mozList += MSSpectrum_mz_E.collectDescendantText(false).toDouble / 1000
                              MSSpectrum_mz_E.advance()
                            }
                          case "MSSpectrum_abundance" => // fragment intensities
                            val MSSpectrum_abundance_E = MSSpectrum_children.childElementCursor().advance()
                            while (MSSpectrum_abundance_E.getCurrEvent() != null) {
                              intensityList += MSSpectrum_abundance_E.collectDescendantText(false).toFloat
                              MSSpectrum_abundance_E.advance()
                            }
                          case "MSSpectrum_iscale" => 
                            scale = MSSpectrum_children.collectDescendantText(false).toFloat
                          case "MSSpectrum_ids" => spectrumTitle = MSSpectrum_children.childElementCursor().advance().collectDescendantText(false).replace('\\', '/')
                          case _ =>
                        }
                        MSSpectrum_children.advance()
                      }
                      if (spectrumTitle == "") spectrumTitle = " Cmpd " + spectrumNumber + ", +MSn(" + precursorMoz + "), ? min"
                      // correction of the intensities (omssa stores the intensities as integers and indicates at the end the value used to transform floats to ints)
                      var intensities: Array[Float] = new Array[Float](intensityList.length)
                      for (i <- 0 until intensityList.length) { intensities(i) = intensityList(i) / scale }
                      // creating the spectrum
                      val instConfigId = if (instrumentConfig != null) instrumentConfig.id else 0
                      val spec = new Spectrum(id = Spectrum.generateNewId,
                        title = spectrumTitle,
                        precursorMoz = precursorMoz,
                        precursorCharge = precursorCharge,
                        mozList = Some(mozList.toArray),
                        intensityList = Some(intensities),
                        peaksCount = mozList.length,
                        instrumentConfigId = instConfigId,
                        peaklistId = peaklistId)
                      // calling the function that will process the spectrum
                      onEachSpectrum(spec)
                    case _ => // this case should never occur
                  }
                  MSSpectrumset_firstChild.advance()
                }
                allSpectraParsed = true
              case _ =>
            }
            MSRequest_firstChild.advance()
          }
        case _ =>
      }
      MSSearch_firstChild.advance()
    }
    MSSearch.getStreamReader().closeCompletely()
    logger.info("OmssaListSpectrum ended correctly")
    ()
  }
}