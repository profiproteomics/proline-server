package fr.proline.module.parser.omssa

import java.io.File

import org.codehaus.staxmate.SMInputFactory
import org.codehaus.staxmate.in.SMHierarchicCursor
import org.codehaus.staxmate.in.SMInputCursor

import com.weiglewilczek.slf4s.Logging

import javax.xml.stream.XMLInputFactory

class OmssaFilePreloader(omxFile: File) extends Logging {
  private var fileParsed = false
  private var mozScaleValue: Int = 100 // default value, according to the OMSSA.xsd file
  def getMozScaleValue: Int = mozScaleValue
  private var nbSequencesInFastaFile: Int = 0
  def getNbSequencesInFastaFile: Int = nbSequencesInFastaFile

  logger.info("OmssaFilePreloader starts")
  var MSSearch: SMHierarchicCursor = null
  try {
    // open an input factory
    MSSearch = OmssaReadFile.openOmxFile(new SMInputFactory(XMLInputFactory.newInstance()), omxFile)
    MSSearch.setElementTracking(SMInputCursor.Tracking.PARENTS)
    MSSearch.advance // MSSearch
    val MSSearch_request = MSSearch.childElementCursor().advance()
    val MSSearch_response = MSSearch_request.advance()
    if (MSSearch_response.getPrefixedName() != "MSSearch_response") {
      // this should never happen, because the exception would have already been throwed (in OmssaReadFile)
      throw new UnexpectedOmxFormatException("MSSearch_response", MSSearch_response.getPrefixedName())
    }
    val MSResponse = MSSearch_response.childElementCursor().advance()
    val MSResponse_children = MSResponse.childElementCursor().advance()
    while (MSResponse_children.getCurrEvent() != null) {
      MSResponse_children.getPrefixedName() match {
        case "MSResponse_scale"     => mozScaleValue = MSResponse_children.collectDescendantText(false).toInt
        case "MSResponse_dbversion" => nbSequencesInFastaFile = MSResponse_children.collectDescendantText(false).toInt
        case _                      =>
      }
      MSResponse_children.advance()
    }
    fileParsed = true
  } catch {
    case e: Exception => logger.warn("OmssaFilePreloader failed" + e)
  } finally {
    MSSearch.getStreamReader().closeCompletely()
  }
  logger.debug("Searched value is mozScaleValue, found value is " + mozScaleValue)
  logger.debug("Searched value is nbSequencesInFastaFile, found value is " + nbSequencesInFastaFile)

}