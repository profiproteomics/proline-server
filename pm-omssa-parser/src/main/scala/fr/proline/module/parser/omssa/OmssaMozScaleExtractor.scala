package fr.proline.module.parser.omssa

import com.weiglewilczek.slf4s.Logging
import java.io.File
import javax.xml.stream.XMLInputFactory
import org.codehaus.staxmate.in.{ SMHierarchicCursor, SMInputCursor }
import org.codehaus.staxmate.SMInputFactory

class OmssaMozScaleExtractor(omxFile: File) extends Logging {

  def mozScaleValue(): Int = readMozScaleValue
  private lazy val readMozScaleValue: Int = {
    logger.info("OmssaMozScaleExtractor starts")
    var mozScale: Int = 100 // default value, according to the OMSSA.xsd file
    var MSSearch: SMHierarchicCursor = null
    try {
	    // open an input factory
//	    val inf: SMInputFactory = new SMInputFactory(XMLInputFactory.newInstance())
//	    // get the root cursor and advance to the MSSearch_response element
//	    MSSearch = inf.rootElementCursor(omxFile)
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
	        case "MSResponse_scale" => mozScale = MSResponse_children.collectDescendantText(false).toInt
	        case _ =>
	      }
	      MSResponse_children.advance()
	    }

	    logger.info("OmssaMozScaleExtractor ended and returned "+mozScale)
    } catch {
    	case e => logger.warn("OmssaMozScaleExtractor failed" + e)
    } finally {
	    MSSearch.getStreamReader().closeCompletely()
    }
    // return value
    mozScale
  }

}
