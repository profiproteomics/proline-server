package fr.proline.module.parser.omssa

import java.io.File

import org.codehaus.staxmate.SMInputFactory
import org.codehaus.staxmate.in.SMHierarchicCursor
import org.codehaus.staxmate.in.SMInputCursor
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.slf4j.Logging
import javax.xml.stream.XMLInputFactory

class OmssaFilePreloader(omxFile: File) extends Logging {
  private var fileParsed = false
  private var mozScaleValue: Int = 100 // default value, according to the OMSSA.xsd file
  def getMozScaleValue: Int = mozScaleValue
  private var nbSequencesInFastaFile: Int = 0
  def getNbSequencesInFastaFile: Int = nbSequencesInFastaFile
  private var currentProteinId: Option[Int] = None
  private val proteinInternalIdToSequence = new HashMap[Int, String]()
  def getProteinSequencesByInternalIds = proteinInternalIdToSequence.toMap

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
        // comment or uncomment the following line to read (or not) the protein sequences
//        case "MSResponse_bioseqs"   => readSMInput(MSResponse_children.childElementCursor().advance())
        case _                      =>
      }
      MSResponse_children.advance()
    }
    fileParsed = true
  } catch {
    case e: Exception => logger.error("OmssaFilePreloader failed", e)
  } finally {
    MSSearch.getStreamReader().closeCompletely()
  }
  logger.debug("Searched value is mozScaleValue, found value is " + mozScaleValue)
  logger.debug("Searched value is nbSequencesInFastaFile, found value is " + nbSequencesInFastaFile)

  private def readSMInput(node: SMInputCursor) {
    if (node.getCurrEvent == null) {
      return
    }
    if (node.getCurrEvent.isTextualEvent) {
      val text = node.getText.trim
      if(node.getPathDesc().contains("MSBioseq_oid")) {
        currentProteinId = Some(Integer.parseInt(text))
      } else if(node.getPathDesc().contains("/NCBIstdaa[e0]")) {
        if(currentProteinId.isDefined) {
        	proteinInternalIdToSequence.put(currentProteinId.get, decodeNCBIstdaa(text))
        } else {
          logger.warn("Protein sequence found without a valid internal id")
        }
      }
    } else readSMInput(node.childCursor.advance) // son
    readSMInput(node.advance) // bro
  }
  private def decodeNCBIstdaa(encodedSequence: String): String = {
    // Convert from ncbiaa string to amino acid sequence
    // The conversion code has been found at http://www.ncbi.nlm.nih.gov/IEB/ToolBox/C_DOC/lxr/source/data/seqcode.prt
    // It has only been converted into hexadecimal to match the good values
    val code = Map[String, String]("00" -> "-", "01" -> "A", "02" -> "B", "03" -> "C", "04" -> "D", "05" -> "E", "06" -> "F", "07" -> "G", "08" -> "H", "09" -> "I", "0A" -> "K", "0B" -> "L", "0C" -> "M", "0D" -> "N", "0E" -> "P", "0F" -> "Q", "10" -> "R", "11" -> "S", "12" -> "T", "13" -> "V", "14" -> "W", "15" -> "X", "16" -> "Y", "17" -> "Z", "18" -> "U", "19" -> "*", "1A" -> "O", "1B" -> "J")
    var sequence = ""
    var i = 0
    while (i < encodedSequence.length) {
      sequence += code.get(encodedSequence.substring(i, i + 2)).getOrElse("X")
      i = i + 2
    }
    sequence
  }
}