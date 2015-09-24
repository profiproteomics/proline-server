package fr.proline.module.parser.omssa

//import java.io.File
//import org.codehaus.staxmate.SMInputFactory
//import org.codehaus.staxmate.in.SMHierarchicCursor
//import org.codehaus.staxmate.in.SMInputCursor
//import scala.collection.mutable.HashMap
//import com.typesafe.scalalogging.slf4j.Logging
//import javax.xml.stream.XMLInputFactory
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.HashMap
import java.io.File
import java.io.FileReader
import java.io.BufferedReader
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStreamReader
import org.apache.commons.compress.compressors.CompressorInputStream
import org.apache.commons.compress.compressors.CompressorStreamFactory

class OmssaFilePreloader(omxFile: File) extends LazyLogging {
  
  // prepare variables
  var enzymeId: Int = -1
  var mozScaleValue: Int = -1
  var nbSequencesInFastaFile: Int = -1
  private var currentProteinId: Int = -1
  val proteinInternalIdToSequence = new HashMap[Int, String]()

  // prepare regex variables
  private val rEnzyme = """<MSEnzymes>(\d+)</MSEnzymes>""".r
  private val rScale = """<MSResponse_scale>(\d+)</MSResponse_scale>""".r
  private val rDbVersion = """<MSResponse_dbversion>(\d+)</MSResponse_dbversion>""".r
  private val rProteinId = """<MSBioseq_oid>(\d+)</MSBioseq_oid>""".r
  private val rProteinSeq = """<NCBIstdaa>(.+)</NCBIstdaa>""".r
  
  // reading file
  logger.info("OmssaFilePreloader starts")
  val bfr = {
    if(omxFile.getName().endsWith(".bz2")) {
      val fis: FileInputStream = new FileInputStream(omxFile)
      val bis: BufferedInputStream = new BufferedInputStream(fis)
      val input: CompressorInputStream = new CompressorStreamFactory().createCompressorInputStream(bis)
      new BufferedReader(new InputStreamReader(input))
    } else {
      new BufferedReader(new FileReader(omxFile))
    }
  }
  var line = bfr.readLine()
  var keepReading = true
  while((line != null) && keepReading) {
    if(line.contains("<MSEnzymes>")) {
      rEnzyme.findAllIn(line).matchData.foreach(m => enzymeId = m.group(1).toInt)
    } else if(line.contains("<MSResponse_scale>")) {
      rScale.findAllIn(line).matchData.foreach(m => mozScaleValue = m.group(1).toInt)
    } else if(line.contains("<MSResponse_dbversion>")) {
      rDbVersion.findAllIn(line).matchData.foreach(m => nbSequencesInFastaFile = m.group(1).toInt)
    } else if(line.contains("<MSBioseq_oid>")) {
      rProteinId.findAllIn(line).matchData.foreach(m => currentProteinId = m.group(1).toInt)
    } else if(line.contains("<NCBIstdaa>")) {
      rProteinSeq.findAllIn(line).matchData.foreach(m => proteinInternalIdToSequence.put(currentProteinId, decodeNCBIstdaa(m.group(1))))
    }
    // comment the following line to extract the protein sequences
    if(enzymeId != -1 && mozScaleValue != -1 && nbSequencesInFastaFile != -1) keepReading = false
    line = bfr.readLine()
  }
  bfr.close
  logger.debug("Searched value is mozScaleValue, found value is " + mozScaleValue)
  logger.debug("Searched value is nbSequencesInFastaFile, found value is " + nbSequencesInFastaFile)
  
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
//    val MSResponse = MSSearch_response.childElementCursor().advance()
//    val MSResponse_children = MSResponse.childElementCursor().advance()
//    while (MSResponse_children.getCurrEvent() != null) {
//      MSResponse_children.getPrefixedName() match {
//        case "MSResponse_scale"     => mozScaleValue = MSResponse_children.collectDescendantText(false).toInt
//        case "MSResponse_dbversion" => nbSequencesInFastaFile = MSResponse_children.collectDescendantText(false).toInt
//        // comment or uncomment the following line to read (or not) the protein sequences
////        case "MSResponse_bioseqs"   => readSMInput(MSResponse_children.childElementCursor().advance())
//        case _                      =>
//      }
//      MSResponse_children.advance()
//    }
//    fileParsed = true
//  } catch {
//    case e: Exception => logger.error("OmssaFilePreloader failed", e)
//  } finally {
//    MSSearch.getStreamReader().closeCompletely()
//  }
//  logger.debug("Searched value is mozScaleValue, found value is " + mozScaleValue)
//  logger.debug("Searched value is nbSequencesInFastaFile, found value is " + nbSequencesInFastaFile)
//
//  private def readSMInput(node: SMInputCursor) {
//    if (node.getCurrEvent == null) {
//      return
//    }
//    if (node.getCurrEvent.isTextualEvent) {
//      val text = node.getText.trim
//      if(node.getPathDesc().contains("MSBioseq_oid")) {
//        currentProteinId = Some(Integer.parseInt(text))
//      } else if(node.getPathDesc().contains("/NCBIstdaa[e0]")) {
//        if(currentProteinId.isDefined) {
//        	proteinInternalIdToSequence.put(currentProteinId.get, decodeNCBIstdaa(text))
//        } else {
//          logger.warn("Protein sequence found without a valid internal id")
//        }
//      }
//    } else readSMInput(node.childCursor.advance) // son
//    readSMInput(node.advance) // bro
//  }
//  private def decodeNCBIstdaa(encodedSequence: String): String = {
//    // Convert from ncbiaa string to amino acid sequence
//    // The conversion code has been found at http://www.ncbi.nlm.nih.gov/IEB/ToolBox/C_DOC/lxr/source/data/seqcode.prt
//    // It has only been converted into hexadecimal to match the good values
//    val code = Map[String, String]("00" -> "-", "01" -> "A", "02" -> "B", "03" -> "C", "04" -> "D", "05" -> "E", "06" -> "F", "07" -> "G", "08" -> "H", "09" -> "I", "0A" -> "K", "0B" -> "L", "0C" -> "M", "0D" -> "N", "0E" -> "P", "0F" -> "Q", "10" -> "R", "11" -> "S", "12" -> "T", "13" -> "V", "14" -> "W", "15" -> "X", "16" -> "Y", "17" -> "Z", "18" -> "U", "19" -> "*", "1A" -> "O", "1B" -> "J")
//    var sequence = ""
//    var i = 0
//    while (i < encodedSequence.length) {
//      sequence += code.get(encodedSequence.substring(i, i + 2)).getOrElse("X")
//      i = i + 2
//    }
//    sequence
//  }
//}