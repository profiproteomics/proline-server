/**
 * XTandemResultFileVerifier.scala
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Following tests are done : existance of PTM, existance of Enzyme
 */

package fr.proline.module.parser.xtandem

import java.io.File
import scala.collection.mutable.ArrayBuffer
import _root_.fr.proline.core.om.provider.ProviderDecoratedExecutionContext

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import _root_.fr.proline.core.om.model.msi.PtmDefinition
import _root_.fr.proline.core.om.model.msi.Enzyme
import _root_.fr.proline.core.om.provider.msi.IResultFileVerifier
import com.typesafe.scalalogging.slf4j.Logging

class XtandemResultFileVerifier(  val xtandemFilePath : String, 
                                  val parserContext: ProviderDecoratedExecutionContext
                                  ) extends IResultFileVerifier with Logging {

  // returns PtmDefinitions referenced by the specified file
  def getPtmDefinitions(fileLocation: File, importProperties: Map[String, Any]): Seq[PtmDefinition] = {
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var parseur: SAXParser = factory.newSAXParser()
    var manager = new XtandemPtmVerifier(parserContext)
    parseur.parse(new File(xtandemFilePath), manager)
    
    var ptmDefinitions : ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
    ptmDefinitions = manager.fixedPtmDefs ++ manager.variablePtmDefs
    
    ptmDefinitions
  }
  
  // can be used to verify that the provider handle this kind of file (ex: MSMS search, error tolerant search, n15 search, PMF, ...)  
  def isValid(fileLocation: File, importProperties: Map[String, Any]): Boolean = {
    return true
  }

  def getEnzyme(fileLocation: File, importProperties: Map[String, Any]): Array[Enzyme] = {
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var parseur: SAXParser = factory.newSAXParser()
    var manager = new XTandemEnzymeVerifier(parserContext)
    val file = new File(xtandemFilePath)
    parseur.parse(file, manager)
    manager.usedEnzymes.toArray
  }
}
