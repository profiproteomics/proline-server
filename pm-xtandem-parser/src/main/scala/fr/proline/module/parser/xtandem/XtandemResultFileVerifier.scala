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

import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.Enzyme
import fr.proline.core.om.provider.msi.IResultFileVerifier
import com.typesafe.scalalogging.LazyLogging

class XtandemResultFileVerifier extends IResultFileVerifier with LazyLogging {

  var parserContext: ProviderDecoratedExecutionContext = _
  
  def getPtmDefinitions(fileLocation: File, importProperties: Map[String, Any]): Seq[PtmDefinition] = {
    // Requirements
    require(parserContext != null,"getPtmDefinitions - No parser context found. Use setParserContext(parserContext: ProviderDecoratedExecutionContext)")
    
//    logger.debug("IY - XtandemResultFileVerifier - getPtmDefinitions parserContext = " + parserContext)
    
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var parseur: SAXParser = factory.newSAXParser()
    var manager = new XtandemPtmVerifier(parserContext)
    parseur.parse(fileLocation, manager)
    
    var ptmDefinitions : ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
    ptmDefinitions = manager.fixedPtmDefs ++ manager.variablePtmDefs
    
    ptmDefinitions
  }
  
  // can be used to verify that the provider handle this kind of file (ex: MSMS search, error tolerant search, n15 search, PMF, ...)  
  def isValid(fileLocation: File, importProperties: Map[String, Any]): Boolean = {
    return true
  }

  def getEnzyme(fileLocation: File, importProperties: Map[String, Any]): Array[Enzyme] = {
    // Requirements
    require(parserContext != null,"getEnzyme - No parser context found. Use setParserContext(parserContext: ProviderDecoratedExecutionContext)")
    
    val factory: SAXParserFactory = SAXParserFactory.newInstance()
    var parseur: SAXParser = factory.newSAXParser()
    var manager = new XTandemEnzymeVerifier(parserContext)
    parseur.parse(fileLocation, manager)
    manager.usedEnzymes.toArray
  }
  
  def setParserContext(parserContext: ProviderDecoratedExecutionContext) {
    this.parserContext = parserContext
//    logger.debug("IY - XtandemResultFileVerifier - setParserContext parserContext = " + parserContext)
  }
}
