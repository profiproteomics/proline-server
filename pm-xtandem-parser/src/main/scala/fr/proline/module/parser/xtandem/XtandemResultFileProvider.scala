/**
 * XTandemResultFileVerifier.scala
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description 
 */

package fr.proline.module.parser.xtandem

import java.io.File
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.msi.{IResultFileVerifier, IResultFileProvider}
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import com.typesafe.scalalogging.slf4j.Logging

object XtandemResultFileProviderType {
    final val fileType: String = "xtandem.xml"
}

class XtandemResultFileProvider extends IResultFileProvider with Logging {

  var parserContext: ProviderDecoratedExecutionContext = _
  val fileType: String = XtandemResultFileProviderType.fileType

  def getResultFile( fileLocation: File, importProperties : Map[String, Any], parserContext: ProviderDecoratedExecutionContext ): IResultFile = {
    require(parserContext != null,"getResultFile - No parser context found. Use setParserContext(parserContext: ProviderDecoratedExecutionContext)")
    new XtandemParser(fileLocation, parserContext)
  }

  val resultFileProperties : Map[String, Class[_]] = null
  
  def getResultFileVerifier : IResultFileVerifier = {
    // Requirements
    require(parserContext != null,"getResultFileVerifier - No parser context found. Use setParserContext(parserContext: ProviderDecoratedExecutionContext)")
    val RFVerifier = new XtandemResultFileVerifier
    RFVerifier.setParserContext(parserContext)
    RFVerifier
  }
  
  override def setParserContext(parserContext: ProviderDecoratedExecutionContext) {
    this.parserContext = parserContext
//    logger.debug("IY - XtandemResultFileProvider - setParserContext parserContext = " + parserContext)
  }
  
}