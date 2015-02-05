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

object OmssaResultFileProviderType {
    final val fileType: String = "xtandem.xml"
}


class XtandemResultFileProvider(  val xtandemFilePath : String, 
                                  val parserContext: ProviderDecoratedExecutionContext
                                  ) extends IResultFileProvider with Logging {

  val fileType: String = OmssaResultFileProviderType.fileType

  def getResultFile( fileLocation: File, importProperties : Map[String, Any], parserContext: ProviderDecoratedExecutionContext ): IResultFile = {
    new XtandemParser(xtandemFilePath, parserContext)
  }

  val resultFileProperties : Map[String, Class[_]] = null
  
  def getResultFileVerifier : IResultFileVerifier = {
    new XtandemResultFileVerifier(xtandemFilePath, parserContext)
  }
}