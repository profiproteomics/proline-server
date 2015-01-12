package fr.proline.module.parser.mzidentml

import java.io.File
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultFileProvider
import fr.proline.core.om.provider.msi.IResultFileVerifier

object MzIdResultFileProvider {
  final val fileType: String = "mzidentml.mzid"
}

class MzIdResultFileProvider extends IResultFileProvider with Logging { // with IResultFileVerifier

  val fileType: String = MzIdResultFileProvider.fileType

  def getResultFile(fileLocation: File, importProperties: Map[String, Any], parserContext: ProviderDecoratedExecutionContext): IResultFile = {
    new MzIdResultFile(fileLocation, parserContext)
  }
  
  def getResultFileVerifier(): IResultFileVerifier = null
  val resultFileProperties = Map.empty[String,Class[_]]
  
}