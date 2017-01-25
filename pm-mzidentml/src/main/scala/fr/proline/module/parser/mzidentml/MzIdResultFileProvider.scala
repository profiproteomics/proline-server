package fr.proline.module.parser.mzidentml

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultFileProvider
import fr.proline.core.om.provider.msi.IResultFileVerifier
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.PtmDefinition
import fr.profi.chemistry.model.Enzyme

object MzIdResultFileProvider {
  final val fileType: String = "mzidentml.mzid"
}

class MzIdResultFileProvider extends IResultFileProvider with IResultFileVerifier with LazyLogging { // with IResultFileVerifier

  val fileType: String = MzIdResultFileProvider.fileType

  def getResultFile(fileLocation: File, importProperties: Map[String, Any], parserContext: ProviderDecoratedExecutionContext): IResultFile = {
    new MzIdResultFile(fileLocation, parserContext)
  }
  
  def getResultFileVerifier(): IResultFileVerifier = {
    this
  }
  
  val resultFileProperties = Map.empty[String,Class[_]]
 
  def getEnzyme(fileLocation: File, importProperties: Map[String, Any]): Array[Enzyme] = {
    logger.debug("MzIdentML Parser, call ENZYME : Return EMPTY")
  
    val enzymes = new ArrayBuffer[Enzyme]
    enzymes.toArray
  }
  
   def getPtmDefinitions(fileLocation: File, importProperties: Map[String, Any]): Seq[PtmDefinition] = {
       logger.debug("MzIdentML Parser, call getPtmDefinitions : Return EMPTY")  
       val ptmDefs = Seq.empty[PtmDefinition]        
       return ptmDefs
   }
   
   def isValid(fileLocation: File, importProperties: Map[String, Any]) : Boolean = {
     logger.debug("MzIdentML Parser, call isValid : Return TRUE")  
     true
   }
  
}