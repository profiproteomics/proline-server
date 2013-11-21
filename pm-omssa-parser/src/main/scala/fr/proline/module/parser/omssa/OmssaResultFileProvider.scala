package fr.proline.module.parser.omssa

import java.io.File
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.msi.IResultFileProvider
import fr.proline.core.om.provider.msi.IResultFileVerifier
//import fr.proline.core.om.model.msi.{IResultFileProvider, IResultFile}
//import fr.proline.repository.DatabaseContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import com.weiglewilczek.slf4s.Logging

object OmssaResultFileProviderType {
   def fileType: String = "OmssaMSParser"
}


class OmssaResultFileProvider extends IResultFileProvider with Logging {

  val fileType: String = OmssaResultFileProviderType.fileType

//  def getResultFile( fileLocation: File, importProperties : Map[String, Any], providerKey: String ): IResultFile = {   
////  def getResultFile( fileLocation: File, providerKey: String, importProperties : Map[String, Any] ): IResultFile = {
//    new OmssaResultFile( fileLocation, providerKey, importProperties )
//  }
  def getResultFile( fileLocation: File, importProperties : Map[String, Any], parserContext: ProviderDecoratedExecutionContext ): IResultFile = {
    new OmssaResultFile(
        fileLocation = fileLocation, 
        parserContext = parserContext, 
        importProperties = importProperties
    )
  }

  val resultFileProperties : Map[String, Class[_]] = {	 
	val propertiesBuilder = Map.newBuilder[String, Class[_]]
	propertiesBuilder += (OmssaParseParams.RAW_FILE_PATH.toString -> classOf[String])
	propertiesBuilder += (OmssaParseParams.PEAK_LIST_FILE_PATH.toString -> classOf[String])
	propertiesBuilder += (OmssaParseParams.FASTA_CONTAINS_TARGET.toString -> Boolean.getClass())
	propertiesBuilder += (OmssaParseParams.FASTA_CONTAINS_DECOY.toString -> Boolean.getClass())
	propertiesBuilder += (OmssaParseParams.FASTA_FILE_PATH.toString -> classOf[String])
	propertiesBuilder += (OmssaParseParams.FASTA_TAXONOMIES.toString -> classOf[String])
	propertiesBuilder += (OmssaParseParams.OMSSA_VERSION.toString -> classOf[String])
//	propertiesBuilder += (OmssaParseParams.OMSSA_XSD_FILE.toString -> classOf[File])
//	propertiesBuilder += (OmssaParseParams.MOD_XML_FILE.toString -> classOf[File])
	propertiesBuilder += (OmssaParseParams.USERMOD_XML_FILE.toString -> classOf[File])
	propertiesBuilder.result
  }
  
  def getResultFileVerifier : IResultFileVerifier = {
    new OmssaResultFileVerifier()
  }
}