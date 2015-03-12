package fr.proline.cortex.service.dps.msi

import scala.collection.JavaConversions.mapAsScalaMap
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.provider.msi.SeqDbFakeProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.cortex.service.AbstractRemoteProcessService
import fr.profi.util.serialization.ProfiJson._
import fr.proline.core.service.msi.ResultFileCertifier
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.profi.util.StringUtils
import java.io.File
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.orm.util.DataStoreConnectorFactory
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.cortex.service.IRemoteService
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.module.exporter.msi.template.AllPSMViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.IRMaLikeViewSetTemplateAsTSV
import fr.proline.module.exporter.msi.template.IRMaLikeViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.IRMaLikeFullViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.ProlineViewSetTemplateAsXLSX
import java.util.UUID
import fr.proline.cortex.util.WorkDirectoryFactory
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.commons.ViewSetExporter
import fr.proline.module.exporter.mzidentml.MzIdExporter

/**
 * Define a JMS Service to :
 * Exports result summaries on server side in various files formats.  
 * 
 */

object FileFormat extends Enumeration {
  val MZIDENTML = Value("MZIDENTML")
  val TEMPLATED = Value("TEMPLATED")
}

object OutputMode extends Enumeration {
  val FILE = Value("FILE")
  val STREAM = Value("STREAM")
}

class ExportResultSummaries  extends AbstractRemoteProcessService with Logging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/ExportResultSummaries"
  val serviceVersion = "1.0"
  override val defaultVersion = true
  

  // Configure service interface
 
//  val wsParams = Array(
//    MethodParam(
//      "rsm_identifier",
//      JSONType.Object,
//      description = Some("A tuple containing the project id and the result summary id."),
//      scalaType = Some(typeOf[ResultSummaryIdentifier])
//    ),
//    MethodParam(
//      "file_format",
//      JSONType.String,
//      description = Some("The expected file format. Valid values are: MZIDENTML, TEMPLATED."),
//      scalaType = Some(typeOf[String])
//    ),
//    MethodParam(
//      "file_name",
//      JSONType.String,
//      optional = true,
//      description = Some("The desired name for the file. If not provided, a name beginning with 'IdentificationSummaryExport_' will be generated."),
//      scalaType = Some(typeOf[String])
//    ),
//    MethodParam(
//      "file_directory",
//      JSONType.String,
//      optional = true,
//      description = Some("The desired output directory. If not provided, files will be exported in a temporary directory."),
//      scalaType = Some(typeOf[String])
//    ),
//    MethodParam(
//      "extra_params",
//      JSONType.Object,
//      optional = true,
//      description = Some("A map of parameters specific to the used file format. Possible keys for the TEMPLATED format: template_name."),
//      scalaType = Some(typeOf[Map[String, Any]])
//    )
//  )

  case class ResultSummaryIdentifier(projectId: Long, rsmId: Long)
  
    // TODO: find a more dynamic way to load the templates
  val viewSetTemplateByName = Map(
    "ALL_PEP_MATCHES_XLSX" -> AllPSMViewSetTemplateAsXLSX,
    "IRMA_LIKE_TSV" -> IRMaLikeViewSetTemplateAsTSV,
    "IRMA_LIKE_XLSX" -> IRMaLikeViewSetTemplateAsXLSX,
    "IRMA_LIKE_FULL_XLSX" -> IRMaLikeFullViewSetTemplateAsXLSX,
    "PROLINE_XLSX" -> ProlineViewSetTemplateAsXLSX
  )
  
  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "ParamsRetriever is null")

    //val rsmIdentifiers = paramsRetriever.getList("rsm_identifiers").toArray.map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }
    val rsmIdentifiers = Some(paramsRetriever.getMap("rsm_identifier")).map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }
    
    val fileFormat = FileFormat.withName(paramsRetriever.getString("file_format"))

    val fileName = this.parseFileName(paramsRetriever)
    val fileDirectory = this.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap("extra_params", true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Any]](_))
    val outputParams = OutputMode.withName(paramsRetriever.getOptString("output_mode", "FILE"))

    fileFormat match {
      case FileFormat.MZIDENTML => exportToMzIdentML(Seq(rsmIdentifiers.get), fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.TEMPLATED => exportToTemplatedFile(Seq(rsmIdentifiers.get), fileName, fileDirectory, outputParams, extraParams)
    }
    
  }

  def parseFileName(params: NamedParamsRetriever): String = {
	  val providedFileName = params.getOptString("file_name", null)

    if (StringUtils.isEmpty(providedFileName)) {
      "IdentificationSummaryExport_" + UUID.randomUUID().toString
    } else {
      providedFileName
    }
  }

  def parseFileDirectory(params: NamedParamsRetriever): String = {
    val providedFileDirectory = params.getOptString("file_directory", null)

    if (StringUtils.isEmpty(providedFileDirectory)) {
      WorkDirectoryFactory.prolineWorkDirectory.getAbsolutePath
    } else {
      providedFileDirectory
    }
  }

 def exportToMzIdentML(
    rsmIdentifiers: Seq[ResultSummaryIdentifier],
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Array[String] = {

	  require(rsmIdentifiers.length == 1, "can only export one RSM at a time for the mzIdentML file format")

    val rsmIdentifier = rsmIdentifiers(0)
    var filePath = ""
    val execCtx = BuildExecutionContext(DataStoreConnectorFactory.getInstance(), rsmIdentifier.projectId, false)
     
    try {
      
      val exporter = new MzIdExporter(rsmIdentifier.rsmId, execCtx)

      filePath = fileDir + File.separatorChar + fileName + ".mzid"
      exporter.exportResultSummary(filePath)

    } finally {
      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    Array(filePath)
 
  }

  def exportToTemplatedFile(
    rsmIdentifiers: Seq[ResultSummaryIdentifier],
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Array[String] = {

    require(rsmIdentifiers.length == 1, "can only export one RSM at a time for this file format")
    require(extraParams.isDefined, "some extra parameters must be provided")

    val viewSetTemplateName = extraParams.get("template_name").asInstanceOf[String]
    require(StringUtils.isNotEmpty(viewSetTemplateName), "the template name must be provided")

    val viewSetTemplate = viewSetTemplateByName(viewSetTemplateName)
    val loadFullResultSet =  if( viewSetTemplate == AllPSMViewSetTemplateAsXLSX ||viewSetTemplate == IRMaLikeFullViewSetTemplateAsXLSX) true else false
    
    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    var executionContext: IExecutionContext = null
    try {
      executionContext =  BuildExecutionContext(DataStoreConnectorFactory.getInstance(), rsmIdentifier.projectId, true)
      
      // Export
      val viewSet = BuildResultSummaryViewSet(
        executionContext,
        rsmIdentifier.projectId,
        rsmIdentifier.rsmId,
        loadSubsets = true,
        loadFullResultSet = loadFullResultSet,
        viewSetName = fileName,
        viewSetTemplate = viewSetTemplate
      )
      exportedFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
      }
    }

    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      exportedFiles.toArray.map(_.getAbsolutePath) // Local to this Web-Core host
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }

}