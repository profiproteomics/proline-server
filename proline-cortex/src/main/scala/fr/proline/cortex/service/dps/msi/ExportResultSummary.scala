package fr.proline.cortex.service.dps.msi

import java.io.File
import java.util.HashMap
import java.util.UUID

import scala.collection.mutable.ArrayBuffer
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.IExecutionContext
import fr.proline.cortex.api.service.dps.msi.ExportFileFormat
import fr.proline.cortex.api.service.dps.msi.ExportOutputMode
import fr.proline.cortex.api.service.dps.msi.ExportResultSummaryIdentifier
import fr.proline.cortex.api.service.dps.msi.IExportResultSummariesServiceV2_0
import fr.proline.cortex.api.service.dps.msi.IExportResultSummaryServiceV1_0
import fr.proline.cortex.service.dps.uds.DatasetUtil
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.WorkDirectoryFactory
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.util.JMSConstants
import fr.proline.jms.util.NodeConfig
import fr.proline.module.exporter.ViewSetExporter
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigManager
import fr.proline.module.exporter.dataset.view.BuildDatasetViewSet
import fr.proline.module.exporter.msi.template.SpectraListAsTSV
import fr.proline.module.exporter.msi.view.{BuildRSMSpectraViewSet, FormatCompatibility}
import fr.proline.module.exporter.mzidentml.MzIdExporter
import fr.proline.module.exporter.mzidentml.model.Contact
import fr.proline.module.exporter.mzidentml.model.Organization
import fr.proline.module.exporter.pridexml.PrideExporterService

/**
 * Define a JMS Service to :
 * Exports result summaries on server side in various files formats.
 *
 *  Version 1.0
 *  Input Params
 *    rsm_identifier : A tuple containing the project id and the result summary id.
 *    file_format : The expected file format. Valid values are: MZIDENTML, TEMPLATED, PRIDE.
 *    file_name : The desired name for the file. If not provided, a name beginning with 'IdentificationSummaryExport_' will be generated.
 *    file_directory : The desired output directory. If not provided, files will be exported in a temporary directory.
 *    extra_params : A map of parameters specific to the used file format. Possible keys for the TEMPLATED format: template_name.
 *    output_mode : Result exported file format : FILE or STREAM
 *
 *   Output params
 *    If FILE output mode was specified, the files path
 *    If STREAM the files path and PROLINE_NODE_ID
 *
 *   Version 2.0 : Use the customizable export service which also allow quanti RSM export.
 *   Input Params
 *     Same as Version 1.0 but rsm_identifier contains also the Dataset ID to export.
 *   Output params
 *     Same as Version 1.0
 */
class ExportResultSummary extends AbstractRemoteProcessingService with IExportResultSummaryServiceV1_0 with LazyLogging {

  private val exportV2 = new ExportResultSummaryV2_0()
  
  def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require(paramsRetriever != null, "No parameter specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.RSM_IDENTIFIER_PARAM),"No RSM IDENTIFIER provided!")

    val rsmIdentifierAsMap = paramsRetriever.getMap(PROCESS_METHOD.RSM_IDENTIFIER_PARAM)
    val rsmIdentifier = deserialize[ExportResultSummaryIdentifier](serialize(rsmIdentifierAsMap))

    val fileFormat = ExportFileFormat.withName(paramsRetriever.getString(FILE_FORMAT_PARAM))

    val fileName = this.parseFileName(paramsRetriever)
    val fileDirectory = exportV2.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap(EXTRA_PARAMS_PARAM, true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))
    val outputParams = ExportOutputMode.withName(paramsRetriever.getOptString(OUTPUT_MODE_PARAM, ExportOutputMode.FILE))
    
    fileFormat match {
      case ExportFileFormat.MZIDENTML    => exportV2.exportToMzIdentML(rsmIdentifier, fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.TEMPLATED    => exportV2.exportToTemplatedFile(Seq(rsmIdentifier), fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.PRIDE        => exportV2.exportToPrideFile(rsmIdentifier, fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.SPECTRA_LIST => exportV2.exportToSpectraList(rsmIdentifier, fileName, fileDirectory, outputParams, extraParams)
    }

  }

  def parseFileName(params: NamedParamsRetriever): String = {
    val providedFileName = params.getOptString(FILE_NAME_PARAM, null)

    if (StringUtils.isEmpty(providedFileName)) {
      "IdentificationSummaryExport_" + UUID.randomUUID().toString
    } else {
      providedFileName
    }
  }

}

class ExportResultSummaryV2_0 extends AbstractRemoteProcessingService with IExportResultSummariesServiceV2_0 with LazyLogging {

  def parseFileDirectory(params: NamedParamsRetriever): String = {
    val providedFileDirectory = params.getOptString(FILE_DIRECTORY_PARAM, null)

    if (StringUtils.isEmpty(providedFileDirectory)) {
      WorkDirectoryFactory.prolineWorkDirectory.getAbsolutePath
    } else {
      providedFileDirectory
    }
  }

  def parseRsmIdent(rfDescObj: Object): ExportResultSummaryIdentifier = {
    deserialize[ExportResultSummaryIdentifier](serialize(rfDescObj))
  }

  def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require(paramsRetriever != null, "ParamsRetriever is null")

    val rsmIdentifiers: ArrayBuffer[ExportResultSummaryIdentifier] = new ArrayBuffer()
    val listRsm: java.util.List[Object] = paramsRetriever.getList(PROCESS_METHOD.RSM_IDENTIFIERS_PARAM)
    for (rsm <- listRsm.toArray) {
      val rsmIdent: ExportResultSummaryIdentifier = deserialize[ExportResultSummaryIdentifier](serialize(rsm))
      rsmIdentifiers += rsmIdent
    }

    val fileFormat = ExportFileFormat.withName(paramsRetriever.getString(FILE_FORMAT_PARAM))
    fileFormat match {
      case ExportFileFormat.MZIDENTML    => require(rsmIdentifiers.size == 1, "Could export only one Result into MzIdent")
      case ExportFileFormat.TEMPLATED    => require(rsmIdentifiers.nonEmpty, "Could export at least one Result into Excel format")
      case ExportFileFormat.PRIDE        => require(rsmIdentifiers.size == 1, "Could export only one Result into Pride format")
      case ExportFileFormat.SPECTRA_LIST => require(rsmIdentifiers.size == 1, "Could export only one Result into Spectra List")
    }
    
    var fileName = paramsRetriever.getOptString("file_name", null)
    // Don't set default name in case of tempateds : may have more thant one and returned filename format is used by Studio.  
    if ( !fileFormat.equals(ExportFileFormat.TEMPLATED) && StringUtils.isEmpty(fileName)) { 
      fileName = "DatasetExport-" + UUID.randomUUID().toString 
    }
    val fileDirectory = this.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap("extra_params", true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))
    val outputMode = ExportOutputMode.withName(paramsRetriever.getOptString("output_mode", "FILE"))

    fileFormat match {
      case ExportFileFormat.MZIDENTML    => exportToMzIdentML(rsmIdentifiers(0), fileName, fileDirectory, outputMode, extraParams)
      case ExportFileFormat.TEMPLATED    => exportToTemplatedFile(rsmIdentifiers, fileName, fileDirectory, outputMode, extraParams)
      case ExportFileFormat.PRIDE        => exportToPrideFile(rsmIdentifiers(0), fileName, fileDirectory, outputMode, extraParams)
      case ExportFileFormat.SPECTRA_LIST => exportToSpectraList(rsmIdentifiers(0), fileName, fileDirectory, outputMode, extraParams)
    }
  }

  def exportToMzIdentML(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Object = {

    // require(rsmIdentifiers.length == 1, "can only export one RSM at a time for the mzIdentML file format")
    import fr.profi.util.serialization.ProfiJson.deserialize
    // val rsmIdentifier = rsmIdentifiers(0)
    var filePath = ""
    val exportContact = if(extraParams.isDefined && extraParams.get.contains("contact")){
      val contactInfo = serialize(extraParams.get.apply("contact"))
       deserialize[Contact](contactInfo)
    } else {
      new Contact("Proline User", "ProFi", None, None)
    }
    val exportOrg = if(extraParams.isDefined && extraParams.get.contains("organization")){
      val orgInfo = serialize(extraParams.get.apply("organization"))
      deserialize[Organization](orgInfo)
    } else {
      new Organization("Proline User", None)
    }

    val execCtx = DbConnectionHelper.createSQLExecutionContext(rsmIdentifier.projectId)

    try {

      val exporter = new MzIdExporter(rsmIdentifier.rsmId, execCtx)

      filePath = fileDir + File.separatorChar + fileName + ".mzid"
      exporter.exportResultSummary(filePath, exportContact, exportOrg)

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    if (outputFormat == ExportOutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", Seq(filePath))
      resultMap.put(JMSConstants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      Seq(filePath)
    }
  }

  def exportToPrideFile(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Object]]
  ): Object = {

    //    require(extraParams.isDefined, "some extra parameters must be provided")

    val filePath = fileDir + File.separator + fileName
    var executionContext: IExecutionContext = null

    try {
      executionContext = DbConnectionHelper.createSQLExecutionContext(rsmIdentifier.projectId)

      val extraParameters: Map[String, Object] = if (extraParams.isDefined) extraParams.get else Map.empty
      // Export

      val exporter = new PrideExporterService(executionContext, rsmIdentifier.rsmId, filePath, extraParameters)
      exporter.runService()

    } finally {
      DbConnectionHelper.tryToCloseExecContext(executionContext)
    }

    if (outputFormat == ExportOutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", Seq(filePath))
      resultMap.put(JMSConstants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      Seq(filePath)
    }

  }

  def exportToTemplatedFile(
    rsmIdentifiers: Seq[ExportResultSummaryIdentifier],
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Object = {

    //    require(rsmIdentifiers.length == 1, "can only export one RSM at a time for this file format")
    require(extraParams.isDefined, "some extra parameters must be provided")

    val mode: String = DatasetUtil.getExportMode(rsmIdentifiers.head.projectId, rsmIdentifiers.head.dsId.get)
    var exportConfig: ExportConfig = null

    if (extraParams != null && extraParams.isDefined && extraParams.get.contains("config")) {
      exportConfig = ExportConfigManager.readConfig(extraParams.get("config").asInstanceOf[String])
      logger.debug("exporting...")
    } else {
      // default conf if not filled -- all dataset have same type, so the config is based on the first
      exportConfig = ExportConfigManager.getDefaultExportConfig(mode)
    }

    val exportLocation = new java.io.File(fileDir)
    var exportedFiles: ArrayBuffer[java.io.File] = new ArrayBuffer()

    for (rsmIdentifier <- rsmIdentifiers) {
      var executionContext: IExecutionContext = null
      try {
        executionContext = DbConnectionHelper.createJPAExecutionContext(rsmIdentifier.projectId)

        var fileDatasetName = fileName
        if (StringUtils.isEmpty(fileName)) {
          fileDatasetName = "DatasetSummaryExport-" + rsmIdentifier.dsId.get + "_" + UUID.randomUUID().toString //In this version of exported DSId is mandatory
        }
        var exFiles: Seq[java.io.File] = Seq()
        // Export
        val viewSet = BuildDatasetViewSet(
          executionContext = executionContext,
          projectId = rsmIdentifier.projectId,
          dsId = rsmIdentifier.dsId.get,
          rsmId = rsmIdentifier.rsmId,
          viewSetName = fileDatasetName,
          mode = mode,
          exportConfig = exportConfig
        )

        exFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
        for (f <- exFiles) {
          exportedFiles += f
        }

      } finally {
        DbConnectionHelper.tryToCloseExecContext(executionContext)
      }
    }

    if (outputFormat == ExportOutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", exportedFiles.toArray.map(_.getAbsolutePath))
      resultMap.put(JMSConstants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }

  def exportToSpectraList(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Object = {

    val viewSetTemplate = SpectraListAsTSV
    val exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    val formatCompatibility = if (extraParams != null && extraParams.isDefined && extraParams.get.contains("format_compatibility")) {
      FormatCompatibility.withName(extraParams.get("format_compatibility").asInstanceOf[String])
    } else {
      FormatCompatibility.PEAKVIEW
    }

    var executionContext: IExecutionContext = null
    try {
      executionContext = DbConnectionHelper.createJPAExecutionContext(rsmIdentifier.projectId)

      // Export
      val viewSet = BuildRSMSpectraViewSet(
        executionContext,
        rsmIdentifier.projectId,
        rsmIdentifier.rsmId,
        viewSetName = fileName,
        viewSetTemplate = viewSetTemplate,
        mode = formatCompatibility)
      exportedFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
    } finally {
      DbConnectionHelper.tryToCloseExecContext(executionContext)
    }

    if (outputFormat == ExportOutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", exportedFiles.toArray.map(_.getAbsolutePath))
      resultMap.put(JMSConstants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }
}