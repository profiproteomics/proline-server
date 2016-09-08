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
import fr.proline.module.exporter.msi.template.AllPSMViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.IRMaLikeFullViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.IRMaLikeViewSetTemplateAsTSV
import fr.proline.module.exporter.msi.template.IRMaLikeViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.ProlineViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.template.SpectraListAsTSV
import fr.proline.module.exporter.msi.view.BuildRSMSpectraViewSet
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.mzidentml.MzIdExporter
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

  // TODO: find a more dynamic way to load the templates
  val viewSetTemplateByName = Map(
    "ALL_PEP_MATCHES_XLSX" -> AllPSMViewSetTemplateAsXLSX,
    "IRMA_LIKE_TSV" -> IRMaLikeViewSetTemplateAsTSV,
    "IRMA_LIKE_XLSX" -> IRMaLikeViewSetTemplateAsXLSX,
    "IRMA_LIKE_FULL_XLSX" -> IRMaLikeFullViewSetTemplateAsXLSX,
    "PROLINE_XLSX" -> ProlineViewSetTemplateAsXLSX
  )

  def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require(paramsRetriever != null, "No parameter specified")

    //val rsmIdentifiers = paramsRetriever.getList("rsm_identifiers").toArray.map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }
    //    val rsmIdentifiers = paramsRetriever.getList("rsm_identifier").toArray.map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }
    val rsmIdentifier = Some(paramsRetriever.getMap(PROCESS_METHOD.RSM_IDENTIFIER_PARAM)).map { rsmIdent => deserialize[ExportResultSummaryIdentifier](serialize(rsmIdent)) }

    val fileFormat = ExportFileFormat.withName(paramsRetriever.getString(FILE_FORMAT_PARAM))

    val fileName = this.parseFileName(paramsRetriever)
    val fileDirectory = this.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap(EXTRA_PARAMS_PARAM, true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))
    val outputParams = ExportOutputMode.withName(paramsRetriever.getOptString(OUTPUT_MODE_PARAM, ExportOutputMode.FILE))

    fileFormat match {
      //      case FileFormat.MZIDENTML => exportToMzIdentML(Seq(rsmIdentifiers.get), fileName, fileDirectory, outputParams, extraParams)
      //      case FileFormat.TEMPLATED => exportToTemplatedFile(Seq(rsmIdentifiers.get), fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.MZIDENTML    => exportToMzIdentML(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.TEMPLATED    => exportToTemplatedFile(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.PRIDE        => exportToPrideFile(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.SPECTRA_LIST => exportToSpectraList(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
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

  def parseFileDirectory(params: NamedParamsRetriever): String = {
    val providedFileDirectory = params.getOptString(FILE_DIRECTORY_PARAM, null)

    if (StringUtils.isEmpty(providedFileDirectory)) {
      WorkDirectoryFactory.prolineWorkDirectory.getAbsolutePath
    } else {
      providedFileDirectory
    }
  }

  def exportToMzIdentML(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Array[String] = {

    // require(rsmIdentifiers.length == 1, "can only export one RSM at a time for the mzIdentML file format")

    // val rsmIdentifier = rsmIdentifiers(0)
    var filePath = ""
    val execCtx = DbConnectionHelper.createSQLExecutionContext(rsmIdentifier.projectId)

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
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Object = {

    //    require(rsmIdentifiers.length == 1, "can only export one RSM at a time for this file format")
    require(extraParams.isDefined, "some extra parameters must be provided")

    val viewSetTemplateName = extraParams.get("template_name").asInstanceOf[String]
    require(StringUtils.isNotEmpty(viewSetTemplateName), "the template name must be provided")

    val viewSetTemplate = viewSetTemplateByName(viewSetTemplateName)
    val loadFullResultSet = if (viewSetTemplate == AllPSMViewSetTemplateAsXLSX || viewSetTemplate == IRMaLikeFullViewSetTemplateAsXLSX) true else false

    //    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    var executionContext: IExecutionContext = null
    try {
      executionContext = DbConnectionHelper.createJPAExecutionContext(rsmIdentifier.projectId)

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

    //    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    var executionContext: IExecutionContext = null
    try {
      executionContext = DbConnectionHelper.createJPAExecutionContext(rsmIdentifier.projectId)

      // Export
      val viewSet = BuildRSMSpectraViewSet(
        executionContext,
        rsmIdentifier.projectId,
        rsmIdentifier.rsmId,
        viewSetName = fileName,
        viewSetTemplate = viewSetTemplate
      )
      exportedFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
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

  def exportToPrideFile(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Object]]
  ): Object = {

    //    require(extraParams.isDefined, "some extra parameters must be provided")

    var exportLocation = new java.io.File(fileDir)
    val filePath = fileDir + File.separator + fileName
    var executionContext: IExecutionContext = null

    try {
      executionContext = DbConnectionHelper.createSQLExecutionContext(rsmIdentifier.projectId)

      val extraParameters: Map[String, Object] = if (extraParams.isDefined) extraParams.get else Map.empty
      // Export

      val exporter = new PrideExporterService(executionContext, rsmIdentifier.rsmId, filePath, extraParameters)
      exporter.runService()

    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
      }
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
      case ExportFileFormat.TEMPLATED    => require(rsmIdentifiers.size > 0, "Could export at least one Result into Excel format")
      case ExportFileFormat.PRIDE        => require(rsmIdentifiers.size == 1, "Could export only one Result into Pride format")
      case ExportFileFormat.SPECTRA_LIST => require(rsmIdentifiers.size == 1, "Could export only one Result into Spectra List")
    }
    val fileName = paramsRetriever.getOptString("file_name", null)
    val fileDirectory = this.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap("extra_params", true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))
    val outputParams = ExportOutputMode.withName(paramsRetriever.getOptString("output_mode", "FILE"))

    fileFormat match {
      case ExportFileFormat.MZIDENTML    => exportToMzIdentML(rsmIdentifiers(0), fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.TEMPLATED    => exportToTemplatedFile(rsmIdentifiers, fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.PRIDE        => exportToPrideFile(rsmIdentifiers(0), fileName, fileDirectory, outputParams, extraParams)
      case ExportFileFormat.SPECTRA_LIST => exportToSpectraList(rsmIdentifiers(0), fileName, fileDirectory, outputParams, extraParams)
    }
  }

  def exportToMzIdentML(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Any]]
  ): Array[String] = {

    // require(rsmIdentifiers.length == 1, "can only export one RSM at a time for the mzIdentML file format")

    // val rsmIdentifier = rsmIdentifiers(0)
    var filePath = ""
    val execCtx = DbConnectionHelper.createSQLExecutionContext(rsmIdentifier.projectId)

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

  def exportToPrideFile(
    rsmIdentifier: ExportResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: ExportOutputMode.Value,
    extraParams: Option[Map[String, Object]]
  ): Object = {

    //    require(extraParams.isDefined, "some extra parameters must be provided")

    var exportLocation = new java.io.File(fileDir)
    val filePath = fileDir + File.separator + fileName
    var executionContext: IExecutionContext = null

    try {
      executionContext = DbConnectionHelper.createSQLExecutionContext(rsmIdentifier.projectId)

      val extraParameters: Map[String, Object] = if (extraParams.isDefined) extraParams.get else Map.empty
      // Export

      val exporter = new PrideExporterService(executionContext, rsmIdentifier.rsmId, filePath, extraParameters)
      exporter.runService()

    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
      }
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

    var mode: String = DatasetUtil.getExportMode(rsmIdentifiers(0).projectId, rsmIdentifiers(0).dsId.get)
    var exportConfig: ExportConfig = null

    if (extraParams != null && extraParams.isDefined && extraParams.get.contains("config")) {
      exportConfig = ExportConfigManager.readConfig(extraParams.get("config").asInstanceOf[String])
      logger.debug("exporting...")
    } else {
      // default conf if not filled -- all dataset have same type, so the config is based on the first
      exportConfig = ExportConfigManager.getDefaultExportConfig(mode)
    }

    var exportLocation = new java.io.File(fileDir)
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
        if (executionContext != null) {
          executionContext.closeAll()
        }
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

    //    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    var executionContext: IExecutionContext = null
    try {
      executionContext = DbConnectionHelper.createJPAExecutionContext(rsmIdentifier.projectId)

      // Export
      val viewSet = BuildRSMSpectraViewSet(
        executionContext,
        rsmIdentifier.projectId,
        rsmIdentifier.rsmId,
        viewSetName = fileName,
        viewSetTemplate = viewSetTemplate)
      exportedFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
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
}