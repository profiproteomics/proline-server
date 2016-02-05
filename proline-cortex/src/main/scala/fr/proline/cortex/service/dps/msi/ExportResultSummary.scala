package fr.proline.cortex.service.dps.msi

import java.io.File
import java.util.HashMap
import java.util.UUID
import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.orm.uds.{ Dataset => UdsDataset }
import fr.proline.core.orm.uds.Dataset.DatasetType
import fr.proline.core.orm.uds.QuantitationMethod
import fr.proline.core.service.msq.AbundanceUnit
import fr.proline.core.service.msq.QuantMethodType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.WorkDirectoryFactory
import fr.proline.jms.util.Constants
import fr.proline.jms.util.NodeConfig
import fr.proline.module.exporter.ViewSetExporter
import fr.proline.module.exporter.commons.config.ExportConfigConstant
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
import fr.proline.jms.service.api.AbstractRemoteProcessService

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
 *   	If FILE output mode was specified, the files path 
 *      if STREAM the files path and  PROLINE_NODE_ID
 *      
 *   Version 2.0 : Use the customizable export service which also allow quanti RSM export. 
 *   Input Params  
 *     Same as Version 1.0 but rsm_identifier contains also the Dataset ID to export. 
 *   Output params 
 *     Same as Version 1.0 
 */

object FileFormat extends Enumeration {
  val MZIDENTML = Value("MZIDENTML")
  val TEMPLATED = Value("TEMPLATED")
  val PRIDE = Value("PRIDE")
  val SPECTRA_LIST = Value("SPECTRA_LIST")
}

object OutputMode extends Enumeration {
  val FILE = Value("FILE")
  val STREAM = Value("STREAM")
}

case class ResultSummaryIdentifier(projectId: Long, rsmId: Long)
case class RsmIdentifier(projectId: Long, dsId: Long, rsmId: Long)

class ExportResultSummary extends AbstractRemoteProcessService with LazyLogging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/ExportResultSummary"
  val serviceVersion = "1.0"
  override val defaultVersion = true


  // TODO: find a more dynamic way to load the templates
  val viewSetTemplateByName = Map(
    "ALL_PEP_MATCHES_XLSX" -> AllPSMViewSetTemplateAsXLSX,
    "IRMA_LIKE_TSV" -> IRMaLikeViewSetTemplateAsTSV,
    "IRMA_LIKE_XLSX" -> IRMaLikeViewSetTemplateAsXLSX,
    "IRMA_LIKE_FULL_XLSX" -> IRMaLikeFullViewSetTemplateAsXLSX,
    "PROLINE_XLSX" -> ProlineViewSetTemplateAsXLSX)

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "No parameter specified")

    //val rsmIdentifiers = paramsRetriever.getList("rsm_identifiers").toArray.map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }
    //    val rsmIdentifiers = paramsRetriever.getList("rsm_identifier").toArray.map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }
    val rsmIdentifier = Some(paramsRetriever.getMap("rsm_identifier")).map { rsmIdent => deserialize[ResultSummaryIdentifier](serialize(rsmIdent)) }

    val fileFormat = FileFormat.withName(paramsRetriever.getString("file_format"))

    val fileName = this.parseFileName(paramsRetriever)
    val fileDirectory = this.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap("extra_params", true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))
    val outputParams = OutputMode.withName(paramsRetriever.getOptString("output_mode", "FILE"))

    fileFormat match {
      //      case FileFormat.MZIDENTML => exportToMzIdentML(Seq(rsmIdentifiers.get), fileName, fileDirectory, outputParams, extraParams)
      //      case FileFormat.TEMPLATED => exportToTemplatedFile(Seq(rsmIdentifiers.get), fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.MZIDENTML => exportToMzIdentML(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.TEMPLATED => exportToTemplatedFile(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.PRIDE => exportToPrideFile(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.SPECTRA_LIST => exportToSpectraList(rsmIdentifier.get, fileName, fileDirectory, outputParams, extraParams)
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
    rsmIdentifier: ResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Array[String] = {

    //	  require(rsmIdentifiers.length == 1, "can only export one RSM at a time for the mzIdentML file format")

    //    val rsmIdentifier = rsmIdentifiers(0)
    var filePath = ""
    val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, rsmIdentifier.projectId, false)

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
    rsmIdentifier: ResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Object = {

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
      executionContext = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, rsmIdentifier.projectId, true)

      // Export
      val viewSet = BuildResultSummaryViewSet(
        executionContext,
        rsmIdentifier.projectId,
        rsmIdentifier.rsmId,
        loadSubsets = true,
        loadFullResultSet = loadFullResultSet,
        viewSetName = fileName,
        viewSetTemplate = viewSetTemplate)
      exportedFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
      }
    }

    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", exportedFiles.toArray.map(_.getAbsolutePath))
      resultMap.put(Constants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }
  
    def exportToSpectraList(
    rsmIdentifier: ResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Object = {

    val viewSetTemplate = SpectraListAsTSV


    //    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    var executionContext: IExecutionContext = null
    try {
      executionContext = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, rsmIdentifier.projectId, true)

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

    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", exportedFiles.toArray.map(_.getAbsolutePath))
      resultMap.put(Constants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }

  def exportToPrideFile(
    rsmIdentifier: ResultSummaryIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Object]]): Object = {

    //    require(extraParams.isDefined, "some extra parameters must be provided")

    var exportLocation = new java.io.File(fileDir)
    val filePath = fileDir + File.separator + fileName
    var executionContext: IExecutionContext = null

    try {
      executionContext = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), rsmIdentifier.projectId, false)

      val extraParameters: Map[String, Object] = if (extraParams.isDefined) extraParams.get else Map.empty
      // Export

      val exporter = new PrideExporterService(executionContext, rsmIdentifier.rsmId, filePath, extraParameters)
      exporter.runService()

    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
      }
    }

    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", Seq(filePath))
      resultMap.put(Constants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      Seq(filePath)
    }

  }
  
  
  
}

class ExportResultSummaryV2_0 extends AbstractRemoteProcessService with LazyLogging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/ExportResultSummary"
  val serviceVersion = "2.0"

  def parseFileDirectory(params: NamedParamsRetriever): String = {
    val providedFileDirectory = params.getOptString("file_directory", null)

    if (StringUtils.isEmpty(providedFileDirectory)) {
      WorkDirectoryFactory.prolineWorkDirectory.getAbsolutePath
    } else {
      providedFileDirectory
    }
  }

  def parseRsmIdent(rfDescObj: Object): RsmIdentifier  ={
    deserialize[RsmIdentifier](serialize(rfDescObj))
  }
  
  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "ParamsRetriever is null")

    val rsmIdentifiers: ArrayBuffer[RsmIdentifier] = new ArrayBuffer()
    val listRsm : java.util.List[Object] = paramsRetriever.getList("rsm_identifiers")
    for (rsm <- listRsm.toArray){
       val rsmIdent : RsmIdentifier = deserialize[RsmIdentifier](serialize(rsm)) 
       rsmIdentifiers += rsmIdent
    }
    
    val fileFormat = FileFormat.withName(paramsRetriever.getString("file_format"))
   fileFormat match {
      case FileFormat.MZIDENTML => require(rsmIdentifiers.size ==1, "Could export only one Result into MzIdent")
      case FileFormat.TEMPLATED => require(rsmIdentifiers.size > 0, "Could export at least one Result into Excel format")
      case FileFormat.PRIDE => require(rsmIdentifiers.size ==1, "Could export only one Result into Pride format")
      case FileFormat.SPECTRA_LIST => require(rsmIdentifiers.size ==1, "Could export only one Result into Spectra List")
    }
    val fileName = paramsRetriever.getOptString("file_name", null)
    val fileDirectory = this.parseFileDirectory(paramsRetriever)

    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap("extra_params", true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))
    val outputParams = OutputMode.withName(paramsRetriever.getOptString("output_mode", "FILE"))

    fileFormat match {
      case FileFormat.MZIDENTML => exportToMzIdentML(rsmIdentifiers(0), fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.TEMPLATED => exportToTemplatedFile(rsmIdentifiers, fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.PRIDE => exportToPrideFile(rsmIdentifiers(0), fileName, fileDirectory, outputParams, extraParams)
      case FileFormat.SPECTRA_LIST => exportToSpectraList(rsmIdentifiers(0), fileName, fileDirectory, outputParams, extraParams)
    }
  }

  def exportToMzIdentML(
    rsmIdentifier: RsmIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Array[String] = {

    //	  require(rsmIdentifiers.length == 1, "can only export one RSM at a time for the mzIdentML file format")

    //    val rsmIdentifier = rsmIdentifiers(0)
    var filePath = ""
    val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), rsmIdentifier.projectId, false)

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
    rsmIdentifier: RsmIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Object]]): Object = {

    //    require(extraParams.isDefined, "some extra parameters must be provided")

    var exportLocation = new java.io.File(fileDir)
    val filePath = fileDir + File.separator + fileName
    var executionContext: IExecutionContext = null

    try {
      executionContext = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), rsmIdentifier.projectId, false)

      val extraParameters: Map[String, Object] = if (extraParams.isDefined) extraParams.get else Map.empty
      // Export

      val exporter = new PrideExporterService(executionContext, rsmIdentifier.rsmId, filePath, extraParameters)
      exporter.runService()

    } finally {
      if (executionContext != null) {
        executionContext.closeAll()
      }
    }

    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", Seq(filePath))
      resultMap.put(Constants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      Seq(filePath)
    }

  }
  
  
  def exportToTemplatedFile(
    rsmIdentifiers:  Seq[RsmIdentifier],
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Object = {

    //    require(rsmIdentifiers.length == 1, "can only export one RSM at a time for this file format")
    require(extraParams.isDefined, "some extra parameters must be provided")

    var exportConfigStr : String = null
    if (extraParams != null && extraParams.isDefined && extraParams.get.contains("config")){
        exportConfigStr = extraParams.get("config").asInstanceOf[String]
        //logger.debug("export with config "+exportConfigStr)
        logger.debug("exporting...")
        //require(StringUtils.isNotEmpty(exportConfigStr), "the export configuration must be provided")
    }else{// default conf if not filled -- all dataset have same type, so the config is based on the first
      var mode: String = ExportConfigConstant.MODE_IDENT
      val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), rsmIdentifiers(0).projectId, true) 
      try {
        val udsDbCtx = execCtx.getUDSDbConnectionContext()
        val udsEM = udsDbCtx.getEntityManager()
        val udsDs = udsEM.find(classOf[UdsDataset], rsmIdentifiers(0).dsId)
        if (udsDs != null) {
          val dsType: DatasetType = udsDs.getType()
          if (dsType == DatasetType.QUANTITATION) {
            mode = ExportConfigConstant.MODE_QUANT_XIC
            val dsMethod: QuantitationMethod = udsDs.getMethod()
            val quantMethodType = dsMethod.getType
            val abundanceUnit = dsMethod.getAbundanceUnit
            if (quantMethodType == QuantMethodType.LABEL_FREE.toString() && abundanceUnit == AbundanceUnit.SPECTRAL_COUNTS.toString()) {
              mode = ExportConfigConstant.MODE_QUANT_SC
            }
          }
        }
      } finally {
        if (execCtx != null) {
        execCtx.closeAll()
        }
      }
      exportConfigStr = ExportConfigManager.getDefaultConfiguration(mode)
    }
    //    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: ArrayBuffer[java.io.File] = new ArrayBuffer()
    
    for (rsmIdentifier <- rsmIdentifiers) {
      var executionContext: IExecutionContext = null
      try {
        executionContext =  BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), rsmIdentifier.projectId, true)
        
       var fileDatasetName = fileName
       if (StringUtils.isEmpty(fileName)) {
         fileDatasetName = "DatasetSummaryExport-"+rsmIdentifier.dsId+"_" + UUID.randomUUID().toString
       }	
        var exFiles: Seq[java.io.File] = Seq()
        // Export
        val viewSet = BuildDatasetViewSet(
          executionContext = executionContext,
          projectId = rsmIdentifier.projectId,
          dsId = rsmIdentifier.dsId,
          rsmId = rsmIdentifier.rsmId,
          viewSetName = fileDatasetName,
          exportConfigStr = exportConfigStr)

        exFiles = ViewSetExporter.exportViewSetToDirectory(viewSet, exportLocation)
        for(f <- exFiles){
          exportedFiles += f
        }
        
      } finally {
        if (executionContext != null) {
          executionContext.closeAll()
        }
      }
    }
    
    
    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", exportedFiles.toArray.map(_.getAbsolutePath))
      resultMap.put(Constants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }
  
    def exportToSpectraList(
    rsmIdentifier: RsmIdentifier,
    fileName: String,
    fileDir: String,
    outputFormat: OutputMode.Value,
    extraParams: Option[Map[String, Any]]): Object = {

    val viewSetTemplate = SpectraListAsTSV


    //    val rsmIdentifier = rsmIdentifiers(0)

    var exportLocation = new java.io.File(fileDir)
    var exportedFiles: Seq[java.io.File] = Seq()

    var executionContext: IExecutionContext = null
    try {
      executionContext = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, rsmIdentifier.projectId, true)

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

    if (outputFormat == OutputMode.STREAM) {
      // TODO for distributed (JMS) deployed services, return a complete URL with fully qualified host name
      val resultMap = new HashMap[String, Object]()
      resultMap.put("file_paths", exportedFiles.toArray.map(_.getAbsolutePath))
      resultMap.put(Constants.PROLINE_NODE_ID_KEY, NodeConfig.NODE_ID)
      resultMap
    } else {
      exportedFiles.toArray.map(_.getName)
    }

  }
}