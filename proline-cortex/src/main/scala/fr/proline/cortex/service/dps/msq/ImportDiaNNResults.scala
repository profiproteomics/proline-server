package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.{SQLInstrumentConfigProvider, SQLPTMProvider, SQLPeptideProvider}
import fr.proline.core.om.provider.msi.{IInstrumentConfigProvider, IPTMProvider, IPeptideProvider}
import fr.proline.core.om.provider.{PeptideCacheExecutionContext, ProviderDecoratedExecutionContext}
import fr.proline.cortex.api.service.dps.msq.{IImportDiaNNResultsService, ImportedDiaNNResult}
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.{AbstractRemoteProcessingService, ISingleThreadedService}
import fr.proline.module.paser.diann.DiaNNResultsParser

import java.io.File
import java.util
import scala.collection.JavaConverters._

object ImportDiaNNResults extends LazyLogging {

  def buildParserContext(executionContext: IExecutionContext): ProviderDecoratedExecutionContext = {

    // Register some providers
    val parserContext = ProviderDecoratedExecutionContext(PeptideCacheExecutionContext(executionContext)) // Use Object factory and use PeptideCache

    val msiSQLCtx = executionContext.getMSIDbConnectionContext
    val sqlPTMProvider = new SQLPTMProvider(msiSQLCtx)
    parserContext.putProvider(classOf[IPTMProvider], sqlPTMProvider)

    val sqlPepProvider = new SQLPeptideProvider(PeptideCacheExecutionContext(parserContext))
    parserContext.putProvider(classOf[IPeptideProvider], sqlPepProvider)

    val instrumConfigProvider = new SQLInstrumentConfigProvider(executionContext.getUDSDbConnectionContext)
    parserContext.putProvider(classOf[IInstrumentConfigProvider], instrumConfigProvider)
    parserContext
  }
}

class ImportDiaNNResults extends AbstractRemoteProcessingService with IImportDiaNNResultsService with LazyLogging with ISingleThreadedService {

  val singleThreadIdent: String = SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString

  override def doProcess(params: NamedParamsRetriever): Any = {
    require(params != null, "no parameter specified")

    val projectId = params.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultFileFolders = params.getString(PROCESS_METHOD.RESULT_FILES_DIR_PARAM)
    val instrumentConfigId : Long = params.getLong(PROCESS_METHOD.INSTRUMENT_CONFIG_ID_PARAM)
    val peaklistSoftwareId: Long = params.getLong(PROCESS_METHOD.PEAKLIST_SOFTWARE_ID_PARAM)

    val localPathname = MountPointRegistry.replacePossibleLabel(resultFileFolders, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname

    logger.info("Run Import DiaNN using Params : " + serialize(params)+" from "+localPathname)

    // Init execution context and ParserContext ... and call DiaNNResultsParser
    // Initialize the providers
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)
    var result : ImportedDiaNNResult = null
    try {
      val parserCtxt = ImportDiaNNResults.buildParserContext(execCtx)

      val localFile = new File(localPathname)
      if (!localFile.exists())
        throw new IllegalArgumentException("Specified Path not found on server side : " + resultFileFolders)
      if (!localFile.isDirectory)
        throw new IllegalArgumentException("Specified Path is not a directory : " + resultFileFolders)

      val parserOption = new util.HashMap[String, Object]()
      parserOption.put(DiaNNResultsParser.INSTR_CONFIG_OPTION_KEY,  java.lang.Long.valueOf(instrumentConfigId))
      parserOption.put(DiaNNResultsParser.PEAKLIST_SOFT_ID_OPTION_KEY,  java.lang.Long.valueOf(peaklistSoftwareId))
      val diannParser: DiaNNResultsParser = new DiaNNResultsParser(parserCtxt, localPathname, parserOption)
      diannParser.runService()
      val createdRSMIds = diannParser.getRSMIdByResultSetId
      val dsId = diannParser.getCreatedQuantDatasetId
      result = ImportedDiaNNResult( createdRSMIds.asScala.toMap, dsId)
      logger.debug(" Import Diann Done. {} RS imported ", createdRSMIds.size())
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }
    System.gc()
    result
  }
}

