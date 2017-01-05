package fr.proline.cortex.service.dps.msi

import java.io.File
import scala.collection.JavaConversions.mapAsScalaMap
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
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
import fr.proline.core.service.msi.ResultFileCertifier
import fr.proline.cortex.api.service.dps.msi.ICertifyResultFilesService
import fr.proline.cortex.api.service.dps.msi.ResultFileDescriptorRuleId
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  Define JMS Service to :
 *  Verify result files integrity before importing them in the MSIdb. This service should be called before the service ImportResultFiles.
 *  
 *  Input Params :
 *    project_id : The id of the project used for data importation.
 *    result_files : The list of the result files to be imported as ResultFileDescriptor
 *    importer_properties : Properties map specific to result file type.
 *    
 *  Output Params :
 *    "OK" if service run successfuly
 *    Error message if service was not successfull
 */
class CertifyResultFiles extends AbstractRemoteProcessingService with ICertifyResultFilesService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

   require((paramsRetriever != null), "no parameter specified")

    var processResult: String = null

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultFiles = paramsRetriever.getList(PROCESS_METHOD.RESULT_FILES_PARAM).toArray.map { rfd => deserialize[ResultFileDescriptorRuleId](serialize(rfd)) }

    val importerProperties = if (paramsRetriever.hasParam(PROCESS_METHOD.IMPORTER_PROPERTIES_PARAM) == false) Map.empty[String, Any]
    else paramsRetriever.getMap(PROCESS_METHOD.IMPORTER_PROPERTIES_PARAM).map {
      case (a, b) => {
        if (a.endsWith(".file")) {
          a -> MountPointRegistry.replacePossibleLabel(b.toString(), Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
        } else a -> b.asInstanceOf[Any]
      }
    } toMap

    // Initialize the providers    
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context

    try {
      val parserCtx = buildParserContext(execCtx)

      val filesByFormat = resultFiles.groupBy(_.format).mapValues(_.map(rf =>
        {
          val localPathname = MountPointRegistry.replacePossibleLabel(rf.path, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname

          new File(localPathname)
        }
      ))

//      for (format <- filesByFormat.keys) {
//
//        // Instantiate the appropriate result file provider and register it
//        val rfProviderOpt = ResultFileProviderRegistry.get(format)
//        if (rfProviderOpt.isEmpty) throw new Exception("unsupported result file type: " + format)
//        val rfProvider = rfProviderOpt.get
//      }

      var certifyResult: Boolean = false
      val errorMessage = new StringBuilder()

      try {
        // Instantiate the ResultFileCertifier service
        val rsCertifier = new ResultFileCertifier(
          executionContext = parserCtx,
          resultIdentFilesByFormat = filesByFormat,
          importProperties = importerProperties
        )

        rsCertifier.run()

        certifyResult = true
      } catch {

        case t: Throwable => {
          val message = "Error certifying ResultFiles"

          logger.error(message, t)

          errorMessage.append(message).append(" : ").append(t)
          errorMessage.append(StringUtils.LINE_SEPARATOR)

          errorMessage.append(t.getStackTraceString)
        }

      }

      processResult = if (certifyResult) {
        "OK" // ResultFileCertifier success
      } else {
        errorMessage.toString // ResultFileCertifier complete abruptly
      }

    } finally {

      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }

    }

    processResult
  }

  private def buildParserContext(executionContext: IExecutionContext): ProviderDecoratedExecutionContext = {

    // Register some providers
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    // TODO: use real protein and seqDb providers
    parserContext.putProvider(classOf[IProteinProvider], ProteinFakeProvider)
    parserContext.putProvider(classOf[ISeqDatabaseProvider], SeqDbFakeProvider)

    val psSQLCtx = executionContext.getPSDbConnectionContext
    val sqlPTMProvider = new SQLPTMProvider(psSQLCtx)
    parserContext.putProvider(classOf[IPTMProvider], sqlPTMProvider)

    val sqlPepProvider = new SQLPeptideProvider(psSQLCtx)
    parserContext.putProvider(classOf[IPeptideProvider], sqlPepProvider)

    parserContext
  }

}