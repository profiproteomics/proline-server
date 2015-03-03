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

class CertifyResultFiles extends AbstractRemoteProcessService with Logging {

  /* JMS Service identification */
  val serviceName = "proline/dps/msi/CertifyResultFiles"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  case class ResultFileDescriptor(format: String, path: String)

  //    // Configure service interface
  //  val wsParams = Array(
  //    MethodParam(
  //      "project_id",
  //      JSONType.Integer,
  //      description = Some("The id of the project used for data importation."),
  //      scalaType = Some(typeOf[Long])
  //    ),
  //    MethodParam(
  //      "result_files",
  //      JSONType.Array,
  //      description = Some("The list of the result files to be imported."),
  //      scalaType = Some(typeOf[Array[ResultFileDescriptor]])
  //    ),
  //    MethodParam(
  //      "importer_properties",
  //      JSONType.Object,
  //      description = Some("Properties map specific to result file type."),
  //      optional = true,
  //      scalaType = Some(typeOf[Map[String, Any]])
  //    )
  //  )

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "ParamsRetriever is null")

    var processResult: String = null

    val projectId = paramsRetriever.getLong("project_id")
    val resultFiles = paramsRetriever.getList("result_files").toArray.map { rfd => deserialize[ResultFileDescriptor](serialize(rfd)) }

    val importerProperties = if (paramsRetriever.hasParam("importer_properties") == false) Map.empty[String, Any]
    else paramsRetriever.getMap("importer_properties").map {
      case (a, b) => {
        if (a.endsWith(".file")) {
          a -> MountPointRegistry.replacePossibleLabel(b.toString(), Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
        } else a -> b.asInstanceOf[Any]
      }
    } toMap

    // Initialize the providers    
    val execCtx = BuildExecutionContext(DataStoreConnectorFactory.getInstance(), projectId, true) // Use JPA context

    try {
      val parserCtx = buildParserContext(execCtx)

      val filesByFormat = resultFiles.groupBy(_.format).mapValues(_.map(rf =>
        {
          val localPathname = MountPointRegistry.replacePossibleLabel(rf.path, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname

          new File(localPathname)
        }
      ))

      for (format <- filesByFormat.keys) {

        // Instantiate the appropriate result file provider and register it
        val rfProviderOpt = ResultFileProviderRegistry.get(format)
        if (rfProviderOpt.isEmpty) throw new Exception("unsupported result file type: " + format)
        val rfProvider = rfProviderOpt.get
      }

      var certifyResult: Boolean = false
      val errorMessage = new StringBuilder()

      try {
        // Instantiate the ResultFileImporter service
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