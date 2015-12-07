package fr.proline.cortex.service.dps.msi

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import scala.util.matching.Regex
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import scala.collection.JavaConversions.mapAsScalaMap
import fr.profi.util.serialization.ProfiJson._
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.core.service.msi.ResultFileImporter
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import java.io.File
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.provider.msi.SeqDbFakeProvider
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessService
import fr.proline.jms.service.api.ISingleThreadedService

trait IResultFileDescriptor {

  val format: String
  val path: String
  val peaklistId: Option[Long]
}

/**
 * Import a result file in the MSIdb corresponding to the provided project id
 * 
 * Input params
 *   project_id : The id of the project used for data importation.
 *   result_files : The list of the result files to be imported, as IResultFileDescriptor object
 *   instrument_config_id : id in datastore of the instrument config used for result file acquisition
 *   peaklist_software_id : id in datastore of the software use to generate peaklist
 *   save_spectrum_matches : If true, fragment matches of MS/MS spectra will be stored in the MSIdb.
 *   importer_properties : Map of properties for importer, specific to result files format
 *   
 *  Output params
 *    List of ImportedResultFile : path of imported file and Id of created target RS 
 */
abstract class AbstractImportResultFiles extends AbstractRemoteProcessService with LazyLogging with ISingleThreadedService {
  /* JMS Service identification */
  val serviceName = "proline/dps/msi/ImportResultFiles"

  case class ImportedResultFile(path: String, var targetResultSetId: Long = -1L)


  // Methods to be implemented
  protected def parseResultFileDescriptor(rfDescObj: Object): IResultFileDescriptor

  protected def getProtMatchDecoyRegex(resultFileDescriptor: IResultFileDescriptor, decoyRegexById: Map[Long, Regex]): Option[Regex]

  private def buildParserContext(executionContext: IExecutionContext): ProviderDecoratedExecutionContext = {

    // Register some providers
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    //parserContext.putProvider(classOf[IPeptideProvider], PeptideFakeProvider)

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

  /* Define the concrete doProcess method */
  override def doProcess(params: NamedParamsRetriever): Object = {
    require((params != null), "no parameter specified")
    
    val projectId = params.getLong("project_id")
    val resultFiles = params.getList("result_files").toArray.map { rfd => parseResultFileDescriptor(rfd) }
    val instrumentConfigId = params.getLong("instrument_config_id")
    val peaklistSoftwareId = params.getLong("peaklist_software_id")
    val importerProperties = if (params.hasParam("importer_properties") == false) Map.empty[String, Any]
    else params.getMap("importer_properties").map {
      case (a, b) => {
        if (a.endsWith(".file")) {
          a -> MountPointRegistry.replacePossibleLabel(b.toString(), Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
        } else a -> b.asInstanceOf[Any]
      }
    } toMap

    val saveSpectrumMatches = if (params.hasParam("save_spectrum_matches") == false) false
    else params.getBoolean("save_spectrum_matches")

    logger.info("Params : " + serialize(params))

    //val specTitleRuleId = params.getInt("spec_title_rule_id")
    //val peaklistId = params.getInt("peaklist_id")

    var result: Array[ImportedResultFile] = null

    // Initialize the providers    
    val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), projectId, false)

    try {
      val parserCtx = this.buildParserContext(execCtx)
      val udsDbCtx = execCtx.getUDSDbConnectionContext()

      val udsDbHelper = new UdsDbHelper(udsDbCtx)
      val decoyRegexById = udsDbHelper.getProteinMatchDecoyRegexById()

      val importedRFs = new collection.mutable.ArrayBuffer[ImportedResultFile]

      for (resultFile <- resultFiles) {
        val resultFileType = resultFile.format
        val importedRF = new ImportedResultFile(resultFile.path)

        // Instantiate the appropriate result file provider and register it
        val rfProviderOpt = ResultFileProviderRegistry.get(resultFileType)
        if (rfProviderOpt.isEmpty) throw new Exception("unsupported result file type: " + resultFileType)

        val rfProvider = rfProviderOpt.get

        val acDecoyRegex = getProtMatchDecoyRegex(resultFile, decoyRegexById)
        if (acDecoyRegex.isDefined) {
          this.logger.info("parsing concatenated decoy results with AC regex=[" + acDecoyRegex.get + ']')
        }

        val localPathname = MountPointRegistry.replacePossibleLabel(resultFile.path, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname

        // Instantiate the ResultFileImporter service
        val rsImporter = new ResultFileImporter(
          executionContext = parserCtx,
          resultIdentFile = new File(localPathname),
          fileType = rfProvider.fileType,
          instrumentConfigId = instrumentConfigId,
          peaklistSoftwareId = peaklistSoftwareId,
          importerProperties = importerProperties,
          acDecoyRegex = acDecoyRegex,
          saveSpectrumMatches
        )
        rsImporter.run()

        importedRF.targetResultSetId = rsImporter.getTargetResultSetId

        importedRFs += importedRF

        /* Update result */
        result = importedRFs.toArray

      }

    } finally {
      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    System.gc()

    result
  }

}


  /*
   * format :  The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx').
   * path : The relative path of the file to be imported. The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file.
   * decoyStrategy : The Regular expression used to detect decoy protein matches.
   * peaklistId : The id of the software which has been used to generate the peaklist.
   */
case class ResultFileDescriptorsDecoyRegExp (format: String, path: String, decoyStrategy: Option[String] = None, peaklistId: Option[Long] = None) extends IResultFileDescriptor

  
class ImportResultFilesDecoyRegExp extends AbstractImportResultFiles {
  /* JMS Service identification */
  val serviceVersion = "1.0"
  override val defaultVersion = true


  protected def parseResultFileDescriptor(rfDescObj: Object): IResultFileDescriptor = {
    deserialize[ResultFileDescriptorsDecoyRegExp](serialize(rfDescObj))
  }

  protected def getProtMatchDecoyRegex(resultFileDescriptor: IResultFileDescriptor, decoyRegexById: Map[Long, Regex]): Option[Regex] = {
    resultFileDescriptor.asInstanceOf[ResultFileDescriptorsDecoyRegExp].decoyStrategy.map(new Regex(_))
  }
}


  /*
   * format :  The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx').
   * path : The relative path of the file to be imported. The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file.
   * protMatchDecoyRuleId : The id of the rule to be used to detect decoy protein matches.
   * peaklistId : The id of the software which has been used to generate the peaklist.
   */
  case class ResultFileDescriptorRuleId(format: String, path: String, peaklistId: Option[Long] = None, protMatchDecoyRuleId: Option[Long] = None) extends IResultFileDescriptor

  
class ImportResultFilesprotMatchDecoyRule extends AbstractImportResultFiles {
  /* JMS Service identification */
  val serviceVersion = "2.0"
  override val defaultVersion = false

  protected def parseResultFileDescriptor(rfDescObj: Object): IResultFileDescriptor = {
    deserialize[ResultFileDescriptorRuleId](serialize(rfDescObj))
  }

  protected def getProtMatchDecoyRegex(resultFileDescriptor: IResultFileDescriptor, decoyRegexById: Map[Long, Regex]): Option[Regex] = {
    val decoyRuleIdOpt = resultFileDescriptor.asInstanceOf[ResultFileDescriptorRuleId].protMatchDecoyRuleId
    decoyRuleIdOpt.map { decoyRegexById(_) }
  }
}
