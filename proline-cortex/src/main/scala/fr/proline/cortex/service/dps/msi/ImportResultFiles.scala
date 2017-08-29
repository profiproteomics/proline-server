package fr.proline.cortex.service.dps.msi

import java.io.File

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.{deserialize, serialize}
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi._
import fr.proline.core.om.provider.msi.impl.{SQLPTMProvider, SQLPeptideProvider}
import fr.proline.core.service.msi.ResultFileImporter
import fr.proline.cortex.api.service.dps.msi._
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.{AbstractRemoteProcessingService, ISingleThreadedService}

import scala.collection.JavaConversions.mapAsScalaMap
import scala.language.postfixOps
import scala.util.matching.Regex

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
abstract class AbstractImportResultFiles extends AbstractRemoteProcessingService with IImportResultFilesService with ISingleThreadedService with LazyLogging  {
  
  /* JMS Service identification */
  val singleThreadIdent= SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString()

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
  def doProcess(params: NamedParamsRetriever): Any = {
    require(params != null, "no parameter specified")
    
    val projectId = params.getLong(IMPORT_PROJECT_ID_PARAM)
    val resultFiles = params.getList(RESULT_FILES_PARAM).toArray.map { rfd => parseResultFileDescriptor(rfd) }
    val instrumentConfigId = params.getLong(INSTRUMENT_CONFIG_ID_PARAM)
    val peaklistSoftwareId = params.getLong(PEAKLIST_SOFTWARE_ID_PARAM)
    val importerProperties = if (params.hasParam(IMPORTER_PROPERTIES_PARAM) == false) Map.empty[String, Any]
    else params.getMap(IMPORTER_PROPERTIES_PARAM).map {
      case (a, b) => {
        if (a.endsWith(".file")) {
          a -> MountPointRegistry.replacePossibleLabel(b.toString(), Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
        } else a -> b.asInstanceOf[Any]
      }
    } toMap

    val storeSpectrumMatches = if (params.hasParam(SAVE_SPECTRUM_MATCHES_PARAM) == false) false
    else params.getBoolean(SAVE_SPECTRUM_MATCHES_PARAM)

    logger.info("Params : " + serialize(params))

    //val specTitleRuleId = params.getInt("spec_title_rule_id")
    //val peaklistId = params.getInt("peaklist_id")

    var result: Array[ImportedResultFile] = null

    // Initialize the providers    
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

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
          storeSpectrumMatches = storeSpectrumMatches
        )
        rsImporter.run()

        importedRF.targetResultSetId = rsImporter.getTargetResultSetId

        importedRFs += importedRF

        /* Update result */
        result = importedRFs.toArray

      }

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    result
  }

}

  
class ImportResultFilesDecoyRegExp extends AbstractImportResultFiles with IImportResultFilesServiceV1_0 {
  
  protected def parseResultFileDescriptor(rfDescObj: Object): IResultFileDescriptor = {
    deserialize[ResultFileDescriptorsDecoyRegExp](serialize(rfDescObj))
  }

  protected def getProtMatchDecoyRegex(resultFileDescriptor: IResultFileDescriptor, decoyRegexById: Map[Long, Regex]): Option[Regex] = {
    resultFileDescriptor.asInstanceOf[ResultFileDescriptorsDecoyRegExp].decoyStrategy.map(new Regex(_))
  }
}

class ImportResultFilesProtMatchDecoyRule extends AbstractImportResultFiles with IImportResultFilesServiceV2_0 {


  protected def parseResultFileDescriptor(rfDescObj: Object): IResultFileDescriptor = {
    deserialize[ResultFileDescriptorRuleId](serialize(rfDescObj))
  }

  protected def getProtMatchDecoyRegex(resultFileDescriptor: IResultFileDescriptor, decoyRegexById: Map[Long, Regex]): Option[Regex] = {
    val decoyRuleIdOpt = resultFileDescriptor.asInstanceOf[ResultFileDescriptorRuleId].protMatchDecoyRuleId
    decoyRuleIdOpt.map { decoyRegexById(_) }
  }
}
