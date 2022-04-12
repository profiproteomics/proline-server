package fr.proline.cortex.service.dps.msi

import java.io.File

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.DatabaseConnectionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msi.InferenceMethod
import fr.proline.core.algo.msi.scoring.PepSetScoring
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.SeqDbFakeProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.orm.uds.IdentificationDataset
import fr.proline.core.orm.uds.Project
import fr.proline.core.orm.uds.repository.DatasetRepository
import fr.proline.core.service.msi.{ResultFileCertifier, ResultFileImporter, ResultSetValidator, ValidationConfig}
import fr.proline.cortex.api.service.dps.msi.{IImportValidateGenerateSMService, IImportValidateGenerateSMServiceV1, IImportValidateGenerateSMServiceV2, IResultFileDescriptor, ResultFileDescriptorRuleId, ResultFileDescriptorsDecoyRegExp}
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.module.fragmentmatch.service.SpectrumMatchesGenerator

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex



/**
 * Import a result file in the MSIdb corresponding to the provided project id. Validate the imported result and optionaly 
 * generate Spectrum Matched for the validated PSM.
 * 
 * Input params :
 *  GLobal
 *   project_id : The id of the project used for data importation.
 *  Import Specific
 *   use_decoy_regexp: true if result file is formated with decoy strategy RegExp, false if it is formated with the id of the rule to be used. 
 *   result_files : The list of the result files to be imported, as IResultFileDescriptor object (format, path, peaklist_id (optional)) + protMatchDecoyRuleId or + decoyStrategy
 *   instrument_config_id : id in datastore of the instrument config used for result file acquisition
 *   peaklist_software_id : id in datastore of the software use to generate peaklist
 *   importer_properties : Map of properties for importer, specific to result files format
 * Validate Specific
 *   pep_match_filters : List of PSM filters to use (parameter, threshold and post_validation)
 *   pep_match_validator_config : PSM validation configuration (as PepMatchValidatorConfig : parameter, threshold, expectedFdr)
 *   pep_set_score_type : PeptideSet Scoring to use, one of PepSetScoring (mascot:standard score, mascot:modified mudpit score)
 *   prot_set_filters : List of ProteinSet filters to use (parameter, threshold)
 *   prot_set_validator_config : ProteinSet validation configuration  (as ProtSetValidatorConfig : validation_method, parameter, thresholds, expectedFdr)
 * Generate Spectrum Match specific
 *   generate_spectrum_matches : If true, generate fragment matches of MS/MS spectra for validated PSM.
 *   force_insert : Specify if existing spectrum matches should be replaced 
 *   
 *  Output params
 *    List of ImportedResultFileDesc : path of imported file and Id of created target RS 
 */

abstract class AbstractImportValidateGenerateSM extends AbstractRemoteProcessingService with IImportValidateGenerateSMService with ISingleThreadedService with LazyLogging {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString

  case class ImportedResultFileDesc(path: String, var targetResultSetOpt: Option[ResultSet] = None, var targetResultSetId: Long = -1L)

  //  /*
  //   * format :  The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx').
  //   * path : The relative path of the file to be imported. The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file.
  //   * decoyStrategy : The Regular expression used to detect decoy protein matches.
  //   * peaklistId : The id of the software which has been used to generate the peaklist.
  //   */
  //case class ResultFileDescriptorsDecoyRegExp (format: String, path: String, decoyStrategy: Option[String] = None, peaklistId: Option[Long] = None) extends IResultFileDescriptor
  //  /*
  //   * format :  The type of the file to be imported (for instance 'mascot.dat', 'omssa.omx').
  //   * path : The relative path of the file to be imported. The server uses a 'mount_points.result_files' label as a prefix to find the corresponding file.
  //   * protMatchDecoyRuleId : The id of the rule to be used to detect decoy protein matches.
  //   * peaklistId : The id of the software which has been used to generate the peaklist.
  //   */
  //  case class ResultFileDescriptorRuleId(format: String, path: String, peaklistId: Option[Long] = None, protMatchDecoyRuleId: Option[Long] = None) extends IResultFileDescriptor
  //

  protected def parseResultFileDescriptor(rfDescObj: Object, useDecoyRegExp: Boolean): IResultFileDescriptor = {
    if(useDecoyRegExp)
      deserialize[ResultFileDescriptorsDecoyRegExp](serialize(rfDescObj))
    else
      deserialize[ResultFileDescriptorRuleId](serialize(rfDescObj))
  }


  protected def getProtMatchDecoyRegex(resultFileDescriptor: IResultFileDescriptor, decoyRegexById: Map[Long, Regex], useDecoyRegExp: Boolean): Option[Regex] = {
    if(useDecoyRegExp){
      resultFileDescriptor.asInstanceOf[ResultFileDescriptorsDecoyRegExp].decoyStrategy.map(new Regex(_))
    } else {
      val decoyRuleIdOpt = resultFileDescriptor.asInstanceOf[ResultFileDescriptorRuleId].protMatchDecoyRuleId
      decoyRuleIdOpt.map { decoyRegexById(_) }
    }
  }

  protected def buildParserContext(executionContext: IExecutionContext): ProviderDecoratedExecutionContext = {

    // Register some providers
    val parserContext = ProviderDecoratedExecutionContext(PeptideCacheExecutionContext(executionContext)) // Use Object factory

    // TODO: use real protein and seqDb providers
    parserContext.putProvider(classOf[IProteinProvider], ProteinFakeProvider)
    parserContext.putProvider(classOf[ISeqDatabaseProvider], SeqDbFakeProvider)

    val msiSQLCtx = executionContext.getMSIDbConnectionContext
    val sqlPTMProvider = new SQLPTMProvider(msiSQLCtx)
    parserContext.putProvider(classOf[IPTMProvider], sqlPTMProvider)

    val sqlPepProvider = new SQLPeptideProvider(PeptideCacheExecutionContext(parserContext))
    parserContext.putProvider(classOf[IPeptideProvider], sqlPepProvider)

    parserContext
  }

  protected def isValidationEnabled(params: NamedParamsRetriever) : Boolean

  /* Define the concrete doProcess method */
  override def doProcess(params: NamedParamsRetriever): Object = {
    require(params != null, "no parameter specified")

    // **** Get all parameters
    val projectId = params.getLong(PROJECT_ID_PARAM)

    // Import parameters
    val useDecoyRegExp = params.getBoolean(USE_DECOY_REGEX_PARAM)
    val resultFiles = params.getList(RESULT_FILES_PARAM).toArray.map { rfd => parseResultFileDescriptor(rfd,useDecoyRegExp) }
    val instrumentConfigId = params.getLong(INSTRUMENT_CONFIG_ID_PARAM)
    val peaklistSoftwareId = params.getLong(PEAKLIST_SOFTWARE_ID_PARAM)
    val importerProperties = if (!params.hasParam(IMPORTER_PROPERTIES_PARAM)) Map.empty[String, Any]
    else params.getMap(IMPORTER_PROPERTIES_PARAM).map {
      case (a, b) => {
        if (a.endsWith(".file")) {
          a -> MountPointRegistry.replacePossibleLabel(b.toString, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
        } else a -> b.asInstanceOf[Any]
      }
    } toMap

    // Is Validation enabled ?
    val doValidation = isValidationEnabled(params)

    // Generate Spectrum Matches parameters
    val generateSpectrumMatches = if (!doValidation || !params.hasParam(GENERATE_SPECTRUM_MATCHES_PARAM)) false
    else params.getBoolean(GENERATE_SPECTRUM_MATCHES_PARAM)

    val gsmForceInsert = if (!params.hasParam(FORCE_INSERT_PARAM)) false
    else params.getBoolean(FORCE_INSERT_PARAM)

    // Validation parameters
//    val doValidation = if(!params.hasParam(PROCESS_METHOD.VALIDATE_PARAM)) false
//    else params.getBoolean(PROCESS_METHOD.VALIDATE_PARAM)

    val validationParamOpt = if(doValidation) { Some(ValidateResultSet.parseValidationConfig(params) )  }
    else None

    logger.debug("Params : " + serialize(params))

    var resultImport: Array[ImportedResultFileDesc] = null
    var resultValidate: ArrayBuffer[java.lang.Long] = new ArrayBuffer[java.lang.Long]

    // *** Initialize the providers
    val execCtx =DbConnectionHelper.createJPAExecutionContext(projectId)
    var msiDbConnectionContext: DatabaseConnectionContext = null
    var transactionOk: Boolean = false
    var certifyResult: Boolean = false

    try {
      val parserCtx = this.buildParserContext(execCtx)
      val udsbConnectionContext = execCtx.getUDSDbConnectionContext

      val udsDbHelper = new UdsDbHelper(udsbConnectionContext)
      val decoyRegexById = udsDbHelper.getProteinMatchDecoyRegexById()

      val importedRFs = new collection.mutable.ArrayBuffer[ImportedResultFileDesc]

      // Begin transaction
      msiDbConnectionContext = execCtx.getMSIDbConnectionContext
      msiDbConnectionContext.beginTransaction() // Start a transaction on MSI Db
      udsbConnectionContext.beginTransaction()
      transactionOk = false

      val filesByFormat = resultFiles.groupBy(_.format).mapValues(_.map(rf =>
      {
        val localPathname = MountPointRegistry.replacePossibleLabel(rf.path, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname

        new File(localPathname)
      } ))

      // Instantiate the ResultFileCertifier service
      val rsCertifier = new ResultFileCertifier(
        executionContext = parserCtx,
        resultIdentFilesByFormat = filesByFormat,
        importProperties = importerProperties
      )
      rsCertifier.run()
      certifyResult = true


      // **** Go through each result File
      for (resultFile <- resultFiles) {

        val resultFileType = resultFile.format
        val importedRF = new ImportedResultFileDesc(resultFile.path)

        // Instantiate the appropriate result file provider and register it
        val rfProviderOpt = ResultFileProviderRegistry.get(resultFileType)
        if (rfProviderOpt.isEmpty) throw new Exception("unsupported result file type: " + resultFileType)
        val rfProvider = rfProviderOpt.get

        val acDecoyRegex = getProtMatchDecoyRegex(resultFile, decoyRegexById, useDecoyRegExp)
        if (acDecoyRegex.isDefined) {
          this.logger.info("parsing concatenated decoy results with AC regex=[" + acDecoyRegex.get + ']')
        }

        val localPathname = MountPointRegistry.replacePossibleLabel(resultFile.path, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname

        // **** Import Result File
        // Instantiate the ResultFileImporter service
        val rsImporter = new ResultFileImporter(
          executionContext = parserCtx,
          resultIdentFile = new File(localPathname),
          fileType = rfProvider.fileType,
          instrumentConfigId = instrumentConfigId,
          peaklistSoftwareId = peaklistSoftwareId,
          importerProperties = importerProperties,
          acDecoyRegex = acDecoyRegex,
          storeSpectrumMatches = false
        )
        rsImporter.run()

        val currentRSId =rsImporter.getTargetResultSetId
        val currentRSOPt =  rsImporter.getTargetResultSetOpt
        val currentRS = currentRSOPt.get
        importedRF.targetResultSetId = currentRSId
        importedRF.targetResultSetOpt = currentRSOPt
        importedRFs += importedRF

        /* Update result */
        resultImport = importedRFs.toArray
        val dsName = if(currentRS.msiSearch.isDefined){
          var name = currentRS.msiSearch.get.resultFileName
          val indexOfDot = name.lastIndexOf('.')
          if (indexOfDot != -1) {
            name = name.substring(0, indexOfDot)
          }
          name
        } else { currentRS.name }

        //*** Create Identification Dataset for new RS
        // Then insert it in the current MSIdb
        val udsEM = udsbConnectionContext.getEntityManager
        val project = udsEM.find(classOf[Project], projectId)
        val ds : IdentificationDataset = new IdentificationDataset()
        ds.setProject(project)
        ds.setName(dsName)
        ds.setResultSetId(currentRSId)
        ds.setChildrenCount(0)
        val rootDsNames = DatasetRepository.findRootDatasetNamesByProject(udsEM,projectId)
        val dsNbr = if(rootDsNames == null) 0 else rootDsNames.size()
        ds.setNumber(dsNbr)

        if(doValidation) {
          // **** Validate Result Set
          val rsValidator = ResultSetValidator(
            execContext = execCtx,
            targetRs = currentRS,
            validationConfig = validationParamOpt.get.copy(pepSetScoring = Some(validationParamOpt.get.pepSetScoring.getOrElse(PepSetScoring.MASCOT_STANDARD_SCORE))),
            inferenceMethod = Some(InferenceMethod.PARSIMONIOUS),
            storeResultSummary = true,
            propagatePepMatchValidation = false,
            propagateProtSetValidation = false
          )
          rsValidator.run()
          val currentRSMId = rsValidator.validatedTargetRsm.id
          resultValidate += currentRSMId

          //Complete DS info and persist it
          ds.setResultSummaryId(currentRSMId)
          udsEM.persist(ds)

          if (generateSpectrumMatches) {
            val spectrumMatchesGenerator = new SpectrumMatchesGenerator(execCtx, currentRSId, Some(currentRSMId), None, None, gsmForceInsert)
            spectrumMatchesGenerator.runService()
          }
        }
      } // End Go through RS

      // Commit transaction
      msiDbConnectionContext.commitTransaction()
      udsbConnectionContext.commitTransaction()
      transactionOk = true

    } catch {
      case t: Throwable => {
        val errorMsgPrefix = "Error while running <Import Validate and Spectrum Matches Generator>"
        val errorMessage = if (!certifyResult) { // Certify Not finished
          s"$errorMsgPrefix (certify result files failed)"
        } else errorMsgPrefix

        throw ExceptionUtils.wrapThrowable(errorMessage, t, appendCause = true)
      }

    } finally {

      if (!transactionOk) {
        DbConnectionHelper.tryToRollbackDbTransaction(msiDbConnectionContext)
      }

      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    resultImport
  }

}

class ImportValidateGenerateSMV1 extends AbstractImportValidateGenerateSM with IImportValidateGenerateSMServiceV1  {
  override protected def isValidationEnabled(params: NamedParamsRetriever): Boolean = true
}

class ImportValidateGenerateSMV2 extends AbstractImportValidateGenerateSM with IImportValidateGenerateSMServiceV2 {
  override protected def isValidationEnabled(params: NamedParamsRetriever): Boolean = {
        if(!params.hasParam(PROCESS_METHOD.VALIDATE_PARAM))
          false
        else
          params.getBoolean(PROCESS_METHOD.VALIDATE_PARAM)
  }
}


