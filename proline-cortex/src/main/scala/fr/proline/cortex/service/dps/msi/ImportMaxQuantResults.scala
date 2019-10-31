package fr.proline.cortex.service.dps.msi

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import org.apache.commons.io.FilenameUtils
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.context.execCtxToTxExecCtx
import fr.proline.core.om.model.lcms.MapSet
import fr.proline.core.om.model.msq.BiologicalGroup
import fr.proline.core.om.model.msq.BiologicalSample
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.model.msq.GroupSetup
import fr.proline.core.om.model.msq.MasterQuantChannel
import fr.proline.core.om.model.msq.QuantChannel
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.lcms.IRunProvider
import fr.proline.core.om.provider.lcms.impl.SQLRunProvider
import fr.proline.core.om.provider.lcms.impl.SQLScanSequenceProvider
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.provider.msi.SeqDbFakeProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.orm.uds.{Dataset => UdsDataset}
import fr.proline.core.service.lcms.io.IMapSetBuilder
import fr.proline.core.service.msi.ResultSetValidator
import fr.proline.core.service.msq.quantify.ThirdPartyLabelFreeFeatureQuantifier
import fr.proline.core.service.uds.CreateQuantitation
import fr.proline.cortex.api.service.dps.msi.IImportMaxQuantResultsService
import fr.proline.cortex.api.service.dps.msi.IImportMaxQuantResultsServiceV2
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.module.parser.maxquant.MaxQuantResultParser
import fr.proline.core.orm.uds.RawFile
import fr.proline.core.orm.uds.Run
import fr.proline.core.orm.uds.Project
import fr.proline.core.orm.uds.InstrumentConfiguration
import fr.proline.core.om.storer.lcms.RawMapStorer
import fr.proline.context.LcMsDbConnectionContext
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.orm.lcms.{Scan, ScanSequence}

import scala.collection.mutable

/**
  * Import MaxQuant result file in the MSIdb corresponding to the provided project id
  *
  * Input params
  *   project_id : The id of the project used for data importation.
  *   result_files_dir : The path to folder or Zip file containing Result files to be imported.
  *   instrument_config_id : id in datastore of the instrument config used for result file acquisition
  *   peaklist_software_id : id in datastore of the software use to generate peaklist
  *
  *
  *  Output params
  *    Following map corresponding to import result
  *    - result_set_Ids : List of all created result set Ids
  *    - warning_msg : information message that could be of interest for to caller (even up to GUI)...
  */

object ImportMaxQuantResults extends LazyLogging {

  def buildParserContext(executionContext: IExecutionContext): ProviderDecoratedExecutionContext = {

    // Register some providers
    val parserContext = ProviderDecoratedExecutionContext(PeptideCacheExecutionContext(executionContext)) // Use Object factory and use PeptideCache

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

  /**
    * Unzip maxQuant result file
    * @param zipFile input zip file
    * @return the root folder name
    */
  def unZipIt(zipFile: File): String = {

    val buffer: Array[Byte] = new Array[Byte](4096)
    var zis: ZipInputStream = null
    try {

      //create output directory
      val parentFolder = zipFile.getParentFile
      val folderName = FilenameUtils.getBaseName(zipFile.getAbsolutePath)
      val folder = new File(parentFolder, folderName + "_unzip")
      if (!folder.exists()) {
        folder.mkdir()
      }

      //get the zip file content
      zis = new ZipInputStream(new FileInputStream(zipFile))
      //get the zipped file list entry
      var ze: ZipEntry = zis.getNextEntry

      while (ze != null) {

        val nextFileName = ze.getName
        val newFile = new File(folder, nextFileName)
        logger.debug("file unzip : " + newFile.getAbsoluteFile)

        //create all non exists folders
        //else you will hit FileNotFoundException for compressed folder
        new File(newFile.getParent).mkdirs()

        val fos: FileOutputStream = new FileOutputStream(newFile)
        var len = zis.read(buffer)
        while (len > 0) {
          fos.write(buffer, 0, len)
          len = zis.read(buffer)
        }
        fos.close()
        ze = zis.getNextEntry
      }

      zis.closeEntry()
      zis.close()

      logger.debug("All zip entry unzip : " + zipFile.getName)

      if (folder.list().length == 1) //root folder in ZIP
        return folder.listFiles()(0).getAbsolutePath
      else
        return folder.getAbsolutePath

    } catch {
      case e: IOException =>
      {
        logger.debug("exception caught: " + e.getMessage)
        if (zis != null) {
          try {
            zis.closeEntry()
            zis.close()
          } catch {
            case e: IOException => logger.error("Error closing ZIP : " + zipFile.getName)
          }
        }
      }

        return null
    }
  }

}

class ImportMaxQuantResults extends AbstractRemoteProcessingService with IImportMaxQuantResultsService with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString

  def doProcess(params: NamedParamsRetriever): Any = {
    require(params != null, "no parameter specified")

    val projectId = params.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultFileFolders = params.getString(PROCESS_METHOD.RESULT_FILES_DIR_PARAM)
    val instrumentConfigId = params.getLong(PROCESS_METHOD.INSTRUMENT_CONFIG_ID_PARAM)
    val peaklistSoftwareId = params.getLong(PROCESS_METHOD.PEAKLIST_SOFTWARE_ID_PARAM)

    logger.info("Params : " + serialize(params))

    val result: Map[String, Object] = Map.empty

    // Initialize the providers
    val execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      val parserCtx = ImportMaxQuantResults.buildParserContext(execCtx)
      val udsDbCtx = execCtx.getUDSDbConnectionContext

      var localPathname = MountPointRegistry.replacePossibleLabel(resultFileFolders, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
      var mqFolderName = localPathname
      val localFile = new File(localPathname)
      if (!localFile.exists())
        throw new IllegalArgumentException("Specified Path not found on server side : " + resultFileFolders)

      if (!localFile.isDirectory) { //Should be a zip file !
        mqFolderName = ImportMaxQuantResults.unZipIt(localFile)
      }
      logger.debug("TRY to Import MaxQuant result on: " + mqFolderName)

      // Instantiate the ResultFileImporter service
      val rsMQImporter = new MaxQuantResultParser(parserCtx,
        instrumentConfigId,
        peaklistSoftwareId,
        mqFolderName
      )
      rsMQImporter.run()
      val rsIds = rsMQImporter.getCreatedResultSetIds
      result.put("result_set_Ids", rsIds.keySet.toList)
      result.put("warning_msg", rsMQImporter.getWarningMessage)
      logger.debug("Import MaxQuant result done : " + result)

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    System.gc()

    result
  }
}

class ImportMaxQuantResultsV2_0 extends AbstractRemoteProcessingService with IImportMaxQuantResultsServiceV2 with IMapSetBuilder with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString
  var rsMQImporterOpt: Option[MaxQuantResultParser] = None
  val pseudoScanByRsName: mutable.Map[String, Long] = scala.collection.mutable.Map[String, Long]()

  /* Define the concrete doProcess method */
  def doProcess(params: NamedParamsRetriever): Any = {
    require(params != null, "no parameter specified")

    val projectId = params.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultFileFolders = params.getString(PROCESS_METHOD.RESULT_FILES_DIR_PARAM)
    val instrumentConfigId = params.getLong(PROCESS_METHOD.INSTRUMENT_CONFIG_ID_PARAM)
    val accessionRegexp = if (params.hasParam(PROCESS_METHOD.ACCESSION_REGEXP_PARAM)) params.getString(PROCESS_METHOD.ACCESSION_REGEXP_PARAM) else "(.*)"
    val importQuantitation = if (params.hasParam(PROCESS_METHOD.IMPORT_QUANT_RESULTS_PARAM)) params.getBoolean(PROCESS_METHOD.IMPORT_QUANT_RESULTS_PARAM) else false
    val fragmentationRuleSetId = if (params.hasParam(PROCESS_METHOD.FRAGMENTATION_RULE_SET_ID_PARAM)) params.getLong(PROCESS_METHOD.FRAGMENTATION_RULE_SET_ID_PARAM) else -1

    logger.info("Params : " + serialize(params))

    var result: Map[String, Object] = Map.empty

    // Initialize the providers    
    var execCtx = DbConnectionHelper.createSQLExecutionContext(projectId)

    try {
      val parserCtx = ImportMaxQuantResults.buildParserContext(execCtx)
      var udsDbCtx = execCtx.getUDSDbConnectionContext

      var localPathname = MountPointRegistry.replacePossibleLabel(resultFileFolders, Some(MountPointRegistry.RESULT_FILES_DIRECTORY)).localPathname
      var mqFolderName = localPathname
      val localFile = new File(localPathname)
      if (!localFile.exists())
        throw new IllegalArgumentException("Specified Path not found on server side : " + resultFileFolders)

      if (!localFile.isDirectory) { //Should be a zip file !
        mqFolderName = ImportMaxQuantResults.unZipIt(localFile)
      }
      logger.debug("TRY to Import MaxQuant result on: " + mqFolderName)

      // Instantiate the ResultFileImporter service
      rsMQImporterOpt = Some(new MaxQuantResultParser(parserCtx,
        instrumentConfigId,
        accessionRegexp,
        mqFolderName,
        fragmentationRuleSetId
      ))

      val rsMQImporter = rsMQImporterOpt.get
      rsMQImporter.run()


      val rsIds = rsMQImporter.getCreatedResultSetIds
      result.put("result_set_Ids", rsIds.keySet.toList)
      result.put("warning_msg", rsMQImporter.getWarningMessage)


      if (importQuantitation) {

        val rsmIds: Map[String, Long] = Map()
        val rsmIdsByRsId: Map[Long, Long] = Map()

        for ((rsId, rsName) <- rsIds.asScala) {

          val rsValidator = ResultSetValidator(
            execContext = execCtx,
            targetRsId = rsId
          )
          rsValidator.run()
          rsmIds += (rsName -> rsValidator.targetRSMIdPerRsId(rsId))
          rsmIdsByRsId += (rsId.longValue() -> rsValidator.targetRSMIdPerRsId(rsId))
        }

        result.put("result_summary_ids_by_result_set_id", rsmIdsByRsId)

        //TODO : understand why JPAExecution context did'nt work for the MQ Import
        // Workaround is : use a SQL context for import then close it and open a JPA context for Quantification

        DbConnectionHelper.tryToCloseExecContext(execCtx)

        execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)
        udsDbCtx = execCtx.getUDSDbConnectionContext

        // Register SQLRunProvider 
        val scanSeqProvider = new SQLScanSequenceProvider(execCtx.getLCMSDbConnectionContext)
        val lcMsRunProvider = new SQLRunProvider(
          udsDbCtx,
          Some(scanSeqProvider),
          None
        )

        val providerContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory
        providerContext.putProvider(classOf[IRunProvider], lcMsRunProvider)

        providerContext.tryInTransactions(lcmsTx = true, udsTx = true, msiTx = true, txWork = {

          val quantChannels = ArrayBuffer[QuantChannel]()
          var count = 0
          // Search for or Create the RawFile


          for ((rsName, rsmId) <- rsmIds) {

            val udsEM =  execCtx.getUDSDbConnectionContext.getEntityManager
            val lcmsEM = execCtx.getLCMSDbConnectionContext.getEntityManager

            var rawFile = udsEM.find(classOf[RawFile], rsName)
            val project = udsEM.find(classOf[Project], projectId)
            val instrumentConfig = udsEM.find(classOf[InstrumentConfiguration], instrumentConfigId)

            val udsRun = if (rawFile == null) {
              rawFile = new RawFile()
              rawFile.setIdentifier(rsName)
              rawFile.setOwnerId(project.getOwner.getId)
              rawFile.setRawFileName(rsName)
              udsEM.persist(rawFile)
              // Create Run
              val newUdsRun = new Run()
              newUdsRun.setNumber(1)
              newUdsRun.setRunStart(0f)
              newUdsRun.setRunStop(0f)
              newUdsRun.setDuration(0f)
              newUdsRun.setRawFile(rawFile)
              udsEM.persist(newUdsRun)
              newUdsRun
            } else {
              //TODO : can getRuns be null or empty
              logger.info("Raw file found in db: "+rawFile.getIdentifier)
              rawFile.getRuns.get(0)
            }

            logger.info("Create Scan Sequence")

            val scanSequence = lcmsEM.find(classOf[ScanSequence], udsRun.getId)
            if (scanSequence == null) {

              val newScanSequence = new ScanSequence()
              newScanSequence.setId(udsRun.getId)
              newScanSequence.setRawFileName(rsName)

              // TODO : retrieve real values instead of fake constants
              newScanSequence.setMs1ScanCount(10000)
              newScanSequence.setMs2ScanCount(10000)

              logger.info("Persist Scan Sequence")
              lcmsEM.persist(newScanSequence)

              // create a pseudo scan for features
              val pseudoScan = new Scan()
              pseudoScan.setInitialId(0)
              pseudoScan.setCycle(0)
              pseudoScan.setMsLevel(1)
              pseudoScan.setPrecursorMoz(0.0)
              pseudoScan.setTime(0)
              pseudoScan.setBasePeakIntensity(0.0)
              pseudoScan.setTic(0.0)
              newScanSequence.addScan(pseudoScan)
              lcmsEM.persist(pseudoScan)
              pseudoScanByRsName += (rsName -> pseudoScan.getId)
            } else {
              val scans = scanSequence.getScans
              if ((scans == null) || scans.isEmpty) {
                val pseudoScan = new Scan()
                pseudoScan.setInitialId(0)
                pseudoScan.setCycle(0)
                pseudoScan.setMsLevel(1)
                pseudoScan.setPrecursorMoz(0.0)
                pseudoScan.setTime(0)
                pseudoScan.setBasePeakIntensity(0.0)
                pseudoScan.setTic(0.0)
                scanSequence.addScan(pseudoScan)
                lcmsEM.persist(pseudoScan)
                pseudoScanByRsName += (rsName -> pseudoScan.getId)
              } else {
                pseudoScanByRsName += (rsName -> scans.get(0).getId())
              }

            }

            lcmsEM.flush()
            logger.debug("registered Pseudo scans : "+pseudoScanByRsName)

            quantChannels += QuantChannel(
              number = count,
              name = rsName,
              sampleNumber = 1,
              identResultSummaryId = rsmId,
              runId = Some(udsRun.getId)
            )

            count += 1
          }

          //TODO build new Experimental Design from MaxQuant file
          val expDesign = ExperimentalDesign(
            biologicalSamples = Array(BiologicalSample(number = 1, name = "Sample 1")),
            groupSetups = Array(GroupSetup(number = 1, name = "Group Setup 1", biologicalGroups = Array(BiologicalGroup(number = 1, name = "Group 1", sampleNumbers = Array(1))))),
            masterQuantChannels = Array(MasterQuantChannel(number = 1, name = Some("Master Quant Channel"), quantChannels = quantChannels.toArray))
          )

          var quantiId: Long = 0L

          val name = "MaxQuant imported XIC"
          val methodId = 1
          val description = "Import MaxQuant"

          // Store quantitation in the UDSdb
          val quantiCreator = new CreateQuantitation(
            executionContext = execCtx,
            name = name,
            description = description,
            projectId = projectId,
            methodId = methodId,
            experimentalDesign = expDesign
          )
          quantiCreator.runService()

          quantiId = quantiCreator.getUdsQuantitation.getId
          result.put("quantitation_dataset_id",Predef.long2Long(quantiId))

          // Retrieve entity manager
          val udsDbCtx = providerContext.getUDSDbConnectionContext
          val udsEM = udsDbCtx.getEntityManager
          val udsQuantitation = udsEM.find(classOf[UdsDataset], quantiCreator.getUdsQuantitation.getId)

          // Retrieve master quant channels (they should be sorted by their number)
          val udsMasterQuantitationChannels = udsQuantitation.getMasterQuantitationChannels.toList

          for (udsMasterQuantChannel <- udsMasterQuantitationChannels) {

            new ThirdPartyLabelFreeFeatureQuantifier(
              executionContext = providerContext,
              experimentalDesign = expDesign,
              udsMasterQuantChannel = udsMasterQuantChannel,
              mapSetBuilder = this
            ).quantify()
          }

        }) // end of tryInTransactions

      }

    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    // TODO: DBO => please, avoid calling System.gc()
    System.gc()
    logger.debug("Import MaxQuant results done : " + result)

    result
  }


  def buildMapSet(lcmsDbCtx: LcMsDbConnectionContext, name: String, runIdByRsName: scala.collection.immutable.Map[String, Long]): MapSet = {
    require(rsMQImporterOpt.isDefined, "rsmImporter cannot be null before creating MaxQuant MapSet")

    //convert scala Map[.. , scala.Long] to Map<.., java.lang.java>
    val map = new java.util.HashMap[String, java.lang.Long]
    for ((rsName, runId) <- runIdByRsName) {
      map.put(rsName, runId)
    }

    val scanMap = new java.util.HashMap[String, java.lang.Long]
    for ((rsName, scanId) <- pseudoScanByRsName) {
      scanMap.put(rsName, scanId)
    }

    val mapSet = rsMQImporterOpt.get.buildMapSet(name, map, scanMap)

    val rawMapStorer = RawMapStorer(lcmsDbCtx)
    val childRawMaps = mapSet.childMaps.flatMap(m => m.getRawMaps().map(rm => rm.get))
    for (rawMap <- childRawMaps) {
      rawMapStorer.storeRawMap(rawMap, storeFeatures = true, storePeakels = false)
    }


    mapSet
  }

}

