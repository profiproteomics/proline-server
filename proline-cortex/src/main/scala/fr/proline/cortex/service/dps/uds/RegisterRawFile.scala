package fr.proline.cortex.service.dps.uds

import javax.persistence.EntityManager
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.orm.uds.RawFile
import fr.proline.core.orm.uds.Run
import fr.proline.cortex.api.service.dps.uds.IRegisterRawFileService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.cortex.service.SingleThreadIdentifierType

/**
 *  Define JMS Service wich allows to register a raw file in UDS database
 *
 *  Input params:
 *    raw_file_identifier: The identifier of the raw file, defined as its name without the extension.
 *    raw_file_path: The raw file path relative to a managed mount point.
 *    mzdb_file_path: The mzDB file path relative to a managed mount point.
 *    instrument_id: database id of the instrument on which acquisition was done.
 *    owner_id: id of the project owner associated to the raw file.
 *    overwrite: If true, the service will overwrite existing raw file information.
 *
 *  Output params:
 *    Boolean for service run status
 */
class RegisterRawFile extends AbstractRemoteProcessingService with IRegisterRawFileService with ISingleThreadedService  with LazyLogging {
 
  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.SHORT_SERVICES_SINGLETHREAD_IDENT.toString
 
  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.RAW_FILE_IDENTIFIER_PARAM), "raw_file_identifier parameter not specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.INSTRUMENT_ID_PARAM), "instrument_id parameter not specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.OWNER_ID_PARAM), "owner_id parameter not specified")
    
    val overwriteRawFile = paramsRetriever.getOptBoolean(PROCESS_METHOD.OVERWRITE, false)
    
    val rawFileIdentifier = paramsRetriever.getString(PROCESS_METHOD.RAW_FILE_IDENTIFIER_PARAM)
    
    // Retrieve UdsDb context and entity manager
    val udsDbCtx = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory().getUdsDbConnector)
    
    try {
      val udsEM = udsDbCtx.getEntityManager
      
      // Search for this raw file identifier in the UDSdb
      val existingRawFile = udsEM.find(classOf[RawFile], rawFileIdentifier)
      
      // Create new raw file
      val udsRawFile = if (existingRawFile != null) {
        if (!overwriteRawFile) {
          logger.info(s"The raw file '$rawFileIdentifier' is already registered, but no update will be performed ('overwrite' option set to false) !")
          return existingRawFile.getRuns.get(0).getId
        } else {
          logger.warn(s"The raw file '$rawFileIdentifier' is already registered, it's properties (location, owner...) will be updated !")
          existingRawFile
        }
      }
      else {
        val newRawFile = new RawFile()
        newRawFile.setIdentifier(rawFileIdentifier)
        
        newRawFile
      }
      
      // BEGIN TRANSACTION
      udsDbCtx.beginTransaction()
      
      udsRawFile.setOwnerId(paramsRetriever.getLong(PROCESS_METHOD.OWNER_ID_PARAM))
      
      // Parse mzDB file path if provided
      val mzDbFilePathOpt = if (paramsRetriever.hasParam(PROCESS_METHOD.MZDB_FILE_PATH_PARAM)) {
        val mzDbFilePath = paramsRetriever.getString(PROCESS_METHOD.MZDB_FILE_PATH_PARAM)
        this._extractMzDbFileMetaData(udsRawFile, mzDbFilePath)
        Some(mzDbFilePath)
      } else None
  
      // Parse raw file path if provided
      if (paramsRetriever.hasParam(PROCESS_METHOD.RAW_FILE_PATH_PARAM)) {
        val rawFilePath = paramsRetriever.getString(PROCESS_METHOD.RAW_FILE_PATH_PARAM)
        val rawFile = new java.io.File(rawFilePath)
  
        udsRawFile.setRawFileDirectory(rawFile.getParent)
        udsRawFile.setRawFileName(rawFile.getName)
        
        // If the creation timestamp was not retrieved from the mzDB file
        if (udsRawFile.getCreationTimestamp == null) {
          // Try to retrieve this timestamp by accessing raw file meta-data
          val rawFileLocalPathname = MountPointRegistry.replacePossibleLabel(rawFilePath).localPathname
          val rawFileLocal = new java.io.File(rawFileLocalPathname)
          
          if (rawFileLocal.exists) udsRawFile.setCreationTimestamp(new java.sql.Timestamp(rawFileLocal.lastModified))
        }
      } else {
        // Provide a raw file name if no raw file path was provided
        if (mzDbFilePathOpt.isDefined) {
          val mzDBFilePath = mzDbFilePathOpt.get
          val rawFilePath = mzDBFilePath.substring(0, mzDBFilePath.lastIndexOf('.'))
          val rawFileFake = new java.io.File(rawFilePath)
          udsRawFile.setRawFileName(rawFileFake.getName)
        } else {
          udsRawFile.setRawFileName(rawFileIdentifier)
        }
      }
      
      // Retrieve the run id
      val udsRun = if (existingRawFile != null) {
        existingRawFile.getRuns.get(0)
      } else {
        udsEM.persist(udsRawFile)
        this._attachRunToRawFile(udsRawFile, udsEM)
      }
      
      // COMMIT TRANSACTION
      udsDbCtx.commitTransaction()
      
      udsRun.getId
    
    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbCtx)
    }
    
  }

  private def _attachRunToRawFile(udsRawFile: RawFile, udsEM: EntityManager): Run = {

    // Create new run and link it to the raw file
    val udsRun = new Run()
    udsRun.setNumber(1)
    udsRun.setRunStart(0f)
    udsRun.setRunStop(0f)
    udsRun.setDuration(0f)
    udsRun.setRawFile(udsRawFile)

    udsEM.persist(udsRun)
    
    udsRun
  }

  private def _extractMzDbFileMetaData(udsRawFile: RawFile, mzDbFilePath: String): Unit = {

    val mzDbFile = new java.io.File(mzDbFilePath)
    val mzDbFileDir = mzDbFile.getParent
    val mzDbFileName = mzDbFile.getName
    udsRawFile.setMzDbFileDirectory(mzDbFileDir)
    udsRawFile.setMzDbFileName(mzDbFileName)

    val mzDbFileLocalPathname = MountPointRegistry.replacePossibleLabel(mzDbFilePath).localPathname
    val mzDbFileLocal = new java.io.File(mzDbFileLocalPathname)

    if (mzDbFileLocal.exists) {

      val mzDb = new fr.profi.mzdb.MzDbReader(mzDbFileLocal, false)

      try {
        // Retrieve and set the sample name
        val sampleName = mzDb.getSamples.get(0).getName
        udsRawFile.setSampleName(sampleName)

        // Retrieve and set the raw file creation date from the mzDB file if not already set
        if (udsRawFile.getCreationTimestamp == null) {
          val creationDate = mzDb.getRuns.get(0).getStartTimestamp
          if (creationDate != null) {
            val creationDateAsEpochMilli = creationDate.getTime
            udsRawFile.setCreationTimestamp(new java.sql.Timestamp(creationDateAsEpochMilli))
          }

        }

      } finally {
        mzDb.close()
      }
    }

    ()
  }
}