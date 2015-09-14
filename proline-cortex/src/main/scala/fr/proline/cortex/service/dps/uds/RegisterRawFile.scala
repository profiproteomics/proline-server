package fr.proline.cortex.service.dps.uds

import fr.proline.cortex.service.AbstractRemoteProcessService
import com.typesafe.scalalogging.slf4j.Logging
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.proline.core.orm.uds.RawFile
import fr.proline.cortex.util.MountPointRegistry
import fr.proline.context.DatabaseConnectionContext
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.core.orm.uds.Run

/**
 *  Define JMS Service wich allows to register a raw file in UDS database
 *
 *  Input params :
 *    raw_file_identifier : The identifier of the raw file, defined as its name without the extension.
 *    raw_file_path : The raw file path relative to a managed mount point.
 *    mzdb_file_path :  The mzDB file path relative to a managed mount point.
 *    instrument_id : database id of the instrument on which acquisition was done
 *    owner_id :  id of the project owner associated to the raw file
 *
 *  Output params :
 *    Boolean for service run status
 */
class RegisterRawFile extends AbstractRemoteProcessService with Logging {

  /* JMS Service identification */
  val serviceName = "proline/dps/uds/RegisterRawFile"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require((paramsRetriever != null), "no parameter specified")
    require(paramsRetriever.hasParam("raw_file_identifier"), "raw_file_identifier parameter not specified")
    require(paramsRetriever.hasParam("instrument_id"), "instrument_id parameter not specified")
    require(paramsRetriever.hasParam("owner_id"), "owner_id parameter not specified")

    // Create new raw file
    val udsRawFile = new RawFile()
    udsRawFile.setIdentifier(paramsRetriever.getString("raw_file_identifier"))
    udsRawFile.setInstrumentId(paramsRetriever.getLong("instrument_id"))
    udsRawFile.setOwnerId(paramsRetriever.getLong("owner_id"))

    // Parse raw file path if provided
    if (paramsRetriever.hasParam("raw_file_path")) {
      val rawFilePath = paramsRetriever.getString("raw_file_path")
      val rawFile = new java.io.File(rawFilePath)

      udsRawFile.setRawFileDirectory(rawFile.getParent())
      udsRawFile.setRawFileName(rawFile.getName())

      val rawFileLocalPathname = MountPointRegistry.replacePossibleLabel(rawFilePath).localPathname
      val rawFileLocal = new java.io.File(rawFileLocalPathname)

      // TODO: try to get this information from the mzDB file (implement a getRuns method in mzdb-access)
      if (rawFileLocal.exists) udsRawFile.setCreationTimestamp(new java.sql.Timestamp(rawFileLocal.lastModified))
    }

    // Parse mzDB file path if provided
    if (paramsRetriever.hasParam("mzdb_file_path")) {
      val mzDbFilePath = paramsRetriever.getString("mzdb_file_path")
      extractMzDbFileMetaData(udsRawFile, mzDbFilePath)
    }

    val runId = this.registerRawFile(udsRawFile)
    runId
  }

  protected def registerRawFile(udsRawFile: RawFile): java.lang.Long = {

    // 	Retrieve UdsDb context and entity manager
    val udsDbCtx: DatabaseConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getIDataStoreConnectorFactory.getUdsDbConnector())

    val runId = try {

      udsDbCtx.beginTransaction()

      val udsEM = udsDbCtx.getEntityManager()
      udsEM.persist(udsRawFile)

      // Create new run and link it to the raw file
      val udsRun = new Run()
      udsRun.setNumber(1)
      udsRun.setRunStart(0f)
      udsRun.setRunStop(0f)
      udsRun.setDuration(0f)
      udsRun.setRawFile(udsRawFile)

      udsEM.persist(udsRun)

      udsDbCtx.commitTransaction()

      udsRun.getId

    } finally {
      // Close UdsDb context
      try {
        udsDbCtx.close()
      } catch {
        case exClose: Exception => logger.error("Error closing UDS Db Context", exClose)
      }

    }

    runId
  }

  protected def extractMzDbFileMetaData(udsRawFile: RawFile, mzDbFilePath: String): Unit = {

    val mzDbFile = new java.io.File(mzDbFilePath)
    val mzDbFileDir = mzDbFile.getParent()
    val mzDbFileName = mzDbFile.getName()
    udsRawFile.setMzDbFileDirectory(mzDbFileDir)
    udsRawFile.setMzDbFileName(mzDbFileName)

    val mzDbFileLocalPathname = MountPointRegistry.replacePossibleLabel(mzDbFilePath).localPathname
    val mzDbFileLocal = new java.io.File(mzDbFileLocalPathname)

    if (mzDbFileLocal.exists) {

      val mzDb = new fr.profi.mzdb.MzDbReader(mzDbFileLocal, false)

      try {
        // Retrieve and set the sample name
        val sampleName = mzDb.getSamples().get(0).getName()
        udsRawFile.setSampleName(sampleName)

        // Retrieve and set the raw file creation date from the mzDB file if not already set
        if (udsRawFile.getCreationTimestamp() == null) {
          //val creationDateAsEpochMilli = mzDb.getRuns().get(0).getStartTimestamp().toEpochMilli()
          val creationDate = mzDb.getRuns().get(0).getStartTimestamp()
          if (creationDate != null) {
            val creationDateAsEpochMilli = creationDate.getTime()
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