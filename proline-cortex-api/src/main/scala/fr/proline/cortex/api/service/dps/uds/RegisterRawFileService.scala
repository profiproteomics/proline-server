package fr.proline.cortex.api.service.dps.uds

import scala.reflect.runtime.universe.typeOf
import fr.proline.cortex.api.IDefaultServiceVersion
import fr.proline.cortex.api.IRemoteProcessingService
import fr.proline.cortex.util.jsonrpc.JSONRPC2DefaultMethodParameter
import fr.proline.cortex.util.jsonrpc.JSONRPC2MethodResult

object RegisterRawFileService extends IRegisterRawFileService

trait IRegisterRawFileService extends IUdsService with IRemoteProcessingService with IDefaultServiceVersion {
  
  /* JMS Service identification */
  val serviceLabel = "RegisterRawFile"
  this.serviceDescription = Some("Registers paths of raw and mzDB files in the UDS database.")
  
  /* Configure the service interface */
  val serviceParams = List(
    RAW_FILE_IDENTIFIER_PARAM,
    RAW_FILE_PATH_PARAM,
    MZDB_FILE_PATH_PARAM,
    INSTRUMENT_ID_PARAM,
    OWNER_ID_PARAM
  )
  val serviceResult = JSONRPC2MethodResult(
    typeOf[Long],
    "The ID of the run corresponding the the newly registered raw/mzDB pair."
  )
  
  object RAW_FILE_IDENTIFIER_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "raw_file_identifier"
    val description = "The identifier of the raw file, defined as its name without the extension."
    val scalaType = typeOf[String]
  }
  object RAW_FILE_PATH_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "raw_file_path"
    val description = "The raw file path relative to a managed mount point."
    val scalaType = typeOf[String]
    optional = true
  }
  object MZDB_FILE_PATH_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "mzdb_file_path"
    val description = "The mzDB file path relative to a managed mount point."
    val scalaType = typeOf[Map[String,Object]]
    optional = true
  }
  object INSTRUMENT_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "instrument_id"
    val description = null
    val scalaType = typeOf[Long]
  }
  object OWNER_ID_PARAM extends JSONRPC2DefaultMethodParameter {
    val name = "owner_id"
    val description = null
    val scalaType = typeOf[Long]
  }
}

