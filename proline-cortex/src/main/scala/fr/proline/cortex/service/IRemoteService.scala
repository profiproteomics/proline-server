package fr.proline.cortex.service
import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.JSONRPC2Response
import com.thetransactioncompany.jsonrpc2.JSONRPC2Error

trait IRemoteService {

  /**
   * Fully qualified Proline service name like "proline/dps/msi/ImportResultFiles".
   */
  val serviceName: String

  /**
   * Version of this service implementation.
   */
  val serviceVersion: String

  /**
   * True if this service implementation version is the default one. False by default.
   * <p> For a given serviceName, it must exist only one default version.
   */
  val defaultVersion: Boolean = false

  def process(req: JSONRPC2Request): JSONRPC2Response = {
    require(req != null, "Req is null")

    val requestId = req.getID

    new JSONRPC2Response(JSONRPC2Error.METHOD_NOT_FOUND, requestId)
  }

}

trait ISingleThreadedService extends IRemoteService
