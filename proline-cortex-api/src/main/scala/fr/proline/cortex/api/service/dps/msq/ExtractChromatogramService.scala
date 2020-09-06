package fr.proline.cortex.api.service.dps.msq

import scala.reflect.runtime.universe.typeOf

import fr.profi.util.jsonrpc.{IJSONRPC2Method, JSONRPC2DefaultMethod, JSONRPC2DefaultMethodParameter, JSONRPC2MethodResult}
import fr.proline.jms.service.api.{IDefaultServiceVersion, RemoteServiceIdentity}

object ExtractChromatogramService extends IExtractChromatogramService

trait IAbstractExtractChromatogramService extends IMsqService {
  /* JMS Service identification */
  val serviceLabel = "ExtractChromatogram"
  this.serviceDescription = Some(
    "Extract Chromatogram for m/z"
  )

}

trait IExtractChromatogramService extends IAbstractExtractChromatogramService with IDefaultServiceVersion {

  /* JMS Service identification */

  // List the handled methods
  val methodDefinitions: Seq[IJSONRPC2Method] = List(PROCESS_METHOD)

  object PROCESS_METHOD extends JSONRPC2DefaultMethod {

    // Method description
    val name = RemoteServiceIdentity.PROCESS_METHOD_NAME
    val description = serviceDescription.get

    // Configure method interface
    val parameters = List(
      RAW_FILE_IDENTIFIER_PARAM,
      MZ_PARAM,
      PPM_PARAM
    )
    val returns = JSONRPC2MethodResult(
      typeOf[String],
      "XIC Chromatogram as json String"
    )

    object RAW_FILE_IDENTIFIER_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "raw_file_identifier"
      val description = "List of raw file identifiers corresponding to the XIC to retrieve."
      val scalaType = typeOf[Array[String]]
    }

    object MZ_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "mz"
      val description = "List of m/z of the XICs to retrieve."
      val scalaType = typeOf[Array[Double]]
    }

    object PPM_PARAM extends JSONRPC2DefaultMethodParameter {
      val name = "ppm"
      val description = "The ppm of for the XIC m/Z parameter."
      val scalaType = typeOf[Double]
    }

  }
}


