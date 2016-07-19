package fr.proline.cortex.util.jsonrpc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever

object JSONRPC2Utils {

  def buildParamsRetriever(req: JSONRPC2Request): NamedParamsRetriever = {
    require((req != null), "Req is null")

    val namedParams = req.getNamedParams

    new NamedParamsRetriever(namedParams)
  }

}
