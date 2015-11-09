package fr.proline.jms.util.jsonrpc

import com.thetransactioncompany.jsonrpc2.JSONRPC2Request
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever

object JSONRPC2Utils {

  /***
   * Return NamedParamsRetriever specified in JSONRPC2Request or null if none was specified
   */
  def buildParamsRetriever(req: JSONRPC2Request): NamedParamsRetriever = {
    require((req != null), "Req is null")

    val namedParams = req.getNamedParams
    if(namedParams !=null)
    	new NamedParamsRetriever(namedParams)
    else
      null
  }

}
