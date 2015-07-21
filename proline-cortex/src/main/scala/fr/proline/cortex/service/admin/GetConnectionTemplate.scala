package fr.proline.cortex.service.admin

import fr.proline.cortex.service.AbstractRemoteProcessService
import com.typesafe.scalalogging.slf4j.Logging
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import scala.collection.mutable.HashMap
import fr.proline.admin.service.db.SetupProline
import collection.JavaConversions._


/**
 * JMS Service to create a get the connection template to access Proline DataStore (UDS db access and JMS Server information) .
 * No password will be returned. 
 * 
 * Input params :
 *  NONE 
 *  
 * Output params : 
 *   HashMap containing UDSdb connection properties and JMS server information 
 *  
 */
class GetConnectionTemplate extends AbstractRemoteProcessService with Logging {

  /* JMS Service identification */
  override val serviceName = "proline/admin/GetConnectionTemplate"
  override val defaultVersion = true
  override val serviceVersion = "1.0"

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    val prolineConf = SetupProline.config

    val connection = new HashMap[Object, Object]()
    connection ++= (prolineConf.udsDBConfig.dbConnProperties - "javax.persistence.jdbc.password")

    if (SetupProline.getConfigParams.hasPath("jms-config.host")) {
      val jmsHost = SetupProline.getConfigParams.getConfig("jms-config").getString("host")
      if (!jmsHost.trim().isEmpty())
        connection += "jms.server.host" -> SetupProline.getConfigParams.getConfig("jms-config").getString("host")
    }

    if (SetupProline.getConfigParams.hasPath("jms-config.port")) {
      try {
        connection += "jms.server.port" -> new Integer(SetupProline.getConfigParams.getConfig("jms-config").getInt("port"))
      } catch {
        case e: Exception => {
          logger.trace("Invalid specifed port. ")
          //Ignore port
        }
      }

    }

    connection
  }

}