package fr.proline.cortex.service.admin

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.HashMap
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.proline.admin.service.db.SetupProline
import fr.proline.cortex.api.service.admin.IGetConnectionTemplateService
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.jms.service.api.ISingleThreadedService

/**
 * JMS Service to create a get the connection template to access Proline DataStore (UDS db access and JMS Server information) .
 * No password will be returned. 
 * 
 * Input params :
 *  NONE 
 *  
 * Output params : 
 *   HashMap containing UDSdb connection properties // and JMS server information 
 *  
 */
class GetConnectionTemplate extends AbstractRemoteProcessingService with IGetConnectionTemplateService with ISingleThreadedService  with LazyLogging {
  
  /* JMS Service identification */
  val singleThreadIdent= SingleThreadIdentifierType.SHORT_SERVICES_SINGLETHREAD_IDENT.toString()
 

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    val prolineConf = SetupProline.config

    val connection = new HashMap[Object, Object]()
    connection ++= (prolineConf.udsDBConfig.dbConnProperties - "javax.persistence.jdbc.password")

//    if (SetupProline.getConfigParams.hasPath("jms-config.host")) {
//      val jmsHost = SetupProline.getConfigParams.getConfig("jms-config").getString("host")
//      if (!jmsHost.trim().isEmpty())
//        connection += "jms.server.host" -> SetupProline.getConfigParams.getConfig("jms-config").getString("host")
//    }
//
//    if (SetupProline.getConfigParams.hasPath("jms-config.port")) {
//      try {
//        connection += "jms.server.port" -> new Integer(SetupProline.getConfigParams.getConfig("jms-config").getInt("port"))
//      } catch {
//        case e: Exception => {
//          logger.trace("Invalid specifed port. ")
//          //Ignore port
//        }
//      }
//
//    }

    connection
  }

}