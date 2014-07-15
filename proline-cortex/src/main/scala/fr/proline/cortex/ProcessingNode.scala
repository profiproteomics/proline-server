package fr.proline.cortex

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.collection.JavaConversions.mutableMapAsJavaMap
import scala.collection.mutable
import org.hornetq.api.core.TransportConfiguration
import org.hornetq.api.jms.HornetQJMSClient
import org.hornetq.api.jms.JMSFactoryType
import org.hornetq.core.remoting.impl.netty.NettyConnectorFactory
import org.hornetq.core.remoting.impl.netty.TransportConstants
import com.typesafe.scalalogging.slf4j.Logging
import Constants.MAX_PORT
import NodeConfig._
import NodeConfig.JMS_SERVER_PORT
import NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME
import NodeConfig.SERVICE_THREAD_POOL_SIZE
import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.cortex.service.monitoring.InfoService
import fr.proline.cortex.service.monitoring.SingleThreadedInfoService
import javax.jms.Connection
import javax.jms.ConnectionFactory
import fr.proline.core.orm.util.DataStoreConnectorFactory
import fr.proline.admin.service.db.SetupProline

object ProcessingNode extends Logging {

  def main(args: Array[String]) {
    Thread.currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))

    val server = new ProcessingNode(JMS_SERVER_HOST, JMS_SERVER_PORT)
    addShutdownHook(server)

    /* Start JMS Consumers */
    server.startJMSConsumers
  }

  private def addShutdownHook(server: ProcessingNode) {
    assert(server != null, "addShutdownHook() server is null")

    val target = new Runnable {

      override def run() {
        logger.debug("Stopping JMS Consumers")
        server.stopJMSConsumers()
      }

    }

    val thr = new Thread(target, "Thread-shutdownJMSConsumers")
    thr.setPriority(Thread.NORM_PRIORITY)
    thr.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))

    Runtime.getRuntime.addShutdownHook(thr)
  }

}

class ProcessingNode(jmsServerHost: String, jmsServerPort: Int) extends Logging {

  import ProcessingNode._

  /* Constructor checks */
  require(!StringUtils.isEmpty(jmsServerHost), "Invalid JMS Server Host name or address")
  require((0 < jmsServerPort) && (jmsServerPort <= MAX_PORT), "Invalid JMS Server port")

  private val m_lock = new Object()

  private var m_connection: Connection = null

  private var m_executor: ExecutorService = null

  /**
   * Starts JMS Connection and Executor running Consumers receive loop.
   */
  def startJMSConsumers() {

    m_lock.synchronized {

      if (m_connection != null) {
        throw new IllegalArgumentException("JMS Consumers already started !")
      }

      try {
        // Step 1. Directly instantiate the JMS Queue object.
        val queue = HornetQJMSClient.createQueue(PROLINE_SERVICE_REQUEST_QUEUE_NAME)

        logger.debug("JMS Queue : " + queue)

        // Step 2. Instantiate the TransportConfiguration object which contains the knowledge of what transport to use,
        // The server port etc.

        val connectionParams = mutable.Map.empty[String, Object]
        connectionParams.put(TransportConstants.HOST_PROP_NAME, jmsServerHost) // JMS Server hostname or IP
        connectionParams.put(TransportConstants.PORT_PROP_NAME, java.lang.Integer.valueOf(jmsServerPort)) // JMS port

        val transportConfiguration = new TransportConfiguration(classOf[NettyConnectorFactory].getName,
          connectionParams)

        // Step 3 Directly instantiate the JMS ConnectionFactory object using that TransportConfiguration
        val cf = HornetQJMSClient.createConnectionFactoryWithoutHA(JMSFactoryType.CF, transportConfiguration).asInstanceOf[ConnectionFactory]

        // Step 4.Create a JMS Connection
        m_connection = cf.createConnection()

        logger.info("This Node Id : " + NodeConfig.NODE_ID)

        initDataStore()

        initServices()

        /* Create Executor */
        m_executor = Executors.newCachedThreadPool()

        /* Add SingleThreadedServiceRunner */
        val handledSingleThreadedServiceNames = ServiceRegistry.getSingleThreadedServices.keySet

        for (serviceName <- handledSingleThreadedServiceNames) {
          val singleThreadedServiceRunner = new SingleThreadedServiceRunner(queue, m_connection, serviceName)
          m_executor.submit(singleThreadedServiceRunner)
        }

        /* Add Parallelizable SeviceRunner */
        logger.debug("Starting " + SERVICE_THREAD_POOL_SIZE + " Parallelizable ServiceRunners")

        for (i <- 1 to SERVICE_THREAD_POOL_SIZE) {
          val parallelizableSeviceRunner = new ServiceRunner(queue, m_connection)
          m_executor.submit(parallelizableSeviceRunner)
        }

        m_connection.start() // Explicitely start connection to begin Consumer reception
        logger.debug("JMS Connection : " + m_connection + "  started")
      } catch {

        case ex: Exception => {
          logger.error("Error starting JMS Consumers", ex)

          stopJMSConsumers()
        }

      }

    } // End of synchronized block on m_lock

  }

  private def initDataStore() {
    val dsConnectorFactory = DataStoreConnectorFactory.getInstance

    if (!dsConnectorFactory.isInitialized) {
      val prolineConfig = SetupProline.config

      val udsDbProperties = prolineConfig.udsDBConfig.dbConnProperties

      logger.debug("Initializing DataStoreConnectorFactory from UDS Db Properties")

      dsConnectorFactory.initialize(udsDbProperties)
    }

  }

  private def initServices() {

    /* Single-threaded services */
    ServiceRegistry.addService(new SingleThreadedInfoService()) // Test

    if (ENABLE_IMPORTS) {
      logger.info("This node HANDLE Result Files Import")
    } else {
      logger.info("This node do NOT handle Result Files Import")
    }

    /* Parallelizable Sevice */
    ServiceRegistry.addService(new InfoService()) // Test

  }

  /**
   * Gracefully closes JMS Connection and stops the Executor running Consumers receive loop.
   */
  def stopJMSConsumers() {

    m_lock.synchronized {

      if (m_connection != null) {
        logger.debug("Closing JMS Connection")

        try {
          m_connection.close()
          logger.info("JMS Connection closed")
        } catch {
          case exClose: Exception => logger.error("Error closing JMS Connection", exClose)
        }

      }

      if (m_executor != null) {
        logger.debug("Stopping JMS Consumers Executor")
        m_executor.shutdown()

        logger.debug("Waiting Executor termination")

        try {
          if (m_executor.awaitTermination(Long.MaxValue, TimeUnit.SECONDS)) {
            logger.info("JMS Consumers Executor terminated")
          }
        } catch {
          case intEx: InterruptedException => logger.warn("ExecutorService.awaitTermination() interrupted", intEx)
        }

      }

      logger.info("JMS Consumers stopped")
    } // End of synchronized block on m_lock

  }

}
