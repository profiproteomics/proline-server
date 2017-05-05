package fr.proline.jms.util

import java.util.UUID

import scala.math.min

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import JMSConstants._

object NodeConfig extends LazyLogging {

  /* Singleton "Constructor" and fields declaration
   * Set a ThreadLogger to catch initialization errors */
  private val m_currentThread = Thread.currentThread

  if (!m_currentThread.getUncaughtExceptionHandler.isInstanceOf[ThreadLogger]) {
    m_currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))
  }

  // -- Property File loader  (jms-config.conf)
  var classLoader = NodeConfig.getClass().getClassLoader()
  private var _jmsConfParams: Config = null

  def getJMSConfigParams(): Config = {

    // Parse config if it not already done
    if (_jmsConfParams == null) {
      synchronized {
        _jmsConfParams = ConfigFactory.load("jms-node")
      }
    }

    _jmsConfParams
  }

  // TODO persist this as a PID
  val NODE_ID = UUID.randomUUID().toString

  /* JMS Node Config KEY loaded from "jms-node.conf" file */
  private val NODE_CONFIG_KEY = "node_config"
  private val JMS_SERVER_HOST_KEY = "jms_server_host"
  private val JMS_SERVER_PORT_KEY = "jms_server_port"
  private val PROLINE_SERVICE_REQUEST_QUEUE_KEY = "proline_service_request_queue_name"
  private val SERVICE_THREAD_POOL_SIZE_KEY = "service_thread_pool_size"
  private val MZDB_MAX_PARALLELISM_KEY = "mzdb_max_parallelism" // TODO: remove me (see #15945,#15948)
  private val PEAKELDB_TEMP_DIRECTORY_KEY = "peakeldb_temp_directory" // TODO: remove me (see #15945,#15948)
  private val ENABLE_IMPORTS_KEY = "enable_imports"

  private val DEFAULT_MZDB_MAX_PARALLELISM = 2 // TODO: remove me (see #15945,#15948)
  private val DEFAULT_PEAKELDB_TEMP_DIRECTORY = System.getProperty("java.io.tmpdir") // TODO: remove me (see #15945,#15948)
  private val DEFAULT_PROLINE_SERVICE_REQUEST_QUEUE_NAME = "ProlineServiceRequestQueue"
  private val ABSOLUTE_MAX_N_THREADS = 1000
  private val AUTO_MAX_N_THREAD = 20 // Same as AbstractDatabaseConnector.DEFAULT_MAX_POOL_CONNECTIONS

  private val m_jmsConfig = getJMSConfigParams().getConfig(NODE_CONFIG_KEY)

  private val m_jmsServerHost = m_jmsConfig.getString(JMS_SERVER_HOST_KEY)
  def getJMSServerHost = m_jmsServerHost
  require(!StringUtils.isEmpty(getJMSServerHost), "Invalid \"" + JMS_SERVER_HOST_KEY + "\" value")

  private val m_jmsServerPort = m_jmsConfig.getInt(JMS_SERVER_PORT_KEY)
  def getJMSServerPort = m_jmsServerPort
  require(((0 < getJMSServerPort) && (getJMSServerPort <= MAX_JMS_SERVER_PORT)), "Invalid \"" + JMS_SERVER_PORT_KEY + "\" value")

  /* JMS Queue name */
  val PROLINE_EXPIRED_MESSAGE_QUEUE_NAME = "ExpiryQueue"
  val PROLINE_SERVICE_REQUEST_QUEUE_NAME = retrieveQueueName(m_jmsConfig)

  val SERVICE_THREAD_POOL_SIZE = retrieveThreadPoolSize(m_jmsConfig)

  val ENABLE_IMPORTS = retrieveEnableImports(m_jmsConfig)

  val MZDB_MAX_PARALLELISM = retrieveMzdbMaxParallelism(m_jmsConfig)
  val PEAKELDB_TEMP_DIRECTORY = retrievePeakeldbTempDirectory(m_jmsConfig)

  private def retrieveQueueName(config: Config): String = {
    var queueName: String = null

    if (config.hasPath(PROLINE_SERVICE_REQUEST_QUEUE_KEY)) {
      queueName = config.getString(PROLINE_SERVICE_REQUEST_QUEUE_KEY)
    }

    if (StringUtils.isEmpty(queueName)) {
      logger.info("Invalid \"" + PROLINE_SERVICE_REQUEST_QUEUE_KEY + "\" using DEFAULT : " + DEFAULT_PROLINE_SERVICE_REQUEST_QUEUE_NAME)
      queueName = DEFAULT_PROLINE_SERVICE_REQUEST_QUEUE_NAME
    }

    queueName
  }

  private def retrieveThreadPoolSize(config: Config): Int = {
    var nThreads: Int = -1

    if (config.hasPath(SERVICE_THREAD_POOL_SIZE_KEY)) {
      nThreads = config.getInt(SERVICE_THREAD_POOL_SIZE_KEY)
    }

    if (nThreads < 1) { // At least 1 thread
      nThreads = min(Runtime.getRuntime.availableProcessors / 2 + 1, AUTO_MAX_N_THREAD)
      logger.info("Invalid \"" + SERVICE_THREAD_POOL_SIZE_KEY + "\" using AUTO : " + nThreads)
    } else if (nThreads > ABSOLUTE_MAX_N_THREADS) {
      logger.info("Invalid \"" + SERVICE_THREAD_POOL_SIZE_KEY + "\" using ABSOLUTE MAX : " + nThreads)
      nThreads = ABSOLUTE_MAX_N_THREADS
    }

    nThreads
  }

  private def retrieveMzdbMaxParallelism(config: Config): Int = {
    val maxParallelism = if (!config.hasPath(MZDB_MAX_PARALLELISM_KEY)) DEFAULT_MZDB_MAX_PARALLELISM
    else config.getInt(MZDB_MAX_PARALLELISM_KEY)
    
    logger.info(s"$MZDB_MAX_PARALLELISM_KEY=$maxParallelism")
    
    maxParallelism
  }
  
  private def retrievePeakeldbTempDirectory(config: Config): String = {
    val peakeldbTempDirectory = if (!config.hasPath(PEAKELDB_TEMP_DIRECTORY_KEY)) DEFAULT_PEAKELDB_TEMP_DIRECTORY
    else config.getString(PEAKELDB_TEMP_DIRECTORY_KEY)
    
    logger.info(s"$PEAKELDB_TEMP_DIRECTORY_KEY=$peakeldbTempDirectory")
    
    peakeldbTempDirectory
  }

  private def retrieveEnableImports(config: Config): Boolean = {
    var enableImports = false // Default do NOT handle imports

    if (config.hasPath(ENABLE_IMPORTS_KEY)) {
      enableImports = config.getBoolean(ENABLE_IMPORTS_KEY)
    }

    enableImports
  }

}
