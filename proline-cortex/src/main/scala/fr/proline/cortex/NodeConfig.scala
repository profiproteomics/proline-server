package fr.proline.cortex

import java.util.UUID

import scala.math.min

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.admin.service.db.SetupProline
import fr.proline.cortex.Constants.MAX_PORT

object NodeConfig extends LazyLogging {

  /* Singleton "Constructor" and fields declaration
   * Set a ThreadLogger to catch initialization errors */
  private val m_currentThread = Thread.currentThread

  if (!m_currentThread.getUncaughtExceptionHandler.isInstanceOf[ThreadLogger]) {
    m_currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))
  }

  // TODO persist this as a PID
  val NODE_ID = UUID.randomUUID().toString

  /* JMS Node Config loaded from "application.conf" file */
  private val NODE_CONFIG_KEY = "node_config"

  private val JMS_SERVER_HOST_KEY = "jms_server_host"

  private val JMS_SERVER_PORT_KEY = "jms_server_port"

  private val PROLINE_SERVICE_REQUEST_QUEUE_KEY = "proline_service_request_queue_name"

  private val DEFAULT_PROLINE_SERVICE_REQUEST_QUEUE_NAME = "ProlineServiceRequestQueue"

  private val SERVICE_THREAD_POOL_SIZE_KEY = "service_thread_pool_size"

  private val ABSOLUTE_MAX_N_THREADS = 1000

  private val AUTO_MAX_N_THREAD = 20 // Same as AbstractDatabaseConnector.DEFAULT_MAX_POOL_CONNECTIONS

  private val ENABLE_IMPORTS_KEY = "enable_imports"

  private val m_prolineConfig = SetupProline.getConfigParams.getConfig(NODE_CONFIG_KEY)

  val JMS_SERVER_HOST = m_prolineConfig.getString(JMS_SERVER_HOST_KEY)
  require(!StringUtils.isEmpty(JMS_SERVER_HOST), "Invalid \"" + JMS_SERVER_HOST_KEY + "\" value")

  val JMS_SERVER_PORT = m_prolineConfig.getInt(JMS_SERVER_PORT_KEY)
  require(((0 < JMS_SERVER_PORT) && (JMS_SERVER_PORT <= MAX_PORT)), "Invalid \"" + JMS_SERVER_PORT_KEY + "\" value")

  /* JMS Queue name */
  val PROLINE_SERVICE_REQUEST_QUEUE_NAME = retrieveQueueName(m_prolineConfig)

  val SERVICE_THREAD_POOL_SIZE = retrieveThreadPoolSize(m_prolineConfig)

  val ENABLE_IMPORTS = retrieveEnableImports(m_prolineConfig)

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

  private def retrieveEnableImports(config: Config): Boolean = {
    var enableImports = false // Default do NOT handle imports

    if (config.hasPath(ENABLE_IMPORTS_KEY)) {
      enableImports = config.getBoolean(ENABLE_IMPORTS_KEY)
    }

    enableImports
  }

}
