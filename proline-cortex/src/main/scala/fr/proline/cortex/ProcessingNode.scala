package fr.proline.cortex

import java.io.File
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import javax.jms.Connection
import javax.jms.ExceptionListener
import javax.jms.JMSException
import scala.collection.JavaConversions.mutableMapAsJavaMap
import scala.collection.mutable
import org.hornetq.api.core.TransportConfiguration
import org.hornetq.api.jms.HornetQJMSClient
import org.hornetq.api.jms.JMSFactoryType
import org.hornetq.core.remoting.impl.netty.NettyConnectorFactory
import org.hornetq.core.remoting.impl.netty.TransportConstants
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.cortex.service.admin.CreateProject
import fr.proline.cortex.service.admin.GetConnectionTemplate
import fr.proline.cortex.service.admin.UserAccount
import fr.proline.cortex.service.dps.msi._
import fr.proline.cortex.service.dps.msq._
import fr.proline.cortex.service.dps.uds.GetExportInformation
import fr.proline.cortex.service.dps.uds.RegisterRawFile
import fr.proline.cortex.service.misc.FileSystem
import fr.proline.cortex.service.misc.FileUpload
import fr.proline.cortex.service.monitoring.InfoService
import fr.proline.cortex.service.monitoring.SingleThreadedInfoService
import fr.proline.cortex.util._
import fr.proline.cortex.util.fs._
import fr.proline.jms._
import fr.proline.jms.util.JMSConstants._
import fr.proline.jms.util._
import scala.collection.mutable.HashMap
import java.util.concurrent.Future
import fr.proline.jms.ServiceManager
import fr.proline.cortex.service.dps.uds.ValidateIdentDSInTree


object ProcessingNode extends LazyLogging {

  /* Constants */
  private val EXECUTOR_SHUTDOWN_TIMEOUT = 30 // 30 seconds : te be sure currently running services could have a chance to terminate 

  def main(args: Array[String]) {
    Thread.currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))

    val server = new ProcessingNode(NodeConfig.getJMSServerHost, NodeConfig.getJMSServerPort)
    addShutdownHook(server)

    /* Start JMS Consumers */
    server.startJMSConsumers
  }

  private def addShutdownHook(server: ProcessingNode) {
    assert((server != null), "addShutdownHook() server is null")

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

class ProcessingNode(jmsServerHost: String, jmsServerPort: Int) extends LazyLogging {

  import ProcessingNode._

  /* Constructor checks */
  require(!StringUtils.isEmpty(jmsServerHost), "Invalid JMS Server Host name or address")

  require(0 < jmsServerPort && jmsServerPort <= MAX_JMS_SERVER_PORT, "Invalid JMS Server port")

  private val m_lock = new Object()

  /* All mutable fields are @GuardedBy("m_lock") */
  private var m_connection: Connection = null

  private var m_executor: ExecutorService = null

  /**
   * Starts JMS Connection and Executor running Consumers receive loop.
   */
  def startJMSConsumers() {

    m_lock.synchronized {

      if (m_connection != null) {
        throw new IllegalStateException("JMS Consumers already started !")
      }

      try {
        // Step 1. Directly instantiate the JMS Queue object.
        val serviceRequestQueue = HornetQJMSClient.createQueue(NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME)

        logger.debug("JMS Queue : " + serviceRequestQueue)
        val expiredRequestQueue = HornetQJMSClient.createQueue(NodeConfig.PROLINE_EXPIRED_MESSAGE_QUEUE_NAME)
        

        // Step 2. Instantiate the TransportConfiguration object which contains the knowledge of what transport to use,
        // The server port etc.

        val connectionParams = mutable.Map.empty[String, Object]
        connectionParams.put(TransportConstants.HOST_PROP_NAME, jmsServerHost) // JMS Server hostname or IP
        connectionParams.put(TransportConstants.PORT_PROP_NAME, java.lang.Integer.valueOf(jmsServerPort)) // JMS port

        val transportConfiguration = new TransportConfiguration(
          classOf[NettyConnectorFactory].getName,
          connectionParams
        )

        // Step 3 Directly instantiate the JMS ConnectionFactory object using that TransportConfiguration
        val cf = HornetQJMSClient.createConnectionFactoryWithoutHA(JMSFactoryType.CF, transportConfiguration) //.asInstanceOf[ConnectionFactory]
       cf.setConsumerWindowSize(0)
//       cf.setClientFailureCheckPeriod(5000);
       cf.setReconnectAttempts(10);
        // Step 4.Create a JMS Connection
        m_connection = cf.createConnection()

        // Add an ExceptionListener to handle asynchronous Connection problems
        val exceptionListener = new ExceptionListener() {
          override def onException(exception: JMSException) = {
            logger.error("Asynchronous JMS Connection problem", exception)
          }
        }

        m_connection.setExceptionListener(exceptionListener)

        logger.info("This Node Id : " + NodeConfig.NODE_ID)

        initFileSystem()

        DbConnectionHelper.initDataStore()

        initServices()

        /* Create Executor */
        m_executor = Executors.newCachedThreadPool()

        val serviceMonitoringNotifier = new MonitoringTopicPublisherRunner(m_connection)
        m_executor.submit(serviceMonitoringNotifier)

        /* Add SingleThreadedServiceRunner */
        val handledSingleThreadedServiceIdents = ServiceRegistry.getSingleThreadedServicesByThreadIdent().keySet

        var nbrSingleThreads = 0 
        for (threadIdent <- handledSingleThreadedServiceIdents) {
          val singleThreadedServiceRunner = new SingleThreadedServiceRunner(serviceRequestQueue, m_connection, serviceMonitoringNotifier, threadIdent, true)
          val singleThreadFuture =  m_executor.submit(singleThreadedServiceRunner)
          ServiceManager.addRunnale2FutureEntry(singleThreadedServiceRunner, singleThreadFuture)
          nbrSingleThreads += 1
        }

        // Start Expired Message Listener
        val expiredMessageConsumer = new ExpiredMessageConsumer(expiredRequestQueue, m_connection, serviceMonitoringNotifier)
        m_executor.submit(expiredMessageConsumer)
        nbrSingleThreads += 1
        
        logger.debug(nbrSingleThreads +" Single Thread ServiceRunners started")
       
        /* Add Parallelizable SeviceRunner */
        logger.debug("Starting " + NodeConfig.SERVICE_THREAD_POOL_SIZE + " Parallelizable ServiceRunners")

        for (i <- 1 to NodeConfig.SERVICE_THREAD_POOL_SIZE) {
          val parallelizableSeviceRunner = new ServiceRunner(serviceRequestQueue, m_connection, serviceMonitoringNotifier)
          val threadFuture = m_executor.submit(parallelizableSeviceRunner)
          ServiceManager.addRunnale2FutureEntry(parallelizableSeviceRunner, threadFuture)
        }

        m_connection.start() // Explicitly start connection to begin Consumer reception
        logger.debug("JMS Connection : " + m_connection + "  started")
      } catch {

        case ex: Exception => {
          logger.error("Error starting JMS Consumers", ex)

          stopJMSConsumers()
        }

      }

    } // End of synchronized block on m_lock

  }

  private def initFileSystem() {

    /* Register input directories from MountPointRegistry */
    val mountPoints = MountPointRegistry.retrieveAllMountPoints(true)

    for (mp <- mountPoints) {
      val dir = new File(mp.path)

      try {
        WorkDirectoryRegistry.registerWorkDirectory(dir)
      } catch {
        case ex: Exception => logger.error("Cannot register input directory \"" + mp.path + '\"', ex)
      }

    }

  }


  private def initServices() {

    /* Single-threaded services */
    // TODO Remove after test
    ServiceRegistry.addService(new SingleThreadedInfoService())

    if (NodeConfig.ENABLE_IMPORTS) {
      ServiceRegistry.addService(new ImportResultFilesDecoyRegExp())
      ServiceRegistry.addService(new ImportResultFilesProtMatchDecoyRule())
      ServiceRegistry.addService(new ImportValidateGenerateSM())
      logger.info("This node HANDLE Result Files Import")
    } else {
      logger.info("This node do NOT handle Result Files Import")
    }

    /* Parallelizable Service */
    ServiceRegistry.addService(new InfoService()) // Monitoring
    ServiceRegistry.addService(new FileSystem())
    ServiceRegistry.addService(new FileUpload())
    ServiceRegistry.addService(new ValidateResultSet())
    ServiceRegistry.addService(new ValidateIdentDSInTree())
    ServiceRegistry.addService(new UpdateSpectraParamsV1_0())
    ServiceRegistry.addService(new UpdateSpectraParamsV2_0())
    ServiceRegistry.addService(new MergeDatasetsV1_0())
    ServiceRegistry.addService(new MergeDatasetsV2_0())
    ServiceRegistry.addService(new ChangeTypicalProteinMatch())
    ServiceRegistry.addService(new CertifyResultFiles())
    ServiceRegistry.addService(new ExportResultSummary())
    ServiceRegistry.addService(new ExportResultSummaryV2_0())
    ServiceRegistry.addService(new GetExportInformation())
    ServiceRegistry.addService(new GenerateSpectrumMatches())
    ServiceRegistry.addService(new GenerateMSDiagReport())
    ServiceRegistry.addService(new FilterRSMProteinSets())
    ServiceRegistry.addService(new ComputeQuantProfiles())
    ServiceRegistry.addService(new Quantify())
    ServiceRegistry.addService(new QuantifySC())
    ServiceRegistry.addService(new UserAccount())
    ServiceRegistry.addService(new GetConnectionTemplate())
    ServiceRegistry.addService(new CreateProject())
    ServiceRegistry.addService(new RegisterRawFile())
    ServiceRegistry.addService(new DeleteOrphanData())
    ServiceRegistry.addService(new QuantifyV2_0())
    ServiceRegistry.addService(new ImportMaxQuantResults())
    //VDS TEST only ! 
//    ServiceRegistry.addService(new WaitService())
//    ServiceRegistry.addService(new CancelService())
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
          case exClose: JMSException => logger.error("Error closing JMS Connection", exClose)
        }

      }

      if (m_executor != null) {
        logger.debug("Stopping JMS Consumers Executor")
        m_executor.shutdown()

        logger.debug("Waiting " + EXECUTOR_SHUTDOWN_TIMEOUT + " seconds for Executor termination...")

        try {

          if (m_executor.awaitTermination(EXECUTOR_SHUTDOWN_TIMEOUT, TimeUnit.SECONDS)) {
            logger.info("JMS Consumers Executor terminated")
          } else {
            val remainingRunnables = m_executor.shutdownNow()
            logger.info(s"JMS Consumers Executor terminated, ${remainingRunnables.size} remaining Runnable(s) that never started.")
          }

        } catch {
          case intEx: InterruptedException => logger.warn("ExecutorService.awaitTermination() interrupted", intEx)
        }

      }

      logger.info("JMS Consumers stopped")
    } // End of synchronized block on m_lock

  }

}
