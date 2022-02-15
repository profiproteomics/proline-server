package fr.proline.cortex

import java.io.File
import java.util.ArrayList
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
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.StringUtils
import fr.profi.util.ThreadLogger
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.orm.uds.Project
import fr.proline.core.orm.util.JsonSerializer
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.service.admin.CreateProject
import fr.proline.cortex.service.admin.GetConnectionTemplate
import fr.proline.cortex.service.admin.UserAccount
import fr.proline.cortex.service.dps.msi._
import fr.proline.cortex.service.dps.msq._
import fr.proline.cortex.service.dps.uds.GetExportInformation
import fr.proline.cortex.service.dps.uds.RegisterRawFile
import fr.proline.cortex.service.dps.uds.ValidateIdentDSInTree
import fr.proline.cortex.service.misc.CancelService
import fr.proline.cortex.service.misc.FileSystem
import fr.proline.cortex.service.misc.FileUpload
import fr.proline.cortex.service.misc.ProlineResourceService
import fr.proline.cortex.service.monitoring.InfoService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.cortex.util.fs.WorkDirectoryRegistry
import fr.proline.jms.ServiceManager
import fr.proline.jms.ServiceRegistry
import fr.proline.jms.ServiceRunner
import fr.proline.jms.SingleThreadedServiceRunner
import fr.proline.jms.util.ExpiredMessageConsumer
import fr.proline.jms.util.JMSConstants.MAX_JMS_SERVER_PORT
import fr.proline.jms.util.MonitoringTopicPublisherRunner
import fr.proline.jms.util.NodeConfig
import javax.jms.{Connection, ExceptionListener, InvalidDestinationException, JMSException, Session}

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._

object ProcessingNode extends LazyLogging {

  /* Constants */
  private val EXECUTOR_SHUTDOWN_TIMEOUT = 30 // 30 seconds : te be sure currently running services could have a chance to terminate 

  def main(args: Array[String]) {
    Thread.currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName))

    val server = new ProcessingNode(NodeConfig.getJMSServerHost, NodeConfig.getJMSServerPort)
    addShutdownHook(server)

    /* Start JMS Consumers */
    server.startJMSConsumers()
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

class ProcessingNode(jmsServerHost: String, jmsServerPort: Int) extends LazyLogging {

  import ProcessingNode._

  /* Constructor checks */
  require(!StringUtils.isEmpty(jmsServerHost), "Invalid JMS Server Host name or address")

  require(0 < jmsServerPort && jmsServerPort <= MAX_JMS_SERVER_PORT, "Invalid JMS Server port")

  private val m_lock = new Object()

  /* All mutable fields are @GuardedBy("m_lock") */
  private var m_connection: Connection = _

  private var m_paralleleExecutor: ExecutorService = _
  private var m_singleExecutor: ArrayBuffer[ExecutorService] = new ArrayBuffer[ExecutorService]()

  /**
   * Starts JMS Connection and Executor running Consumers receive loop.
   */
  def startJMSConsumers() {

    var tmpSession : Session = null

    m_lock.synchronized {

      if (m_connection != null) {
        throw new IllegalStateException("JMS Consumers already started !")
      }

      try {
        // Step 1. Directly instantiate the JMS Queue object.
        val serviceRequestQueue = HornetQJMSClient.createQueue(NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME)

        logger.trace("JMS Queue : " + serviceRequestQueue)
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
       cf.setReconnectAttempts(10)

        // Step 4.Create a JMS Connection
        m_connection = cf.createConnection()

        //Test queue exist => generate exception if not
        tmpSession = m_connection.createSession(false, Session.AUTO_ACKNOWLEDGE)
        tmpSession.createConsumer(serviceRequestQueue)
        try {
            tmpSession.close()
        } catch {
            case exClose: JMSException => logger.error("Error closing temporary JMS Session", exClose)
        }


        // Add an ExceptionListener to handle asynchronous Connection problems
        val exceptionListener = new ExceptionListener() {
          override def onException(exception: JMSException): Unit = {
            logger.error("Asynchronous JMS Connection problem", exception)
          }
        }

        m_connection.setExceptionListener(exceptionListener)

        logger.info("Running Cortex Processing Node Id : " + NodeConfig.NODE_ID)

        initFileSystem()

        DbConnectionHelper.initDataStore()

        initServices()

        val threadPoolSize = NodeConfig.SERVICE_THREAD_POOL_SIZE

        // Calculate how projects group is to be created => corresponding to import threads number : (SERVICE_THREAD_POOL_SIZE_KEY % 5) + 1
        val nbrProjectGroupsMax =  (threadPoolSize / 5) + 1
        //VDS WARNING. Currently Index 0 = all other projects. Change to "Not Project in groups" so list at index 0 contains all Ids of all groups
        val projectIdsByGroup = getProjectIdsByGroup(nbrProjectGroupsMax)
        val nbrProjectGroups = if(projectIdsByGroup != null) projectIdsByGroup.size else 0

        /**** Create Executor for poolThreads and SingleThreads service ***/
        /* Create Executor */
        m_paralleleExecutor = Executors.newCachedThreadPool()

        val serviceMonitoringNotifier = new MonitoringTopicPublisherRunner(m_connection)
        m_paralleleExecutor.submit(serviceMonitoringNotifier)

        /* Manage SingleThreadedServiceRunners : List of Services (a SingleThreadRunner) by ThreadIdent */
        //Inner method to create SingleThreadRunner and add it to list/maps
        def createSingleThreadRunner(singleThreadIdent: String, pIds: ArrayList[Long], useIdsAsNot : Boolean = false): Unit ={
          val singleThreadedServiceRunner = new SingleThreadedServiceRunner(serviceRequestQueue, m_connection, serviceMonitoringNotifier, singleThreadIdent, true)
          if(pIds != null && !pIds.isEmpty)
            singleThreadedServiceRunner.setProjectIdsSelector(pIds.asScala.toArray, useIdsAsNot)
          val singleThreadExecutor = Executors.newSingleThreadExecutor()
          m_singleExecutor += singleThreadExecutor
          val singleThreadFuture = singleThreadExecutor.submit(singleThreadedServiceRunner)
          ServiceManager.addRunnale2FutureEntry(singleThreadedServiceRunner, singleThreadFuture)
        }

        val handledSingleThreadedServiceIdents = ServiceRegistry.getSingleThreadedServicesByThreadIdent().keySet
        var nbrSingleThreads = 0
        for (threadIdent <- handledSingleThreadedServiceIdents) { //For each list of service (for a specific ThreadIdent)

          //Manage specific ImportServiceThread : if more than one group and at threadPoolSize >=  5
          if(threadIdent.equals(SingleThreadIdentifierType.IMPORT_SINGLETHREAD_IDENT.toString) && nbrProjectGroups >0 && threadPoolSize>=5){
            for(index <- 0 until nbrProjectGroups){
              val useIdsAsNot = index == 0
              createSingleThreadRunner(threadIdent, projectIdsByGroup.get(index),useIdsAsNot)
              nbrSingleThreads += 1
            }
          } else {
            createSingleThreadRunner(threadIdent, null)
            nbrSingleThreads += 1
          }
        }

        // Start Expired Message Listener
        val expiredMessageConsumer = new ExpiredMessageConsumer(expiredRequestQueue, m_connection, serviceMonitoringNotifier)
        m_paralleleExecutor.submit(expiredMessageConsumer)
        nbrSingleThreads += 1

        logger.debug(nbrSingleThreads +" Single Thread ServiceRunners started")

        /* Add Parallelizable SeviceRunner */
        logger.debug("Starting " + NodeConfig.SERVICE_THREAD_POOL_SIZE + " Parallelizable ServiceRunners")

        for (i <- 1 to NodeConfig.SERVICE_THREAD_POOL_SIZE) {
          val logSelector = if(i==1) true else false
          val parallelizableSeviceRunner = new ServiceRunner(serviceRequestQueue, m_connection, serviceMonitoringNotifier,logSelector)
          val threadFuture = m_paralleleExecutor.submit(parallelizableSeviceRunner)
          ServiceManager.addRunnale2FutureEntry(parallelizableSeviceRunner, threadFuture)
        }

        m_connection.start() // Explicitly start connection to begin Consumer reception
        logger.info(" ************ Proline Cortex successfully started !")
      } catch {

        case ide: InvalidDestinationException => {
          logger.error("WARNING: JMS Server may not be configured correctly. Verify it's host, port and queue name in jms-node.config !", ide)
          closeSession(tmpSession)
        }

        case ex: Throwable => {
          logger.error("Error starting JMS Consumers", ex)
          closeSession(tmpSession)
        }

      }

    } // End of synchronized block on m_lock
  }

  private def closeSession (tmpSession : Session): Unit = {
    if (tmpSession != null) {
      try {
        tmpSession.close()
      } catch {
        case exClose: JMSException => logger.error("Error closing temporary JMS Session", exClose)
      }
    }
    stopJMSConsumers()
  }

  /**
   * Group project Ids depending on their serializedProperties. If too much group specified, few groups will be regrouped
   * to keep only "nbrGroupMax" group.
   * The first group correspond to all unclassified projects. Currently this will contains Ids of all other group to apply
   * a NOT IN operator.
   * In further version, we should modify it to be able to add new Ids dynamically when Create Project is called
   *
   */
  private def getProjectIdsByGroup(nbrGroupMax : Int): ArrayList[ArrayList[Long]] = {
    val connectorFactory = DbConnectionHelper.getDataStoreConnectorFactory()
    val udsDbConnectionContext = new DatabaseConnectionContext(connectorFactory.getUdsDbConnector)

    try{
      val allProjects = udsDbConnectionContext.getEntityManager.createQuery("SELECT p FROM fr.proline.core.orm.uds.Project p", classOf[Project]).getResultList
      val projectIdsByGroup = new mutable.HashMap[Long, ArrayList[Long]]()

      //Create Map of Project Ids list by import group number
      allProjects.asScala.foreach(p => {
        var groupeId: Int = -1
        val serializedPropertiesStr = p.getSerializedProperties
        if (serializedPropertiesStr != null && !serializedPropertiesStr.isEmpty) { //an import group property may have be specified
          val serializedPropertiesMap = JsonSerializer.getMapper.readValue(serializedPropertiesStr, classOf[java.util.Map[String, Object]])
          if (serializedPropertiesMap.containsKey("import_group")) {
            try {
              groupeId = serializedPropertiesMap.get("import_group").asInstanceOf[Int]
            } catch {
              case  ex: ClassCastException => groupeId = -1
            }

            if (groupeId != -1) {
              val pIds = projectIdsByGroup.getOrElseUpdate(groupeId, new ArrayList[Long]())
              pIds.add(p.getId)
            }
          }
        }

        if (groupeId == -1) { // No import group property specified, group all "inclassified" project into group -1
          val pIds = projectIdsByGroup.getOrElseUpdate(groupeId, new ArrayList[Long]())
          pIds.add(p.getId)
        }
      })


      //No Project found at all !
      if(projectIdsByGroup.isEmpty) {
        return  new ArrayList[ArrayList[Long]]()
      }

      val allProjectsClassed = new ArrayList[ArrayList[Long]]()

      // Set Unclassified projects at index 0: Even if empty.
//      if(projectIdsByGroup.containsKey(-1)) { //Unclassified projects defined
//        allProjectsClassed.add(0,projectIdsByGroup(-1))
//      } else
//        allProjectsClassed.add(0,new util.ArrayList[Long]())
      // VDS Create a first group with all grouped Ids
      val allClassedProjectsId = new ArrayList[Long]()
      projectIdsByGroup.foreach(projBygId  =>{
        if(projBygId._1 != -1)
          allClassedProjectsId.addAll(projBygId._2)
      })
      allProjectsClassed.add(0,allClassedProjectsId)

      //If less project groups was specified than max group count : just return defined groups
      if(projectIdsByGroup.size <= nbrGroupMax){
        for(key <- projectIdsByGroup.keySet ){
          if(key != -1) {
            allProjectsClassed.add(projectIdsByGroup(key))
          }
        }

      } else {
        //should regroup projects group to have only  "max group count" group
        var startIndex = 1  // Don't group other projects with "Unclassified projects"
        var allProjectsIndex = startIndex
        for(key <- projectIdsByGroup.keySet ){
          if(key != -1){

            val projectsInGroup = new ArrayList[Long]()
            projectsInGroup.addAll(projectIdsByGroup(key)) // get current project group

            if(allProjectsIndex < allProjectsClassed.size()) { //There are some Projects registered at allProjectsIndex
              projectsInGroup.addAll(allProjectsClassed.get(allProjectsIndex))
              allProjectsClassed.set(allProjectsIndex, projectsInGroup)
            } else
              allProjectsClassed.add(allProjectsIndex, projectsInGroup)

            allProjectsIndex = allProjectsIndex+1
            if(allProjectsIndex == nbrGroupMax) // when max is reached, restart grouping projects from index 1
              allProjectsIndex = startIndex
          }
        }
      }
      allProjectsClassed
    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbConnectionContext)
    }
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

    if (NodeConfig.ENABLE_IMPORTS) {
      ServiceRegistry.addService(new ImportResultFilesDecoyRegExp())
      ServiceRegistry.addService(new ImportResultFilesProtMatchDecoyRule())
      ServiceRegistry.addService(new ImportValidateGenerateSM())
      logger.info("This node HANDLES Result Files Import")
    } else {
      logger.info("This node does NOT handle Result Files Import")
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
    ServiceRegistry.addService(new FilterRsmProteinSets())
    ServiceRegistry.addService(new ComputeQuantProfiles())
    ServiceRegistry.addService(new ComputeQuantPostProcessing())
    ServiceRegistry.addService(new ComputeQuantPostProcessingV2())

    ServiceRegistry.addService(new Quantify())
    ServiceRegistry.addService(new QuantifySC())
    ServiceRegistry.addService(new QuantifySC_V02())
    ServiceRegistry.addService(new AggregateQuantitation())
    ServiceRegistry.addService(new ExtractChromatogram())

    ServiceRegistry.addService(new UserAccount())
    ServiceRegistry.addService(new GetConnectionTemplate())
    ServiceRegistry.addService(new CreateProject())
    ServiceRegistry.addService(new RegisterRawFile())
    ServiceRegistry.addService(new DeleteOrphanData())
    ServiceRegistry.addService(new QuantifyV3_0())
    ServiceRegistry.addService(new ImportMaxQuantResults())
    ServiceRegistry.addService(new ImportMaxQuantResultsV2_0())

    ServiceRegistry.addService(new IdentifyPtmSites())
    ServiceRegistry.addService(new IdentifyPtmSitesV2_0())
    ServiceRegistry.addService(new ProlineResourceService())
    //VDS TEST only !
//    ServiceRegistry.addService(new WaitService())
    ServiceRegistry.addService(new ValidateResultSetV2())
    ServiceRegistry.addService(new ValidateResultSetV3())
    ServiceRegistry.addService(new CancelService())
 }

  /**
   * Gracefully closes JMS Connection and stops the Executor running Consumers receive loop.
   */
  def stopJMSConsumers() {

    m_lock.synchronized {

      if (m_connection != null) {
        logger.trace("Closing JMS Connection")

        try {
          m_connection.close()
          logger.info("JMS Connection closed")
        } catch {
          case exClose: JMSException => logger.error("Error while closing the JMS Connection", exClose)
        }

      }

      if (m_paralleleExecutor != null) {
        logger.trace("Stopping JMS Consumers Executor")
        m_paralleleExecutor.shutdown()

        logger.debug("Waiting " + EXECUTOR_SHUTDOWN_TIMEOUT + " seconds for Executor termination...")

        try {

          if (m_paralleleExecutor.awaitTermination(EXECUTOR_SHUTDOWN_TIMEOUT, TimeUnit.SECONDS)) {
            logger.info("JMS Consumers Executor terminated")
          } else {
            val remainingRunnables = m_paralleleExecutor.shutdownNow()
            logger.info(s"JMS Consumers Executor terminated, ${remainingRunnables.size} remaining Runnable(s) that never started.")
          }

        } catch {
          case intEx: InterruptedException => logger.warn("ExecutorService.awaitTermination() interrupted", intEx)
        }

      }

      if(m_singleExecutor.nonEmpty){
          m_singleExecutor.foreach( executor => {
            logger.trace("Stopping JMS Consumers Single Thread Executor "+executor.toString)
            executor.shutdown

            logger.debug("Waiting " + EXECUTOR_SHUTDOWN_TIMEOUT + " seconds for Executor termination...")

            try {

              if (executor.awaitTermination(EXECUTOR_SHUTDOWN_TIMEOUT, TimeUnit.SECONDS)) {
                logger.info("JMS Consumers Executor terminated")
              } else {
                val remainingRunnables = executor.shutdownNow()
                logger.info(s"JMS Consumers Executor terminated, ${remainingRunnables.size} remaining Runnable(s) that never started.")
              }

            } catch {
              case intEx: InterruptedException => logger.warn("ExecutorService.awaitTermination() interrupted", intEx)
            }
        })

      }

      logger.info("JMS Consumers stopped")
    } // End of synchronized block on m_lock

  }

}
