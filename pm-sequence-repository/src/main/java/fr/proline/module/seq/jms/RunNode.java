package fr.proline.module.seq.jms;

import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Queue;

import org.hornetq.api.core.TransportConfiguration;
import org.hornetq.api.jms.HornetQJMSClient;
import org.hornetq.api.jms.JMSFactoryType;
import org.hornetq.core.remoting.impl.netty.NettyConnectorFactory;
import org.hornetq.core.remoting.impl.netty.TransportConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.ThreadLogger;
import fr.proline.jms.ServiceRegistry;
import fr.proline.jms.ServiceRunner;
import fr.proline.jms.util.JMSConstants;
import fr.proline.jms.util.MonitoringTopicPublisherRunner;
import fr.proline.jms.util.NodeConfig;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.jms.service.RetrieveJMSService;

public class RunNode {

	private static Integer EXECUTOR_SHUTDOWN_TIMEOUT = 30; // 30 seconds
	private static final Logger LOG = LoggerFactory.getLogger(RunNode.class);
	private static final Object m_lock = new Object();

	private String m_jmsServerHost = null;
	private Integer m_jmsServerPort = null;

	/* All mutable fields are @GuardedBy("m_lock") */
	private Connection m_connection = null;
	private ExecutorService m_executor = null;

	public RunNode(String jmsHost, Integer jmsPort) {
		assert jmsHost != null : "Invalid JMS Server Host name or address";
		assert ((0 < jmsPort) && (jmsPort <= JMSConstants.MAX_JMS_SERVER_PORT())) : "Invalid JMS Server port";
		m_jmsServerHost = jmsHost;
		m_jmsServerPort = jmsPort;

	}

	public static void main(String[] args) {
		Thread.currentThread().setUncaughtExceptionHandler(new ThreadLogger(LOG.getName()));

		RunNode runner = new RunNode(NodeConfig.getJMSServerHost(), NodeConfig.getJMSServerPort());
		runner.addShutdownHook(runner);

		/* Start JMS Consumers */
		runner.startJMSConsumers();
	}

	/**
	 * Starts JMS Connection and Executor running Consumers receive loop.
	 */
	private void startJMSConsumers() {

		synchronized (m_lock) {

			if (m_connection != null) {
				throw new IllegalStateException("JMS Consumers already started !");
			}

			try {
				// Step 1. Directly instantiate the JMS Queue object.
				Queue serviceRequestQueue = HornetQJMSClient.createQueue(NodeConfig.PROLINE_SERVICE_REQUEST_QUEUE_NAME());
				LOG.debug("JMS Queue : " + serviceRequestQueue);

				// Step 2. Instantiate the TransportConfiguration object which contains the knowledge of what transport to use,
				// The server port etc.

				HashMap<String, Object> connectionParams = new HashMap<String, Object>();
				connectionParams.put(TransportConstants.HOST_PROP_NAME, m_jmsServerHost); // JMS Server hostname or IP
				connectionParams.put(TransportConstants.PORT_PROP_NAME, java.lang.Integer.valueOf(m_jmsServerPort)); // JMS port

				TransportConfiguration transportConfiguration = new TransportConfiguration(NettyConnectorFactory.class.getName(), connectionParams);

				// 	Step 3 Directly instantiate the JMS ConnectionFactory object using that TransportConfiguration
				ConnectionFactory cf = HornetQJMSClient.createConnectionFactoryWithoutHA(JMSFactoryType.CF, transportConfiguration);

				// Step 4.Create a JMS Connection	
				m_connection = cf.createConnection();

				// Add an ExceptionListener to handle asynchronous Connection problems
				ExceptionListener exceptionListener = new ExceptionListener() {

					@Override
					public void onException(JMSException exception) {
						LOG.error("Asynchronous JMS Connection problem", exception);

					}
				};

				m_connection.setExceptionListener(exceptionListener);

				LOG.info("This Node Id : " + NodeConfig.NODE_ID());

				DatabaseAccess.initDataStoreConnectorFactory();

				/* Force initialization of seq_db on service starting */
				DatabaseAccess.getSEQDatabaseConnector(true);

				initServices();

				/* Create Executor */
				m_executor = Executors.newCachedThreadPool();

				MonitoringTopicPublisherRunner serviceMonitoringNotifier = new MonitoringTopicPublisherRunner(m_connection);
				m_executor.submit(serviceMonitoringNotifier);


				/* Add Parallelizable SeviceRunner */
				LOG.debug("Starting " + NodeConfig.SERVICE_THREAD_POOL_SIZE() + " Parallelizable ServiceRunners");

				for (int i = 0; i < NodeConfig.SERVICE_THREAD_POOL_SIZE(); i++) {
					ServiceRunner parallelizableSeviceRunner = new ServiceRunner(serviceRequestQueue, m_connection, serviceMonitoringNotifier,true);
					m_executor.submit(parallelizableSeviceRunner);
				}

				m_connection.start(); // Start connection to begin Consumer reception
				LOG.debug("JMS Connection : " + m_connection + "  started");
			} catch (Exception ex) {
				LOG.error("Error starting JMS Consumers", ex);
				stopJMSConsumers();

			}

		} // End of synchronized block on m_lock

	}

	private void addShutdownHook(RunNode server) {

		assert (server != null) : "addShutdownHook() server is null";

		Runnable target = new Runnable() {

			@Override
			public void run() {
				LOG.debug("Stopping JMS Consumers for SeqRepository");
				server.stopJMSConsumers();
			}
		};

		Thread thr = new Thread(target, "Thread-shutdownJMSConsumers");
		thr.setPriority(Thread.NORM_PRIORITY);
		thr.setUncaughtExceptionHandler(new ThreadLogger(LOG.getName()));

		Runtime.getRuntime().addShutdownHook(thr);
	}

	private void stopJMSConsumers() {
		synchronized (m_lock) {
			if (m_connection != null) {
				LOG.debug("Closing JMS Connection");

				try {
					m_connection.close();
					LOG.info("JMS Connection closed");
				} catch (JMSException exClose) {
					LOG.error("Error closing JMS Connection", exClose);
				}
			}

			if (m_executor != null) {
				LOG.debug("Stopping JMS Consumers Executor");
				m_executor.shutdown();

				LOG.debug("Waiting " + EXECUTOR_SHUTDOWN_TIMEOUT + " seconds for Executor termination...");

				try {

					if (m_executor.awaitTermination(EXECUTOR_SHUTDOWN_TIMEOUT, TimeUnit.SECONDS)) {
						LOG.info("JMS Consumers Executor terminated");

					} else {
						List<Runnable> remainingRunnables = m_executor.shutdownNow();
						LOG.info("JMS Consumers Executor terminated remain " + remainingRunnables.size() + " never commenced Runnable(s)");
					}

				} catch (InterruptedException intEx) {
					LOG.warn("ExecutorService.awaitTermination() interrupted", intEx);
				}

			}

			LOG.info("JMS Consumers stopped");

		}
	}

	private void initServices() {

		/* Parallelizable Service */
		//		    ServiceRegistry.addService(new InfoService()) // Monitoring
		ServiceRegistry.addService(new RetrieveJMSService());
	}
}
