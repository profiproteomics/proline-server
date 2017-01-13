package fr.proline.module.seq.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;

import ch.qos.logback.classic.Level;
import fr.profi.util.ThreadLogger;
import fr.proline.core.orm.uds.repository.ProjectRepository;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;

/**
 * Launchs Sequence-Repository retrieve Service as single pass or as scheduled task (timer).
 * 
 * @author LMN & AR & AW
 * 
 */
public final class RetrieveService {

	private static final Logger LOG = LoggerFactory.getLogger(RetrieveService.class);

	private static final long TIMER_BEFORE_DELAY = TimeUnit.SECONDS.toMillis(15L);

	public static class RetrieveCommand {
		@Parameter(names = { "-p", "--project" }, description = "the ID of the single project to process")
		private Integer projectId = 0;

		@Parameter(names = { "-debug" }, description = "set logger level to DEBUG (verbose mode)")
		private boolean debug = false;

		@Parameter(names = { "-t", "--time" }, description = "the daemon periodicity (in hours)")
		private Integer hourDelay = -1;

		@Parameter(names = { "-f", "--forceUpdate" }, description = "force update of MSIdb result summaries and biosequences (even if already updated)")
		private boolean forceUpdate = false;
	}

	private RetrieveService() {

	}

	/**
	 * Starts Sequence-Repository retrieve Service.
	 * 
	 * @param args
	 *            If used as periodic timer, first arg is timer delay in hour.
	 */
	public static void main(final String[] args) {

		Thread.currentThread().setUncaughtExceptionHandler(new ThreadLogger(LOG));

		final RetrieveCommand params = new RetrieveCommand();
		JCommander command = new JCommander(params, args);

		command.usage();

		if (params.debug) {
			ch.qos.logback.classic.Logger prolineLogger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger("fr.proline");
			prolineLogger.setLevel(Level.DEBUG);
			LOG.debug("--- Proline Logger set to DEBUG level ");
		}

		/* Force initialization of seq_db on service starting */
		DatabaseAccess.getSEQDatabaseConnector(true);

		if (params.hourDelay > 0) {
			final Timer timer = new Timer("Timer-retrieveBioSequencesForAllProjects");
			final boolean forceUpdate = params.forceUpdate;
			final TimerTask timerTask = new TimerTask() {

				public void run() {
					final Thread currentThread = Thread.currentThread();

					if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
						currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
					}

					try {
						retrieveBioSequencesForAllProjects(forceUpdate);

					} catch (Exception ex) {
						LOG.error("Error running  retrieveBioSequencesForAllProjects()", ex);
					}
				}

			};
			timer.scheduleAtFixedRate(timerTask, TIMER_BEFORE_DELAY, TimeUnit.HOURS.toMillis(params.hourDelay));

			LOG.info("Running \"retrieve task\" every {} hour(s)", params.hourDelay);
		} else {
			LOG.info("No given hourDelay : Running a single \"retrieve task\"");
			if (params.projectId == 0) {
				retrieveBioSequencesForAllProjects(params.forceUpdate);
			} else {
				retrieveBioSequencesForProject(params.projectId, params.forceUpdate);
			}
			BioSequenceRetriever.waitExecutorShutdown();
			System.out.println("\nMain terminated !");
		}

	}

	public static void retrieveBioSequencesForAllProjects(boolean forceUpdate) {
		int totalHandledSEDbIdents = 0;
		LOG.info("Start new pass on all projects ");
		final long start = System.currentTimeMillis();

		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();

		EntityManager udsEM = udsDbConnector.createEntityManager();
		EntityManager msiEM = null;
		
		List<Long> projectIds = getAllProjectIds(udsEM);
		Map<Long, List<Long>> rsmIdsPerProject = new HashMap();
		Map<Long, Map<Long, SEDbInstanceWrapper>> seDbInstancesPerProject  = new HashMap();
		
		if ((projectIds == null) || projectIds.isEmpty()) {
			LOG.warn("NO MSI Project found");
		} else {
			int size = projectIds.size();
			for (int i = size - 1; i >= 0; i--) {
				Long pId = projectIds.get(i);
				
				if (pId != null) {
					//Get data needed by fillXX methods
					final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(pId);
					msiEM = msiDbConnector.createEntityManager();					
					List<Long> rsmIds = ProjectHandler.retrieveRSMIdToFill(pId.longValue(), forceUpdate, null, udsEM, msiEM);
					rsmIdsPerProject.put(pId,rsmIds);
					
					final Map<Long, SEDbInstanceWrapper> seDbInstances = ProjectHandler.retrieveAllSeqDatabases(msiEM);
					seDbInstancesPerProject.put(pId,seDbInstances);
					
					ProjectHandler.fillSEDbIdentifiersBySEDb(pId.longValue(), seDbIdentifiers, seDbInstances, rsmIds);
					DatabaseAccess.getDataStoreConnectorFactory().closeProjectConnectors(pId);
				}
				if(msiEM.isOpen())
					msiEM.close();
			}

			if ((seDbIdentifiers == null) || seDbIdentifiers.isEmpty()) {
				LOG.warn("NO SEDbIdentifier found");
			} else {
				totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
				for (int i = size - 1; i >= 0; i--) {
					Long pId = projectIds.get(i);
					if (pId != null) {
						ProjectHandler.fillProteinMatchesProperties(pId.longValue(),seDbInstancesPerProject.get(pId), rsmIdsPerProject.get(pId));
						DatabaseAccess.getDataStoreConnectorFactory().closeProjectConnectors(pId);
					}
				}
			}
		}

		final long end = System.currentTimeMillis();
		final long duration = end - start;
		if(udsEM.isOpen()){udsEM.close();}
		LOG.info("Total retrieveBioSequencesForAllProjects() execution : {} SEDbIdentifiers handleds in {} ms", totalHandledSEDbIdents, duration);
	}

	public static void retrieveBioSequencesForProject(final long projectId, boolean forceUpdate) {

		LOG.debug("retrieveBioSequencesForProject running ");

		int totalHandledSEDbIdents = 0;

		final long start = System.currentTimeMillis();
		
		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();		
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		
		if (msiDbConnector == null) {
			LOG.warn("Project #{} has NO associated MSI Db", projectId);
		} else {
			EntityManager udsEM = udsDbConnector.createEntityManager();
			EntityManager msiEM = msiDbConnector.createEntityManager();
			long duration = 0; 
			try {
				final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();
				//Get data needed by fillXX methods 
				List<Long> rsmIds = ProjectHandler.retrieveRSMIdToFill(projectId, forceUpdate, null, udsEM, msiEM);
				final Map<Long, SEDbInstanceWrapper> seDbInstances = ProjectHandler.retrieveAllSeqDatabases(msiEM);
				
				ProjectHandler.fillSEDbIdentifiersBySEDb(projectId, seDbIdentifiers,seDbInstances,  rsmIds);
		
				if (seDbIdentifiers.isEmpty()) {
					LOG.warn("NO SEDbIdentifier found");
				} else {
					totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
				}
		
				ProjectHandler.fillProteinMatchesProperties(projectId, seDbInstances, rsmIds);
		
				final long end = System.currentTimeMillis();
				duration = end - start;
			} finally {
				if(udsEM.isOpen())
					udsEM.close();
				if(msiEM.isOpen())
					msiEM.close();				
			}
			LOG.info("Total retrieveBioSequencesForProject(#{}) execution : {} SEDbIdentifiers handled in {} ms", projectId, totalHandledSEDbIdents, duration);
		}
	}

	public static void retrieveBioSequencesForRsms(final long projectId, List<Long> rsmIds, boolean forceUpdate) {

		LOG.debug("retrieveBioSequencesForRsms running ");

		int totalHandledSEDbIdents = 0;

		final long start = System.currentTimeMillis();
		long duration = 0; 
		
		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();
		
		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();		
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		EntityManager udsEM = udsDbConnector.createEntityManager();
		EntityManager msiEM = msiDbConnector.createEntityManager();
		try { 
			//Get data needed by fillXX methods 
			List<Long> finalRsmIds = ProjectHandler.retrieveRSMIdToFill(projectId, forceUpdate, rsmIds, udsEM, msiEM);
			final Map<Long, SEDbInstanceWrapper> seDbInstances = ProjectHandler.retrieveAllSeqDatabases(msiEM);
	
	
			ProjectHandler.fillSEDbIdentifiersBySEDb(projectId, seDbIdentifiers, seDbInstances, finalRsmIds);
	
			if (seDbIdentifiers.isEmpty()) {
				LOG.warn("NO SEDbIdentifier found");
			} else {
				totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
			}
	
			ProjectHandler.fillProteinMatchesProperties(projectId,seDbInstances, rsmIds);
	
			final long end = System.currentTimeMillis();
			duration = end - start;
		} finally {
			if(udsEM.isOpen())
				udsEM.close();
			if(msiEM.isOpen())
				msiEM.close();	
		}
		LOG.info("Total retrieveBioSequencesForRsms in Project(#{}) execution : {} SEDbIdentifiers handled in {} ms",
			projectId, totalHandledSEDbIdents, duration);
	}

	private static List<Long> getAllProjectIds(EntityManager udsEM) {

		List<Long> projectIds = null;

		try {
			projectIds = ProjectRepository.findAllProjectIds(udsEM);
		} finally {
			if (udsEM != null) {
				try {
					udsEM.close();
				} catch (Exception exClose) {
					LOG.error("Error closing UDS Db EntityManager", exClose);
				}
			}
		}

		return projectIds;
	}

}

