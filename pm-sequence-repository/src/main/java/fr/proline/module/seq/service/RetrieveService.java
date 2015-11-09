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
import javax.persistence.EntityManagerFactory;

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
		final EntityManagerFactory emf = udsDbConnector.getEntityManagerFactory();

		EntityManager udsEM = emf.createEntityManager();
		List<Long> projectIds = getAllProjectIds(udsEM);

		if ((projectIds == null) || projectIds.isEmpty()) {
			LOG.warn("NO MSI Project found");
		} else {
			int size = projectIds.size();
			for (int i = size - 1; i >= 0; i--) {
				Long pId = projectIds.get(i);
				if (pId != null) {
					ProjectHandler.fillSEDbIdentifiersBySEDb(pId.longValue(), seDbIdentifiers, null, forceUpdate);
					DatabaseAccess.getDataStoreConnectorFactory().closeProjectConnectors(pId);
				}
			}
		}

		if ((seDbIdentifiers == null) || seDbIdentifiers.isEmpty()) {
			LOG.warn("NO SEDbIdentifier found");
		} else {
			totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
			int size = projectIds.size();
			for (int i = size - 1; i >= 0; i--) {
				Long pId = projectIds.get(i);
				ProjectHandler.fillProteinMatchesProperties(pId.longValue(), null, forceUpdate);
				DatabaseAccess.getDataStoreConnectorFactory().closeProjectConnectors(pId);
			}
		}

		final long end = System.currentTimeMillis();
		final long duration = end - start;

		LOG.info("Total retrieveBioSequencesForAllProjects() execution : {} SEDbIdentifiers handleds in {} ms", totalHandledSEDbIdents, duration);
	}

	public static void retrieveBioSequencesForProject(final long projectId, boolean forceUpdate) {

		LOG.debug("retrieveBioSequencesForProject running ");

		int totalHandledSEDbIdents = 0;

		final long start = System.currentTimeMillis();
		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();
		ArrayList<Long> rsmIds = new ArrayList<Long>();
		rsmIds.add(783l);
		ProjectHandler.fillSEDbIdentifiersBySEDb(projectId, seDbIdentifiers, rsmIds, forceUpdate);

		if (seDbIdentifiers.isEmpty()) {
			LOG.warn("NO SEDbIdentifier found");
		} else {
			totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
		}

		ProjectHandler.fillProteinMatchesProperties(projectId, rsmIds, forceUpdate);

		final long end = System.currentTimeMillis();
		final long duration = end - start;

		LOG.info("Total retrieveBioSequencesForProject(#{}) execution : {} SEDbIdentifiers handled in {} ms",
			projectId, totalHandledSEDbIdents, duration);
	}

	public static void retrieveBioSequencesForRsms(final long projectId, List<Long> rsmIds, boolean forceUpdate) {

		LOG.debug("retrieveBioSequencesForRsms running ");

		int totalHandledSEDbIdents = 0;

		final long start = System.currentTimeMillis();
		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();

		ProjectHandler.fillSEDbIdentifiersBySEDb(projectId, seDbIdentifiers, rsmIds, forceUpdate);

		if (seDbIdentifiers.isEmpty()) {
			LOG.warn("NO SEDbIdentifier found");
		} else {
			totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
		}

		ProjectHandler.fillProteinMatchesProperties(projectId, rsmIds, forceUpdate);

		final long end = System.currentTimeMillis();
		final long duration = end - start;

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
