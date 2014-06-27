package fr.proline.module.seq.service;

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

import fr.proline.core.orm.uds.repository.ProjectRepository;
import fr.proline.core.orm.util.DataStoreConnectorFactory;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.repository.IDatabaseConnector;
import fr.profi.util.ThreadLogger;

/**
 * Launchs Sequence-Repository retrieve Service as single pass or as scheduled task (timer).
 * 
 * @author LMN
 * 
 */
public final class RetrieveService {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveService.class);

    private static final long TIMER_BEFORE_DELAY = TimeUnit.SECONDS.toMillis(15L);

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

	/* Force initialization of seq_db on service starting */
	DatabaseAccess.getSEQDatabaseConnector(true);

	int hourDelay = -1;

	if ((args != null) && (args.length > 0)) {
	    final String trimmedDelay = args[0].trim();

	    if (!trimmedDelay.isEmpty()) {
		try {
		    hourDelay = Integer.parseInt(trimmedDelay);
		} catch (NumberFormatException nfEx) {
		    LOG.warn("Cannot parse [" + trimmedDelay + "] as Integer value", nfEx);
		}
	    }

	}

	if (hourDelay > 0) {
	    final Timer timer = new Timer("Timer-retrieveBioSequencesForAllProjects");

	    final TimerTask timerTask = new TimerTask() {

		public void run() {
		    final Thread currentThread = Thread.currentThread();

		    if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
			currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
		    }

		    try {
			retrieveBioSequencesForAllProjects();
		    } catch (Exception ex) {
			LOG.error("Error running  retrieveBioSequencesForAllProjects()", ex);
		    }

		}

	    };

	    timer.scheduleAtFixedRate(timerTask, TIMER_BEFORE_DELAY, TimeUnit.HOURS.toMillis(hourDelay));

	    LOG.info("Running \"retrieve task\" every {} hour(s)", hourDelay);
	} else {
	    LOG.info("No given hourDelay : Running a single \"retrieve task\"");

	    retrieveBioSequencesForAllProjects();

	    BioSequenceRetriever.waitExecutorShutdown();

	    System.out.println("\nMain terminated !");
	}

    }

    public static void retrieveBioSequencesForAllProjects() {
	int totalHandledSEDbIdents = 0;

	final long start = System.currentTimeMillis();

	final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = fillSEDbIdentifiersForAllProjects();

	if ((seDbIdentifiers == null) || seDbIdentifiers.isEmpty()) {
	    LOG.warn("NO SEDbIdentifier found");
	} else {
	    totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
	}

	final long end = System.currentTimeMillis();

	final long duration = end - start;

	LOG.info(
		"Total retrieveBioSequencesForAllProjects() execution : {} SEDbIdentifiers handled in {} ms",
		totalHandledSEDbIdents, duration);
    }

    public static void retrieveBioSequencesForProject(final long projectId) {
	int totalHandledSEDbIdents = 0;

	final long start = System.currentTimeMillis();

	final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();

	ProjectHandler.fillSEDbIdentifiersBySEDb(projectId, seDbIdentifiers);

	if (seDbIdentifiers.isEmpty()) {
	    LOG.warn("NO SEDbIdentifier found");
	} else {
	    totalHandledSEDbIdents = BioSequenceRetriever.retrieveBioSequences(seDbIdentifiers);
	}

	final long end = System.currentTimeMillis();

	final long duration = end - start;

	LOG.info("Total retrieveBioSequencesForProject(#{}) execution : {} SEDbIdentifiers handled in {} ms",
		projectId, totalHandledSEDbIdents, duration);
    }

    private static Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> fillSEDbIdentifiersForAllProjects() {
	Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = null;

	final DataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();

	final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
	final EntityManagerFactory emf = udsDbConnector.getEntityManagerFactory();

	List<Long> projectIds = null;

	EntityManager udsEM = emf.createEntityManager();

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

	if ((projectIds == null) || projectIds.isEmpty()) {
	    LOG.warn("NO MSI Project found");
	} else {
	    seDbIdentifiers = new HashMap<>();

	    for (final Long pId : projectIds) {

		if (pId != null) {
		    ProjectHandler.fillSEDbIdentifiersBySEDb(pId.longValue(), seDbIdentifiers);
		}

	    }

	}

	return seDbIdentifiers;
    }

}
