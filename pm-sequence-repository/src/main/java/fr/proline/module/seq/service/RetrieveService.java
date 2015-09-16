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
 * @author LMN
 * 
 */
public final class RetrieveService {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveService.class);

    private static final long TIMER_BEFORE_DELAY = TimeUnit.SECONDS.toMillis(15L);

	private static boolean force_update = false;

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
	long projectId = 0; 
	LOG.debug("number of arguments: " + args.length);
	LOG.info("Arguments can be either 1 for periodicity (in hours) or p1 for project 1");
	
	if ((args != null) && (args.length > 0)) {
	    
		String trimmedDelay = "";
		
		if(args[0].startsWith("p")) { // project id such as p1
			projectId = Long.parseLong(args[0].substring(1));
			LOG.debug(" is going to process project " + projectId);
		} else if (args[0].startsWith("f")) {
			force_update = true;
			LOG.debug(" force mode: will compute even if already computed");
			if(args[0].substring(1, 2).equals("p")) {
				projectId = Long.parseLong(args[0].substring(2));
				LOG.debug(" is going to process project " + projectId);
			}
		}
		else {
			trimmedDelay = args[0].trim();	
		}
		
	  
	    if (!trimmedDelay.isEmpty()) {
			try {
			    hourDelay = Integer.parseInt(trimmedDelay);
			    LOG.debug("\nPeriodicity: " + hourDelay);
			    
			} catch (NumberFormatException nfEx) {
			    LOG.warn("Cannot parse [" + trimmedDelay + "] as Integer value", nfEx);
			}
	    }
	    
	}

	if (hourDelay > 0 ) { 
		final Timer timer = new Timer("Timer-retrieveBioSequencesForAllProjects");

		final TimerTask timerTask = new TimerTask() {

			public void run() {
				final Thread currentThread = Thread.currentThread();

				if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
					currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
				}

				try {
					retrieveBioSequencesForAllProjects(force_update);
					
				} catch (Exception ex) {
					LOG.error("Error running  retrieveBioSequencesForAllProjects()", ex);
				}
			}

		};
		timer.scheduleAtFixedRate(timerTask, TIMER_BEFORE_DELAY, TimeUnit.HOURS.toMillis(hourDelay));

		LOG.info("Running \"retrieve task\" every {} hour(s)", hourDelay);
	} else {
		LOG.info("No given hourDelay : Running a single \"retrieve task\"");
		if(projectId==0) {
			retrieveBioSequencesForAllProjects(force_update);
		}
		else {
			retrieveBioSequencesForProject(projectId,force_update);
		}
		BioSequenceRetriever.waitExecutorShutdown();
		System.out.println("\nMain terminated !");
	}
	
	}
    
    public static void retrieveBioSequencesForAllProjects(boolean force_update2) {
	int totalHandledSEDbIdents = 0;
	LOG.debug("Computing data for ALL projects ");
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
		"Total retrieveBioSequencesForAllProjects() execution : {} SEDbIdentifiers handleds in {} ms",
		totalHandledSEDbIdents, duration);
    }
   

    public static void retrieveBioSequencesForProject(final long projectId, boolean force_update2) {
   
    	LOG.debug("retrieveBioSequencesForProject en cours ");

	int totalHandledSEDbIdents = 0;

	final long start = System.currentTimeMillis();

	final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers = new HashMap<>();

	ProjectHandler.fillSEDbIdentifiersBySEDb(projectId, seDbIdentifiers);
	ProjectHandler.fillSequenceMatchesByProteinMatch(projectId,force_update2);   
	

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

	final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();

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
		   ProjectHandler.fillSequenceMatchesByProteinMatch(pId.longValue(), force_update);   
		}

	    }
	}
	return seDbIdentifiers;
    }
    
}
