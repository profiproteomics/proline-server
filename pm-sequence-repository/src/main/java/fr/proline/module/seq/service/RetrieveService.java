package fr.proline.module.seq.service;

import java.util.*;
import java.util.concurrent.TimeUnit;

import fr.proline.module.seq.dto.DDatabankInstance;
import fr.proline.module.seq.dto.DDatabankProtein;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;

import ch.qos.logback.classic.Level;
import fr.profi.util.ThreadLogger;
import fr.proline.module.seq.DatabaseAccess;

import static fr.proline.module.seq.Constants.PERSISTENCE;

/**
 * Launch Sequence-Repository retrieve Service as single pass or as scheduled task (timer).
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

		@Parameter(names = { "-rsm" }, description = "specify rsm id to update")
		private Integer rsmId = 0;

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
			try {
				if (params.projectId == 0) {
					retrieveBioSequencesForAllProjects(params.forceUpdate);
				} else {
					List<Long> rsmIds = new ArrayList<>();
					if(params.rsmId>0)
						rsmIds.add(params.rsmId.longValue());
					retrieveBioSequences(params.projectId, params.forceUpdate, rsmIds);
				}
				BioSequenceRetriever.waitExecutorShutdown();
				System.out.println("\nMain terminated !");
				System.exit(0);
			} catch(Exception e){
				LOG.error("Error running  retrieveBioSequences ", e);
				System.out.println("\nMain terminated with ERROR");
				System.exit(1);
			}
		}

	}

	public static void retrieveBioSequencesForAllProjects(boolean forceUpdate) throws Exception {

		LOG.info("Start RetrieveService for all projects");

		int persistedProteinsCount = 0;
		final long start = System.currentTimeMillis();
		List<Long> projectIds = ProjectHandler.retrieveAllActiveProjectIds();

		if ((projectIds == null) || projectIds.isEmpty()) {
			LOG.warn("NO MSI Project found");
		} else {
			for (Long projectId : projectIds) {
				if (projectId != null) {
					//Get data needed by fillXX methods
					ProjectHandler projectHandler = new ProjectHandler(projectId);
					List<Long> rsmIds = projectHandler.findAllRSMIds();
					persistedProteinsCount += _retrieveBioSequences(projectHandler, forceUpdate, rsmIds);
					DatabaseAccess.getDataStoreConnectorFactory().closeProjectConnectors(projectId);
				}
			}
		}

		long duration = System.currentTimeMillis() - start;
		LOG.info("Total for all projects execution: {} Protein Identifiers handled in {} ms", persistedProteinsCount, duration);
	}

	public static int retrieveBioSequences(final long projectId, boolean forceUpdate, List<Long> rsmIds) throws Exception {
		final long start = System.currentTimeMillis();
		ProjectHandler projectHandler = new ProjectHandler(projectId);
		if(rsmIds == null ||rsmIds.isEmpty()){
			//Update all RSMs
			rsmIds = projectHandler.findAllRSMIds();
		}
		LOG.info("Start RetrieveBioSequences for RSMs {} of Project #{}", rsmIds.toString(), projectId);

		int persistedProteinsCount = _retrieveBioSequences(projectHandler, forceUpdate, rsmIds);
		long duration = System.currentTimeMillis() - start;
		LOG.info("Total RetrieveBioSequences for RSMs of Project #{} execution: {} Protein Identifiers persisted in {} ms", projectId, persistedProteinsCount, duration);

		return persistedProteinsCount;
	}

	private static int _retrieveBioSequences(final ProjectHandler projectHandler, boolean forceUpdate, List<Long> rsmIds) throws Exception {

		int persistedProteinsCount = 0;

		if (projectHandler.isProjectActive()) {

			final Map<DDatabankInstance, Set<DDatabankProtein>> proteinsByDatabank = new HashMap<>();
			List<Long> finalRsmIds = projectHandler.filterRSMIdsToUpdate(rsmIds, forceUpdate);

			// Create 'seqDB' DDatabankInstance (name, fasta path and version if exist) from msiDB SeqDatabase information. Map these 'fake' object by msiSeqDB.id
			final Map<Long, DDatabankInstance> databankBySeqDatabase = projectHandler.retrieveAllSeqDatabases();
			// Create 'Fake' DDatabankProtein from info read in MSIdb and map these DDatabankProtein to DDatabankInstance  (same as in databankBySeqDatabase)
			projectHandler.findProteinIdentifiers(proteinsByDatabank, databankBySeqDatabase, finalRsmIds);

			if ((proteinsByDatabank == null) || proteinsByDatabank.isEmpty()) {
				LOG.warn("NO DatabankProtein found");
			} else {
				persistedProteinsCount = BioSequenceRetriever.retrieveBioSequences(proteinsByDatabank);
				if (PERSISTENCE) {
					projectHandler.fillProteinMatchesProperties(databankBySeqDatabase, proteinsByDatabank, finalRsmIds);
				}
			}

			projectHandler.close();

		} else {
			LOG.warn("Project #{} is not active", projectHandler.getProjectId());
		}

		return persistedProteinsCount;
	}

}


