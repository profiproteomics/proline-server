package fr.proline.module.seq.service;

import java.io.File;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.DateUtils;
import fr.profi.util.FileUtils;
import fr.profi.util.StringUtils;
import fr.profi.util.ThreadLogger;
import fr.proline.module.seq.Constants;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.config.ParsingRuleEntry;
import fr.proline.module.seq.config.SeqRepoConfig;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.module.seq.orm.Alphabet;
import fr.proline.module.seq.orm.BioSequence;
import fr.proline.module.seq.orm.Repository;
import fr.proline.module.seq.orm.RepositoryIdentifier;
import fr.proline.module.seq.orm.SEDb;
import fr.proline.module.seq.orm.SEDbIdentifier;
import fr.proline.module.seq.orm.SEDbInstance;
import fr.proline.module.seq.orm.SEDbInstanceComparator;
import fr.proline.module.seq.orm.repository.BioSequenceRepository;
import fr.proline.module.seq.orm.repository.RepositoryIdentifierRepository;
import fr.proline.module.seq.orm.repository.SEDbIdentifierRepository;
import fr.proline.module.seq.orm.repository.SEDbRepository;
import fr.proline.module.seq.util.HashUtil;
import fr.proline.module.seq.util.RegExUtil;
import fr.proline.repository.IDatabaseConnector;

public final class BioSequenceRetriever {

	public static final Pattern GENERIC_SE_DB_IDENT_PATTERN = Pattern.compile(">(\\S+)");

	/**
	 * Protect Write Transaction on SEQ Database.
	 */
	public static final Object SEQ_DB_WRITE_LOCK = new Object();

	private static final Logger LOG = LoggerFactory.getLogger(BioSequenceRetriever.class);

	/**
	 * Protect re-entrance of public retrieveBioSequences() method (Only one thread at a time).
	 */
	private static final Object RUNNING_LOCK = new Object();

	private static final ExecutorService EXECUTOR = Executors.newFixedThreadPool(Constants.calculateNThreads());

	private static final DataSourceBuilder DATA_SOURCE_BUILDER = new DataSourceBuilder();

	/* Private constructor (Utility class) */
	private BioSequenceRetriever() {
	}

	/**
	 * Blocks until all given SEDbInstance are searched.
	 * 
	 * @param seDbIdentifiers
	 * @return Number of handled (created or updated) SEDbIdentifiers.
	 */
	public static int retrieveBioSequences(final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers) {
		if ((seDbIdentifiers == null) || seDbIdentifiers.isEmpty()) {
			throw new IllegalArgumentException("Invalid seDbIdentifiers Map");
		}

		LOG.info("Retrieve Sequence from {} differents source ", seDbIdentifiers.size());

		int totalHandledSEDbIdents = 0;

		synchronized (RUNNING_LOCK) {// Only one Thread at a time
			final long start = System.currentTimeMillis();

			SeqRepoConfig.forcePropertiesReload();//Read back properties to take into account potential changes.
			DATA_SOURCE_BUILDER.forceRescanFastaFiles();

			final List<Future<Integer>> futures = new ArrayList<>();

			final Set<Map.Entry<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>>> entries = seDbIdentifiers.entrySet();

			for (final Map.Entry<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> entry : entries) {

				final Set<SEDbIdentifierWrapper> seDbIdentsW = entry.getValue();
				if ((seDbIdentsW != null) && !seDbIdentsW.isEmpty()) {

					final SEDbInstanceWrapper seDbInstanceW = entry.getKey();
					final Callable<Integer> task = new Callable<Integer>() {

						public Integer call() throws Exception {
							final Thread currentThread = Thread.currentThread();

							if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
								currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
							}

							return Integer.valueOf(retrieveBioSequences(seDbInstanceW, seDbIdentsW));
						}

					};

					final Future<Integer> future = EXECUTOR.submit(task);
					futures.add(future);
				}

			} // End loop for each seDbIdentifiers entries

			/* Wait (blocking) for all futures to complete */
			for (final Future<Integer> f : futures) {

				try {

					final Integer result = f.get();
					if (result != null) {
						final int nHandledSEDbIdents = result.intValue();

						if (nHandledSEDbIdents > 0) {
							totalHandledSEDbIdents += nHandledSEDbIdents;
						}

					}

				} catch (Exception ex) {
					LOG.error("Error trying to get Future result", ex);
				}

			}

			final long end = System.currentTimeMillis();

			final long duration = end - start;

			LOG.info("Total retrieveBioSequences() execution : {} SEDbIdentifiers retrieved from sources in {} ms", totalHandledSEDbIdents, duration);
		} // End of synchronized block on RUNNING_LOCK

		return totalHandledSEDbIdents;
	}

	public static boolean waitExecutorShutdown() {
		boolean result = false;

		try {
			EXECUTOR.shutdown();
			result = EXECUTOR.awaitTermination(Integer.MAX_VALUE, TimeUnit.SECONDS);
		} catch (Exception ex) {
			LOG.error("Error shutting-down BioSequenceRetriever EXECUTOR", ex);
		}

		return result;
	}

	/* Private methods */
	private static int retrieveBioSequences(final SEDbInstanceWrapper seDbInstanceW, final Set<SEDbIdentifierWrapper> seDbIdentifiers) {

		assert (seDbInstanceW != null) : "retrieveBioSequences() seDbInstanceW is null";
		int nHandledSEDbIdents = 0;
		EntityManager seqEM = null;

		try {
			final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(true);
			final EntityManagerFactory emf = seqDb.getEntityManagerFactory();
			seqEM = emf.createEntityManager();

			nHandledSEDbIdents = retrieveBioSequences(seqEM, seDbInstanceW, seDbIdentifiers, true);
		} catch (Throwable t) {
			/* Catch all ! */
			LOG.error("Error loading Sequences from [" + seDbInstanceW.getSourcePath() + ']', t);
		} finally {

			if (seqEM != null) {
				try {
					seqEM.close();
				} catch (Exception exClose) {
					LOG.error("Error closing SEQ Db EntityManager", exClose);
				}
			}

		}

		return nHandledSEDbIdents;
	}

	private static int retrieveBioSequences(
		final EntityManager seqEM,
		final SEDbInstanceWrapper seDbInstanceW,
		final Set<SEDbIdentifierWrapper> seDbIdentifiers,
		final boolean doRecurse) {
		assert (seDbInstanceW != null) : "retrieveBioSequences() seDbInstanceW is null";

		int nHandledSEDbIdents = 0;

		final Map<String, List<SEDbIdentifierWrapper>> identByValues = buildIdentbyValuesMap(seDbIdentifiers);

		final String seDbName = seDbInstanceW.getName();
		final String sourcePath = seDbInstanceW.getSourcePath();

		final String fastaFileName = FileUtils.extractFileName(sourcePath);

		/* Try to load SEDbInstance, SEDb, ParsingRule */
		SEDbInstance seDbInstance = loadSEDbInstance(seqEM, seDbInstanceW, fastaFileName);
		if (seDbInstance != null) {
			removeKnownIdentifiers(seqEM, seDbInstance, identByValues);
		}

		if (!identByValues.isEmpty()) {
			
//			String repositoryIdentRegex = null; //VD TODO How to manage it !! 
			String parsingRuleReleaseRegex = null;
			String seDbIdentRegex = null; 
			ParsingRuleEntry parsingRule = ParsingRuleEntry.getParsingRuleEntry(fastaFileName);
			if(parsingRule != null){
				parsingRuleReleaseRegex = parsingRule.getFastaReleaseRegEx();
				seDbIdentRegex = parsingRule.getProteinAccRegEx();
			}
			

			/* Retrieve or guess Parsing rules */
			final String release = parseReleaseVersion(fastaFileName, parsingRuleReleaseRegex, seDbInstance);
			
			Pattern seDbIdentPattern = null;
			if (seDbIdentRegex == null) {
				LOG.debug("Sequence source [{}] will be parsed with Default Protein Accession Regex", sourcePath);
				String defaultRegEx = SeqRepoConfig.getInstance().getDefaultProtAccRegEx();
				seDbIdentPattern = Pattern.compile(defaultRegEx, Pattern.CASE_INSENSITIVE);					
			} else {
				LOG.debug("Sequence source [{}] will be parsed using Protein Accession Regex {} ", fastaFileName, seDbIdentRegex);
				seDbIdentPattern = Pattern.compile(seDbIdentRegex, Pattern.CASE_INSENSITIVE);
			}

			Pattern repositoryIdentPattern = null;
//
//			if (repositoryIdentRegex != null) {
//				repositoryIdentPattern = Pattern.compile(repositoryIdentRegex, Pattern.CASE_INSENSITIVE);
//			}

			final DataSource fastaSource = DATA_SOURCE_BUILDER.buildFastaSource(fastaFileName, seDbIdentPattern, repositoryIdentPattern);

			if (fastaSource == null) {
				LOG.warn("NO FastaSource found for [{}]", sourcePath);

				if (doRecurse) {

					final File bestFastaFile = selectBestFile(fastaFileName, release, parsingRuleReleaseRegex);
					if (bestFastaFile != null) {
						LOG.info("Trying to load [{}] sequences from [{}]", sourcePath, bestFastaFile.getAbsolutePath());
						final SEDbInstanceWrapper fakeSEDbInstance = new SEDbInstanceWrapper(seDbName, null, bestFastaFile.getName());
						nHandledSEDbIdents = retrieveBioSequences(seqEM, fakeSEDbInstance, seDbIdentifiers, false);
					}

				}

			} else {

				final Map<SEDbIdentifierWrapper, String> foundSequences = fastaSource.retrieveSequences(identByValues);
				if ((foundSequences != null) && !foundSequences.isEmpty()) {

					if (LOG.isDebugEnabled()) {
						LOG.debug("Retrieved {} sequences from [{}]", foundSequences.size(), sourcePath);
					}

					/* Big write lock on SEQ Db */
					synchronized (SEQ_DB_WRITE_LOCK) {// Only one Thread at a time
						EntityTransaction seqTransac = seqEM.getTransaction();
						boolean transacOK = false;

						try {
							seqTransac.begin();
							transacOK = false;

							LOG.debug("SEQ Db WRITE Transaction begin");
							final long start = System.currentTimeMillis();

							if (seDbInstance == null) {
								seDbInstance = loadOrCreateSEDbInstance(seqEM, seDbInstanceW, null, release, fastaSource.getLastModifiedTime());
							} else {
								seDbInstance = seqEM.merge(seDbInstance);
							}

							SEDb seDb = seDbInstance.getSEDb();// Update seDb (should not be null)

							final Map<String, List<SEDbIdentifier>> existingSEDbIdents = loadExistingSEDbIdentifiers(seqEM, seDbName, foundSequences);

							if (LOG.isDebugEnabled()) {
								LOG.debug("Possible existing SEDbIdentifiers : {}", existingSEDbIdents.size());
							}

							/* BioSequence objects must be unique by sequence (normalized to Upper Case) */
							final Map<String, BioSequence> existingBioSequences = loadExistingBioSequences(seqEM, foundSequences);

							if (LOG.isDebugEnabled()) {
								LOG.debug("Existing BioSequences : {}", existingBioSequences.size());
							}

							final Repository repository = seDb.getRepository();

							Map<String, RepositoryIdentifier> existingRepositoryIdents = null;

							if (repository != null) {// Can be null
								final String repositoryName = repository.getName();// Should not be null
								existingRepositoryIdents = loadExistingRepositoryIdentifiers(seqEM, repositoryName, foundSequences);

								if (LOG.isDebugEnabled()) {
									LOG.debug("Possible existing RepositoryIdentifiers : {}",
										existingRepositoryIdents.size());
								}

							}

							final SeqContext context = new SeqContext(seqEM, seDbInstance, existingSEDbIdents, existingBioSequences, repository,
									existingRepositoryIdents);

							final Set<Map.Entry<SEDbIdentifierWrapper, String>> entries = foundSequences.entrySet();

							for (final Map.Entry<SEDbIdentifierWrapper, String> entry : entries) {
								final SEDbIdentifierWrapper seDbIdentW = entry.getKey();
								final String sequence = entry.getValue();

								if (handleSEDbIdentifier(context, seDbIdentW, sequence)) {
									++nHandledSEDbIdents;
								}

							}

							seqTransac.commit();
							transacOK = true;

							final long end = System.currentTimeMillis();

							final long duration = end - start;

							LOG.info(
								"SEQ Db WRITE Transaction committed : {} SEDbIdentifiers handled from [{}] in {} ms",
								nHandledSEDbIdents, sourcePath, duration);
						} finally {

							if ((seqTransac != null) && !transacOK) {
								try {
									seqTransac.rollback();
								} catch (Exception ex) {
									LOG.error("Error rollbacking SEQ Db EntityManager Transaction", ex);
								}
							}

						} // End of try / finally block on SEQ Db EntityManager Transaction

					} // End of synchronized block on SEQ_DB_WRITE_LOCK

				}

			}

		}

		return nHandledSEDbIdents;
	}

	private static String parseReleaseVersion(
		final String fastaFileName,
		final String parsingRuleReleaseRegex,
		final SEDbInstance seDbInstance) {
		String release = null;


		if(parsingRuleReleaseRegex != null)			
			release = RegExUtil.parseReleaseVersion(fastaFileName, parsingRuleReleaseRegex);
		
		if (release == null) {

			if (seDbInstance != null) {
				release = seDbInstance.getRelease();
			}

		} else {

			if (seDbInstance != null) {
				final String instanceRelease = seDbInstance.getRelease();// Should not be null

				if (!release.equals(instanceRelease)) {
					throw new RuntimeException("Inconsistent Release version");
				}

			}

		}

		return release;
	}
	



	/**
	 * Try to select an existing FASTA file just after expected SEDbInstance release.
	 * 
	 */
	private static File selectBestFile(
		final String fastaFileName,
		final String release,
		final String parsingRuleReleaseRegex) {
		assert (fastaFileName != null) : "selectBestFile() fastaFileName is null";

		File result = null;

		if (!StringUtils.isEmpty(release)) {

			final int releaseIndex = fastaFileName.indexOf(release);
			if (releaseIndex != -1) {

				final String namePart = fastaFileName.substring(0, releaseIndex);
				if (!StringUtils.isEmpty(namePart)) {

					final List<File> fastaFiles = DATA_SOURCE_BUILDER.locateFastaFile(namePart);
					if ((fastaFiles != null) && !fastaFiles.isEmpty()) {
						final NavigableMap<String, File> sortedFiles = new TreeMap<>();

						for (final File f : fastaFiles) {
							final long lastModifiedTime = f.lastModified();

							String fRelease = null;

							if (parsingRuleReleaseRegex != null) {
								fRelease = RegExUtil.parseReleaseVersion(f.getName(), parsingRuleReleaseRegex);
							}

							if (fRelease == null || StringUtils.isEmpty(fRelease)) {
								fRelease = DateUtils.formatReleaseDate(new Date(lastModifiedTime));
							}

							final File oldFile = sortedFiles.get(fRelease);

							if (oldFile == null) {
								sortedFiles.put(fRelease, f);
							} else {

								if (lastModifiedTime > oldFile.lastModified()) {
									LOG.info("Use latest version of [{}]", f.getAbsolutePath());
									sortedFiles.put(fRelease, f);
								}

							}

						} // End loop for each possible fastaFile

						/* First try file just after */
						final Map.Entry<String, File> ceilingEntry = sortedFiles.ceilingEntry(release);
						if (ceilingEntry != null) {
							result = ceilingEntry.getValue();
						}

						if (result == null) {
							/* Then try file just before */

							final Map.Entry<String, File> floorEntry = sortedFiles.floorEntry(release);
							if (floorEntry != null) {
								result = floorEntry.getValue();
							}

						}

					} // End if (fastaFiles is not empty)

				} // End if (namePart is not empty)

			} // End if (fastaFileName contains release)

		} // End if (release string is not empty)

		return result;
	}

	private static Map<String, List<SEDbIdentifierWrapper>> buildIdentbyValuesMap(final Set<SEDbIdentifierWrapper> seDbIdentifiers) {
		assert (seDbIdentifiers != null) : "buildIdentbyValuesMap() seDbIdentifiers Set is null";

		final Map<String, List<SEDbIdentifierWrapper>> result = new HashMap<>();

		for (final SEDbIdentifierWrapper sdi : seDbIdentifiers) {
			final String identValue = sdi.getValue();

			List<SEDbIdentifierWrapper> identifiers = result.get(identValue);

			if (identifiers == null) {
				identifiers = new ArrayList<>(1);// Assume one SEDbIdentWrapper by identValue

				result.put(identValue, identifiers);
			}

			identifiers.add(sdi);
		} // End loop for each seDbIdentifiers

		return result;
	}

	private static SEDbInstance loadSEDbInstance(
		final EntityManager seqEM,
		final SEDbInstanceWrapper seDbInstanceW,
		final String fastaFileName) {
		assert (seDbInstanceW != null) : "loadSEDbInstance() seDbInstanceW is null";

		final String seDbName = seDbInstanceW.getName();
		final String sourcePath = seDbInstanceW.getSourcePath();

		SEDbInstance result = null;

		/* First try to load by sourcePath */
		final List<SEDbInstance> foundSEDbInstances = SEDbRepository.findSEDbInstanceByNameAndSourcePath(seqEM, seDbName, sourcePath);
		if (foundSEDbInstances != null) {
			final int nInstances = foundSEDbInstances.size();

			if (nInstances == 1) {
				result = foundSEDbInstances.get(0);
			} else if (nInstances > 1) {
				LOG.warn("There are {} SEDbInstance for sourcePath [{}]", nInstances, sourcePath);
			}

		}

		if (result == null) {
			/* Then try to parse release and load by release */
					
			String parsingRuleReleaseRegex = null;
			ParsingRuleEntry pre = ParsingRuleEntry.getParsingRuleEntry(fastaFileName);
			if(pre != null) {
				parsingRuleReleaseRegex = pre.getFastaReleaseRegEx();      
				final String release = RegExUtil.parseReleaseVersion(fastaFileName, parsingRuleReleaseRegex);
				if (release != null && !StringUtils.isEmpty(release)) {
					result = SEDbRepository.findSEDbInstanceByNameAndRelease(seqEM, seDbName, release);
				}
			}
		}

		return result;
	}

	
	
	/**
	 * Removes Identifiers already known in the SeqDB from the supplied Map.
	 * 
	 * @param seqEM
	 * @param seDbInstance
	 * @param identByValues
	 */
	private static void removeKnownIdentifiers(
		final EntityManager seqEM,
		final SEDbInstance seDbInstance,
		final Map<String, List<SEDbIdentifierWrapper>> identByValues) {
		assert (seDbInstance != null) : "removeKnownIdentifiers() seDbInstance is null";
		assert ((identByValues != null) && !identByValues.isEmpty()) : "removeKnownIdentifiers() invalid identByValues";

		final Set<String> distinctIndentValues = identByValues.keySet();

		final List<SEDbIdentifier> knownIdents = SEDbIdentifierRepository.findSEDbIdentBySEDbInstanceAndValues(seqEM, seDbInstance, distinctIndentValues);

		int nRemovedIdents = 0;

		if ((knownIdents != null) && !knownIdents.isEmpty()) {

			for (final SEDbIdentifier seDbIdent : knownIdents) {
				final String knownValue = seDbIdent.getValue();

				if (identByValues.remove(knownValue) != null) {
					++nRemovedIdents;
				}

			}

		}

		if ((nRemovedIdents > 0) && LOG.isDebugEnabled()) {
			final String sourcePath = seDbInstance.getSourcePath();// Should not be null
			LOG.debug("{} already known identifiers removed from search list for SEDbInstance [{}]", nRemovedIdents, sourcePath);
		}

	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static SEDbInstance loadOrCreateSEDbInstance(
		final EntityManager seqEM,
		final SEDbInstanceWrapper seDbInstanceW,
		final SEDb seDb,
		final String release,
		final Date lastModifiedTime) {
		assert (seDbInstanceW != null) : "loadOrCreateSEDbInstance() seDbInstanceW is null";
		assert (lastModifiedTime != null) : "loadOrCreateSEDbInstance() lastModifiedTime is null";

		String seDbRelease = null;

		final String seDbName = seDbInstanceW.getName();

		if (StringUtils.isEmpty(release)) {
			seDbRelease = DateUtils.formatReleaseDate(lastModifiedTime);
		} else {
			seDbRelease = release;
		}

		SEDbInstance result = SEDbRepository.findSEDbInstanceByNameAndRelease(seqEM, seDbName, seDbRelease);

		if (result == null) {
			result = new SEDbInstance();
			result.setRelease(seDbRelease);
			result.setSourcePath(seDbInstanceW.getSourcePath());
			result.setSourceLastModifiedTime(new Timestamp(lastModifiedTime.getTime()));

			SEDb newSEDb = null;

			if (seDb == null) {
				newSEDb = loadOrCreateSEDb(seqEM, seDbName);
			} else {
				newSEDb = seqEM.merge(seDb);
			}

			result.setSEDb(newSEDb);

			seqEM.persist(result);

			/* Check number of SEDbInstances and order */
			final String retrievedSEDbName = newSEDb.getName();// Should not be null

			final List<SEDbInstance> seDbInstances = SEDbRepository.findSEDbInstanceBySEDbName(seqEM,
				retrievedSEDbName);
			if (seDbInstances != null) {
				final int nInstances = seDbInstances.size();

				if (nInstances > 1) {
					LOG.info("There are {} SEDbInstances for SEDb [{}]", nInstances, retrievedSEDbName);
				}

			}

		}

		return result;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static SEDb loadOrCreateSEDb(
		final EntityManager seqEM,
		final String seDbName) {
		assert (!StringUtils.isEmpty(seDbName)) : "loadOrCreateSEDb() invalid seDbName";

		SEDb result = SEDbRepository.findSEDbByName(seqEM, seDbName);

		if (result == null) {
			result = new SEDb();
			result.setName(seDbName);
			result.setAlphabet(Alphabet.AA);// Default to Amino Acid sequence

			seqEM.persist(result);
		}

		return result;
	}


	private static Map<String, List<SEDbIdentifier>> loadExistingSEDbIdentifiers(
		final EntityManager seqEM,
		final String seDbName,
		final Map<SEDbIdentifierWrapper, String> foundSequences) {
		assert (foundSequences != null) : "loadExistingSEDbIdentifiers() foundSequences Map is null";

		final Map<String, List<SEDbIdentifier>> result = new HashMap<>();// Multimap

		final Set<String> valuesSet = new HashSet<>();

		final Set<SEDbIdentifierWrapper> identifiers = foundSequences.keySet();

		for (final SEDbIdentifierWrapper ident : identifiers) {
			valuesSet.add(ident.getValue());
		}

		if (!valuesSet.isEmpty()) {

			final List<SEDbIdentifier> foundIdentifiers = SEDbIdentifierRepository
					.findSEDbIdentBySEDbNameAndValues(seqEM, seDbName, valuesSet);
			if ((foundIdentifiers != null) && !foundIdentifiers.isEmpty()) {
				for (final SEDbIdentifier ident : foundIdentifiers) {
					final String key = ident.getValue();

					List<SEDbIdentifier> idents = result.get(key);

					if (idents == null) {
						idents = new ArrayList<>();

						result.put(key, idents);
					}

					idents.add(ident);
				}
			}

		}

		return result;
	}

	/* Distinct BioSequences by Hash */
	private static Map<String, BioSequence> loadExistingBioSequences(
		final EntityManager seqEM,
		final Map<SEDbIdentifierWrapper, String> foundSequences) {
		assert (foundSequences != null) : "loadExistingBioSequences() foundSequences Map is null";

		final Map<String, BioSequence> result = new HashMap<>();

		final Set<String> hashesSet = new HashSet<>();

		final Collection<String> sequences = foundSequences.values();

		for (final String sequence : sequences) {
			final String hash = HashUtil.calculateSHA256(sequence);
			hashesSet.add(hash);
		}

		if (!hashesSet.isEmpty()) {

			final List<BioSequence> foundBSs = BioSequenceRepository
					.findBioSequenceByHashes(seqEM, hashesSet);
			if ((foundBSs != null) && !foundBSs.isEmpty()) {
				for (final BioSequence bs : foundBSs) {
					final String hash = bs.getHash();
					result.put(hash, bs);
				}
			}

		}

		return result;
	}

	/* Distinct RepositoryIdentifiers (associated with the same Repository identified by its name) by value */
	private static Map<String, RepositoryIdentifier> loadExistingRepositoryIdentifiers(
		final EntityManager seqEM,
		final String repositoryName,
		final Map<SEDbIdentifierWrapper, String> foundSequences) {
		assert (foundSequences != null) : "loadExistingRepositoryIdentifiers() foundSequences Map is null";

		final Map<String, RepositoryIdentifier> result = new HashMap<>();

		final Set<String> valuesSet = new HashSet<>();

		final Set<SEDbIdentifierWrapper> identifiers = foundSequences.keySet();

		for (final SEDbIdentifierWrapper ident : identifiers) {

			final String repositoryIdentValue = ident.getRepositoryIdentifier();
			if (repositoryIdentValue != null) {// Can be null
				valuesSet.add(repositoryIdentValue);
			}

		}

		if (!valuesSet.isEmpty()) {

			final List<RepositoryIdentifier> foundIdentifiers = RepositoryIdentifierRepository
					.findRepositoryIdentByRepoNameAndValues(seqEM, repositoryName, valuesSet);
			if ((foundIdentifiers != null) && !foundIdentifiers.isEmpty()) {
				for (final RepositoryIdentifier ident : foundIdentifiers) {
					final String repositoryIdentValue = ident.getValue();
					result.put(repositoryIdentValue, ident);
				}
			}

		}

		return result;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static boolean handleSEDbIdentifier(
		final SeqContext context,
		final SEDbIdentifierWrapper seDbIdentW,
		final String sequence) {
		assert (context != null) : "handleSEDbIdentifier() context is null";
		assert (seDbIdentW != null) : "handleSEDbIdentifier() seDbIdentW is null";
		assert (sequence != null) : "handleSEDbIdentifier() sequence is null";

		boolean seDbIdentModified = false;

		final String identValue = seDbIdentW.getValue();

		final Map<String, List<SEDbIdentifier>> existingSEDbIdents = context.getExistingSEDbIdents();
		final List<SEDbIdentifier> existingIdents = existingSEDbIdents.get(identValue);

		if ((existingIdents == null) || existingIdents.isEmpty()) {
			persistNewSEDbIdentifier(context, seDbIdentW, sequence);
			seDbIdentModified = true;
		} else {
			final SEDbInstance seDbInstance = context.getSEDbInstance();

			boolean sameSequence = false;

			final SEDbInstanceComparator comparator = new SEDbInstanceComparator();

			for (final SEDbIdentifier existingIdent : existingIdents) {
				// Should not be null
				final BioSequence existingBs = existingIdent.getBioSequence();
				final String existingSequence = existingBs.getSequence();

				if (sequence.equals(existingSequence)) {

					if (sameSequence) {
						final SEDb seDb = seDbInstance.getSEDb();// Should not be null
						final String seDbName = seDb.getName();// Should not be null

						LOG.error(
							"There are multiple SEDbIdentifier [{}] with same BioSequence for SEDb [{}] Must clean SEQ Database",
							identValue, seDbName);
					} else {
						sameSequence = true;

						// Should not be null
						final SEDbInstance existingSEDbInstance = existingIdent.getSEDbInstance();

						if (comparator.compare(existingSEDbInstance, seDbInstance) < 0) {
							/* Update SEDbIdentifier to newest seDbInstance */
							existingIdent.setSEDbInstance(seDbInstance);
							seDbIdentModified = true;

							updateRepositoryIdentifier(context, existingIdent, seDbIdentW);
						}

					}

				} // End if (sequence == existingSequence)

			} // End loop for each existingIdent

			if (!sameSequence) {
				LOG.info("New Sequence for SEDbIdentifier [{}]", identValue);

				persistNewSEDbIdentifier(context, seDbIdentW, sequence);
				seDbIdentModified = true;
			}

		} // End if (existingIdents is not empty)

		return seDbIdentModified;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static SEDbIdentifier persistNewSEDbIdentifier(
		final SeqContext context,
		final SEDbIdentifierWrapper seDbIdentW,
		final String sequence) {
		assert (context != null) : "persistNewSEDbIdentifier() context is null";
		assert (seDbIdentW != null) : "persistNewSEDbIdentifier() seDbIdentW is null";
		assert (sequence != null) : "persistNewSEDbIdentifier() sequence is null";

		final SEDbIdentifier result = new SEDbIdentifier();
		result.setValue(seDbIdentW.getValue());
		result.setInferred(seDbIdentW.isInferred());

		final SEDbInstance seDbInstance = context.getSEDbInstance();
		result.setSEDbInstance(seDbInstance);

		final BioSequence bioSequence = loadOrCreateBioSequence(context, sequence);
		result.setBioSequence(bioSequence);

		final Repository repository = context.getRepository();
		final String repositoryIdentValue = seDbIdentW.getRepositoryIdentifier();

		if ((repository != null) && (repositoryIdentValue != null)) {
			final RepositoryIdentifier repositoryIdent = loadOrCreateRepositoryIdentifier(context, repositoryIdentValue);
			result.setRepositoryIdentifier(repositoryIdent);
		}

		context.getSeqEM().persist(result);

		return result;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static BioSequence loadOrCreateBioSequence(final SeqContext context, final String sequence) {
		assert (context != null) : "loadOrCreateBioSequence() context is null";
		assert (sequence != null) : "loadOrCreateBioSequence() sequence is null";

		final String hash = HashUtil.calculateSHA256(sequence);

		final Map<String, BioSequence> existingBioSequences = context.getExistingBioSequences();

		BioSequence result = existingBioSequences.get(hash);

		if (result == null) {
			result = new BioSequence();
			result.setSequence(sequence);
			result.setHash(hash);

			context.getSeqEM().persist(result);

			/* Cache created BioSequence */
			existingBioSequences.put(hash, result);
		}

		return result;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static RepositoryIdentifier loadOrCreateRepositoryIdentifier(
		final SeqContext context,
		final String value) {
		assert (context != null) : "loadOrCreateRepositoryIdentifier() context is null";
		assert (value != null) : "loadOrCreateRepositoryIdentifier() value is null";

		final Map<String, RepositoryIdentifier> existingRepositoryIdents = context
				.getExistingRepositoryIdents();

		if (existingRepositoryIdents == null) {
			throw new IllegalArgumentException("SeqContext.existingRepositoryIdents Map is null");
		}

		RepositoryIdentifier result = existingRepositoryIdents.get(value);

		if (result == null) {
			result = new RepositoryIdentifier();
			result.setValue(value);

			final Repository repository = context.getRepository();

			if (repository == null) {
				throw new IllegalArgumentException("SeqContext.repository is null");
			}

			result.setRepository(repository);

			context.getSeqEM().persist(result);

			/* Cache created RepositoryIdentifier */
			existingRepositoryIdents.put(value, result);
		}

		return result;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static void updateRepositoryIdentifier(
		final SeqContext context,
		final SEDbIdentifier seDbIdentifier,
		final SEDbIdentifierWrapper seDbIdentW) {
		assert (context != null) : "updateRepositoryIdentifier() context is null";
		assert (seDbIdentifier != null) : "updateRepositoryIdentifier() seDbIdentifier is null";
		assert (seDbIdentW != null) : "updateRepositoryIdentifier() seDbIdentW is null";

		final String repositoryIdentValue = seDbIdentW.getRepositoryIdentifier();

		if (repositoryIdentValue == null) {
			seDbIdentifier.setRepositoryIdentifier(null);
		} else {
			boolean same = false;

			final RepositoryIdentifier oldRepositoryIdent = seDbIdentifier.getRepositoryIdentifier();
			if (oldRepositoryIdent != null) {
				final String oldRepositoryIdentValue = oldRepositoryIdent.getValue();// Should not be null
				same = repositoryIdentValue.equals(oldRepositoryIdentValue);
			}

			if (!same) {
				final Repository repository = context.getRepository();

				if (repository == null) {
					/* Relation SEDb -> Repository removed */
					seDbIdentifier.setRepositoryIdentifier(null);
				} else {
					final String seDbIdentValue = seDbIdentifier.getValue();
					LOG.info("New RepositoryIdentifier [{}] for SEDbIdentifier [{}]", repositoryIdentValue,
						seDbIdentValue);

					/* New RepositoryIdentifier for this SEDbIdentifier */
					final RepositoryIdentifier newRepositoryIdent = loadOrCreateRepositoryIdentifier(context,
						repositoryIdentValue);
					seDbIdentifier.setRepositoryIdentifier(newRepositoryIdent);
				}

			} // End if (oldRepositoryIdent differ from repositoryIdent Value)

		}

	}

}
