package fr.proline.module.seq.service;

import fr.profi.util.DateUtils;
import fr.profi.util.FileUtils;
import fr.profi.util.StringUtils;
import fr.profi.util.ThreadLogger;
import fr.proline.module.seq.Constants;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.config.ParsingRuleEntry;
import fr.proline.module.seq.config.SeqRepoConfig;
import fr.proline.module.seq.dto.DDatabankInstance;
import fr.proline.module.seq.dto.DDatabankProtein;
import fr.proline.module.seq.orm.*;
import fr.proline.module.seq.orm.dao.BioSequenceDao;
import fr.proline.module.seq.orm.dao.DatabankDao;
import fr.proline.module.seq.orm.dao.DatabankProteinDao;
import fr.proline.module.seq.orm.dao.RepositoryProteinDao;
import fr.proline.module.seq.util.Counters;
import fr.proline.module.seq.util.DatabankInstanceComparator;
import fr.proline.module.seq.util.HashUtil;
import fr.proline.module.seq.util.RegExUtil;
import fr.proline.repository.IDatabaseConnector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.persistence.EntityManager;
import javax.persistence.EntityTransaction;
import java.io.File;
import java.sql.Timestamp;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Pattern;

import static fr.proline.module.seq.Constants.PERSISTENCE;

public final class BioSequenceRetriever {

	/**
	 * Protect Write Transaction on SEQ Databank.
	 */
	public static final Object SEQ_DB_WRITE_LOCK = new Object();
	private static final Logger LOG = LoggerFactory.getLogger(BioSequenceRetriever.class);

	/**
	 * Protect re-entrance of public retrieveBioSequences() method (Only one thread at a time).
	 */
	private static final Object RUNNING_LOCK = new Object();

	private static final ExecutorService EXECUTOR = Executors.newFixedThreadPool(Constants.calculateNThreads());
	private static final DataSourceBuilder DATA_SOURCE_BUILDER = new DataSourceBuilder();
	private static final DatabankInstanceComparator DATABANK_INSTANCE_COMPARATOR = new DatabankInstanceComparator();

	/* Private constructor (Utility class) */
	private BioSequenceRetriever() {
	}

	/**
	 * Blocks until all given DatabankInstance are searched.
	 * 
	 * @param proteinsByDatabank
	 * @return Number of handled (created or updated) SEDbIdentifiers.
	 * @throws ExecutionException
	 * @throws InterruptedException
	 */
	public static int retrieveBioSequences(final Map<DDatabankInstance, Set<DDatabankProtein>> proteinsByDatabank) throws Exception {

		assert ((proteinsByDatabank != null) && !proteinsByDatabank.isEmpty()) : "proteinsByDatabank must not be null";

		LOG.info("Start RetrieveBioSequences from {} Databank Instance(s) ", proteinsByDatabank.size());

		int totalPersistedProteinsCount = 0;

		synchronized (RUNNING_LOCK) {// Only one Thread at a time
			final long start = System.currentTimeMillis();

			SeqRepoConfig.forcePropertiesReload();//Read back properties to take into account potential changes.
			DATA_SOURCE_BUILDER.forceRescanFastaFiles();

			final List<Future<Integer>> futures = new ArrayList<>();

			final Set<Map.Entry<DDatabankInstance, Set<DDatabankProtein>>> entries = proteinsByDatabank.entrySet();

			for (final Map.Entry<DDatabankInstance, Set<DDatabankProtein>> entry : entries) {

				final Set<DDatabankProtein> proteins = entry.getValue();
				if ((proteins != null) && !proteins.isEmpty()) {

					final DDatabankInstance databank  = entry.getKey();
					final Callable<Integer> task = new Callable<Integer>() {

						public Integer call() throws Exception {
							final Thread currentThread = Thread.currentThread();

							if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
								currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
							}
							return Integer.valueOf(retrieveBioSequences(databank, proteins));
						}

					};

					final Future<Integer> future = EXECUTOR.submit(task);
					futures.add(future);
				}

			} // End loop for each proteinsByDatabank entries

			/* Wait (blocking) for all futures to complete */
			for (final Future<Integer> f : futures) {
				final Integer result = f.get();
				if (result != null) {
					final int nHandledSEDbIdents = result.intValue();

					if (nHandledSEDbIdents > 0) {
						totalPersistedProteinsCount += nHandledSEDbIdents;
					}

				}
			}

			final long end = System.currentTimeMillis();

			final long duration = end - start;

			LOG.info("Total retrieveBioSequences() execution : {} Protein Identifiers retrieved from sources in {} ms", totalPersistedProteinsCount, duration);
		} // End of synchronized block on RUNNING_LOCK

		return totalPersistedProteinsCount;
	}

	public static boolean waitExecutorShutdown() throws Exception {
		boolean result = false;
		EXECUTOR.shutdown();
		result = EXECUTOR.awaitTermination(Integer.MAX_VALUE, TimeUnit.SECONDS);
		return result;
	}

	private static int retrieveBioSequences(final DDatabankInstance databank, final Set<DDatabankProtein> proteins) throws Exception {

		assert (databank != null) : "retrieveBioSequences() databank is null";

		LOG.info("Start RetrieveBioSequences of {} proteins in file {}", proteins.size(), databank.getSourcePath());
		int persistedProteinsCount = 0;
		EntityManager seqEM = null;

		try {
			final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(true);
			seqEM = seqDb.createEntityManager();
			Counters counters = retrieveBioSequences(seqEM, databank, proteins, true);
			persistedProteinsCount = counters.sum("persisted");
			counters.report(LOG);
		} finally {

			if (seqEM != null) {
				try {
					seqEM.close();
				} catch (Exception exClose) {
					LOG.error("Error closing SEQ Db EntityManager", exClose);
				}
			}
		}

		return persistedProteinsCount;
	}

	private static Counters retrieveBioSequences(
		final EntityManager seqEM,
		final DDatabankInstance dDatabankInstance,
		final Set<DDatabankProtein> proteins,
		final boolean doApproximate) throws Exception {

		assert (dDatabankInstance != null) : "retrieveBioSequences() databankInstance is null";

		Counters counters = new Counters(dDatabankInstance.getName());

		final Map<String, List<DDatabankProtein>> proteinsByIdentifier = buildProteinsByIdentifierMap(proteins);

		final String databankInstanceName = dDatabankInstance.getName();
		final String sourcePath = dDatabankInstance.getSourcePath();
		final String sourceFileName = FileUtils.extractFileName(sourcePath);

		String releaseRegex = null;
		String proteinIdentifierRegex = null;
		String release = null;

		ParsingRuleEntry parsingRule = ParsingRuleEntry.getParsingRuleEntry(sourceFileName);
		if(parsingRule != null){
			releaseRegex = parsingRule.getFastaReleaseRegEx();
			proteinIdentifierRegex = parsingRule.getProteinAccRegEx();
			release = RegExUtil.parseReleaseVersion(sourceFileName, releaseRegex);
		}

		DatabankInstance databankInstance = searchDatabankInstance(seqEM, dDatabankInstance, release);
		release = getReleaseInformation(release, databankInstance);
		dDatabankInstance.setRelease(release);

		if (databankInstance != null) {
			removeAlreadyPersistedIdentifiers(seqEM, databankInstance, proteinsByIdentifier);
		}


		if (!proteinsByIdentifier.isEmpty()) {

			Pattern proteinIdentifierPattern = null;
			if (proteinIdentifierRegex == null) {
				String defaultRegEx = SeqRepoConfig.getInstance().getDefaultProtAccRegEx();
				LOG.debug("Sequence source [{}] will be parsed with Default Protein Accession Regex {}", sourcePath, defaultRegEx);
				proteinIdentifierPattern = Pattern.compile(defaultRegEx, Pattern.CASE_INSENSITIVE);
			} else {
				LOG.debug("Sequence source [{}] will be parsed using Protein Accession Regex {} ", sourceFileName, proteinIdentifierRegex);
				proteinIdentifierPattern = Pattern.compile(proteinIdentifierRegex, Pattern.CASE_INSENSITIVE);
			}

			final DataSource fastaSource = DATA_SOURCE_BUILDER.buildFastaSource(sourceFileName, proteinIdentifierPattern, null);

			if (fastaSource == null) {
				LOG.warn("No Fasta file found matching source path [{}]", sourcePath);
				if (doApproximate) {
					LOG.info("Trying to find the closest matching filename to {}", sourceFileName);
					final File bestFastaFile = selectBestMatchingFastaFile(sourceFileName, release, releaseRegex);
					if (bestFastaFile != null) {
						LOG.info("Trying to load [{}] sequences from [{}]", sourcePath, bestFastaFile.getAbsolutePath());
						return retrieveBioSequences(seqEM, new DDatabankInstance(databankInstanceName, null, bestFastaFile.getName()), proteins, false);
					} else {
						LOG.warn("No filename nearly matching to source path [{}] found", sourcePath);
					}
				}
			} else {

				LOG.info("Searching {} BioSequences from sourcePath [{}]", proteinsByIdentifier.size(), sourcePath);

				final Map<DDatabankProtein, String> foundSequences = fastaSource.retrieveSequences(proteinsByIdentifier);
				if ((foundSequences != null) && !foundSequences.isEmpty()) {

					LOG.info("{} BioSequences have been extracted from sourcePath [{}]", foundSequences.size(), sourcePath);

					/* Big write lock on SEQ Db */
					synchronized (SEQ_DB_WRITE_LOCK) {// Only one Thread at a time
						EntityTransaction seqTransac = seqEM.getTransaction();
						boolean transacOK = false;

						try {
							seqTransac.begin();
							transacOK = false;

							LOG.debug("SEQ Db WRITE Transaction begin");
							final long start = System.currentTimeMillis();

							if (databankInstance == null) {
								databankInstance = findOrCreateDatabankInstance(seqEM, dDatabankInstance, null, release, fastaSource.getLastModifiedTime());
							} else {
								databankInstance = seqEM.merge(databankInstance);
							}

							final Map<String, List<DatabankProtein>> existingProteins = searchExistingProteins(seqEM, databankInstanceName, foundSequences.keySet());
							LOG.debug("{} Proteins already exists in the databank {}", existingProteins.size(), databankInstance);

							final Map<String, BioSequence> existingBioSequences = findExistingBioSequences(seqEM, foundSequences.values());
							LOG.debug("{} BioSequence already exists in the SeqDB (compared by hash code)", existingBioSequences.size());

							final Repository repository = databankInstance.getDatabank().getRepository();
							Map<String, RepositoryProtein> existingRepositoryIdents = null;

							if (repository != null) {// Can be null
								final String repositoryName = repository.getName();// Should not be null
								existingRepositoryIdents = loadExistingRepositoryIdentifiers(seqEM, repositoryName, foundSequences);
								LOG.debug("Possible existing RepositoryIdentifiers : {}", existingRepositoryIdents.size());
							}

							final RetrieverContext context = new RetrieverContext(
											seqEM,
											databankInstance,
											existingProteins,
											existingBioSequences,
											repository,
											existingRepositoryIdents,
											counters);


							for (final Map.Entry<DDatabankProtein, String> entry : foundSequences.entrySet()) {
								final DDatabankProtein protein = entry.getKey();
								final String sequence = entry.getValue();
								persistProteinIfNeeded(context, protein, sequence);
							}

							seqTransac.commit();
							transacOK = true;

							long duration = System.currentTimeMillis() - start;
							LOG.info("SeqDb WRITE Transaction committed : {} proteins persisted from [{}] in {} ms", counters.sum("persisted"), sourcePath, duration);

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

		return counters;
	}

	private static String getReleaseInformation(
					final String release,
					final DatabankInstance databankInstance) {

		if (databankInstance != null) {
			if (release == null) {
				return databankInstance.getRelease();
			} else {
				final String instanceRelease = databankInstance.getRelease();// Should not be null
				if (!release.equals(instanceRelease)) {
					throw new RuntimeException("Inconsistent Release version");
				}
			}
		}
		return release;
	}



	/**
	 * Try to select among existing Fasta file one approximately matching the fastaFilename
	 * release.
	 *
	 * @throws Exception
	 * 
	 */
	private static File selectBestMatchingFastaFile(
		final String sourceFileName,
		final String release,
		final String parsingRuleReleaseRegex) throws Exception {

		assert (sourceFileName != null) : "selectBestMatchingFastaFile() sourceFileName is null";

		File result = null;

		if (!StringUtils.isEmpty(release)) {

			final int releaseIndex = sourceFileName.indexOf(release);
			if (releaseIndex != -1) {

				final String namePart = sourceFileName.substring(0, releaseIndex);
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
			} // End if (sourceFileName contains release)
		} // End if (release string is not empty)

		return result;
	}

	private static Map<String, List<DDatabankProtein>> buildProteinsByIdentifierMap(final Set<DDatabankProtein> proteins) {

		assert (proteins != null) : "buildProteinsByIdentifierMap() proteins Set is null";
		final Map<String, List<DDatabankProtein>> result = new HashMap<>();

		for (final DDatabankProtein sdi : proteins) {
			final String identValue = sdi.getIdentifier();
			List<DDatabankProtein> identifiers = result.get(identValue);
			if (identifiers == null) {
				identifiers = new ArrayList<>(1);// Assume one SEDbIdentWrapper by identValue
				result.put(identValue, identifiers);
			}
			identifiers.add(sdi);
		} // End loop for each proteins

		return result;
	}

	/*
	 * Search for the DataBank instance in the SeqDB. The instance is search by matching name and sourcepath from the
	 * dDatabankInstance object or by matching name and release.
	 */
	private static DatabankInstance searchDatabankInstance(
		final EntityManager seqEM,
		final DDatabankInstance dDatabankInstance,
		final String release) {

		assert (dDatabankInstance != null) : "searchDatabankInstance() dDatabankInstance is null";

		final String seDbName = dDatabankInstance.getName();
		final String sourcePath = dDatabankInstance.getSourcePath();

		DatabankInstance result = null;

		//
		// First try to load the databank instance from SeqDB by Name and SourcePath
		//
		final List<DatabankInstance> foundSEDbInstances = DatabankDao.findSEDbInstanceByNameAndSourcePath(seqEM, seDbName, sourcePath);
		if (foundSEDbInstances != null) {
			final int nInstances = foundSEDbInstances.size();
			if (nInstances == 1) {
				result = foundSEDbInstances.get(0);
				LOG.info("DatabankInstance matching name:{} and sourcePath:{} found", seDbName, sourcePath);
			} else if (nInstances > 1) {
				LOG.warn("There are {} DatabankInstances in SeqDB matching name:{} and sourcePath:{}", nInstances, seDbName, sourcePath);
			}
		}

		if (result == null) {
			//
			// If the DatabankInstance cannot be found in the SeqDB, then try find DatabankInstance by name and release
			//
			LOG.warn("DatabankInstance (name, sourcePath) not found or ambiguous in SeqDB, trying to search by release extracted from the fasta filename");
				if (release != null && !StringUtils.isEmpty(release)) {
					LOG.warn("Search DatabankInstance in SeqDB, from the name:{} and release:{}", seDbName, release);
					result = DatabankDao.findSEDbInstanceByNameAndRelease(seqEM, seDbName, release);
					if (result == null) {
						LOG.warn("DatabankInstance (name, release) not found in SeqDB");
					}
				}  else {
					LOG.warn("No Release information supplied, DatabankInstance cannot be found in the SeqDB");
				}
		}

		return result;
	}

	/**
	 * Removes Identifiers already known in the SeqDB from the supplied Map.
	 * 
	 * @param seqEM
	 * @param databankInstance
	 * @param proteinsByIdentifier
	 */
	private static void removeAlreadyPersistedIdentifiers(
		final EntityManager seqEM,
		final DatabankInstance databankInstance,
		final Map<String, List<DDatabankProtein>> proteinsByIdentifier) {

		assert (databankInstance != null) : "removeAlreadyPersistedIdentifiers() databankInstance is null";
		assert ((proteinsByIdentifier != null) && !proteinsByIdentifier.isEmpty()) : "removeAlreadyPersistedIdentifiers() invalid proteinsByIdentifier";

		final Set<String> distinctIdentifiers = proteinsByIdentifier.keySet();
		final List<DatabankProtein> proteinsInDatabank = DatabankProteinDao.findProteinsInDatabank(seqEM, databankInstance, distinctIdentifiers);

		int removedIdentifiersCount = 0;

		if ((proteinsInDatabank != null) && !proteinsInDatabank.isEmpty()) {

			for (final DatabankProtein protein : proteinsInDatabank) {
				final String identifier = protein.getIdentifier();
				if (proteinsByIdentifier.remove(identifier) != null) {
					++removedIdentifiersCount;
				}

			}

		}

		if ((removedIdentifiersCount > 0) && LOG.isDebugEnabled()) {
			LOG.info("{} already known identifiers removed from search list for DatabankInstance {}", removedIdentifiersCount, databankInstance);
		}

	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static DatabankInstance findOrCreateDatabankInstance(
		final EntityManager seqEM,
		final DDatabankInstance dDatabankInstance,
		final Databank databank,
		final String release,
		final Date lastModifiedTime) {

		assert (dDatabankInstance != null) : "findOrCreateDatabankInstance() dDatabankInstance is null";
		assert (lastModifiedTime != null) : "findOrCreateDatabankInstance() lastModifiedTime is null";

		String seDbRelease = null;

		final String seDbName = dDatabankInstance.getName();

		if (StringUtils.isEmpty(release)) {
			seDbRelease = DateUtils.formatReleaseDate(lastModifiedTime);
		} else {
			seDbRelease = release;
		}

		DatabankInstance databankInstance = DatabankDao.findSEDbInstanceByNameAndRelease(seqEM, seDbName, seDbRelease);

		if (databankInstance == null) {
			databankInstance = new DatabankInstance();
			databankInstance.setRelease(seDbRelease);
			databankInstance.setSourcePath(dDatabankInstance.getSourcePath());
			databankInstance.setSourceLastModifiedTime(new Timestamp(lastModifiedTime.getTime()));

			Databank jpaDatabank = null;

			if (databank == null) {
				jpaDatabank = findOrCreateDatabank(seqEM, seDbName);
			} else {
				jpaDatabank = seqEM.merge(databank);
			}

			databankInstance.setDatabank(jpaDatabank);
			persist(seqEM, databankInstance);

		}

		return databankInstance;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static Databank findOrCreateDatabank(
		final EntityManager seqEM,
		final String databankName) {
		assert (!StringUtils.isEmpty(databankName)) : "findOrCreateDatabank() invalid databankName";

		Databank databank = DatabankDao.findSEDbByName(seqEM, databankName);

		if (databank == null) {
			databank = new Databank();
			databank.setName(databankName);
			databank.setAlphabet(Alphabet.AA);// Default to Amino Acid sequence
			persist(seqEM, databank);
		}

		return databank;
	}

	/**
	 * Search in databankName proteins matching identifiers extracted from the specified proteins parameter.
	 * The search do not take into account the release or sourcePath of the databank. As a result, a single
	 * identifier can be found several times by this search.
	 *
	 * @param seqEM
	 * @param databankName
	 * @param proteins
	 * @return
	 */
	private static Map<String, List<DatabankProtein>> searchExistingProteins(
		final EntityManager seqEM,
		final String databankName,
		final Set<DDatabankProtein> proteins) {

		assert (proteins != null) : "searchExistingProteins() proteins Map is null";

		final Map<String, List<DatabankProtein>> result = new HashMap<>();
		final Set<String> identifiers = new HashSet<>();

		for (final DDatabankProtein protein : proteins) {
			identifiers.add(protein.getIdentifier());
		}

		if (!identifiers.isEmpty()) {
			final List<DatabankProtein> foundProteins = DatabankProteinDao.findProteinsInDatabankName(seqEM, databankName, identifiers);
			if ((foundProteins != null) && !foundProteins.isEmpty()) {

				for (DatabankProtein protein : foundProteins) {
					final String identifier = protein.getIdentifier();
					List<DatabankProtein> proteinList = result.get(identifier);

					if (proteinList == null) {
						proteinList = new ArrayList<>();
						result.put(identifier, proteinList);
					}
					proteinList.add(protein);
				}
			}
		}

		return result;
	}

	/* Distinct BioSequences by Hash */
	private static Map<String, BioSequence> findExistingBioSequences(
		final EntityManager seqEM,
		final Collection<String> sequences) {

		assert (sequences != null) : "findExistingBioSequences() sequences collection is null";

		final Map<String, BioSequence> result = new HashMap<>();
		final Set<String> hashesSet = new HashSet<>();

		for (final String sequence : sequences) {
			final String hash = HashUtil.calculateSHA256(sequence);
			hashesSet.add(hash);
		}

		if (!hashesSet.isEmpty()) {
			final List<BioSequence> foundBSs = BioSequenceDao.findBioSequenceByHashes(seqEM, hashesSet);
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
	private static Map<String, RepositoryProtein> loadExistingRepositoryIdentifiers(
		final EntityManager seqEM,
		final String repositoryName,
		final Map<DDatabankProtein, String> foundSequences) {
		assert (foundSequences != null) : "loadExistingRepositoryIdentifiers() foundSequences Map is null";

		final Map<String, RepositoryProtein> result = new HashMap<>();

		final Set<String> valuesSet = new HashSet<>();

		final Set<DDatabankProtein> identifiers = foundSequences.keySet();

		for (final DDatabankProtein ident : identifiers) {

			final String repositoryIdentValue = ident.getRepositoryIdentifier();
			if (repositoryIdentValue != null) {// Can be null
				valuesSet.add(repositoryIdentValue);
			}

		}

		if (!valuesSet.isEmpty()) {

			final List<RepositoryProtein> foundIdentifiers = RepositoryProteinDao.findRepositoryIdentByRepoNameAndValues(seqEM, repositoryName, valuesSet);
			if ((foundIdentifiers != null) && !foundIdentifiers.isEmpty()) {
				for (final RepositoryProtein ident : foundIdentifiers) {
					final String repositoryIdentValue = ident.getValue();
					result.put(repositoryIdentValue, ident);
				}
			}

		}

		return result;
	}

	/**
	 * Persist if needed the specified (protein,sequence) tuple. Returns true if the protein is persisted
	 * or if an existing object mach the (protein, sequence) tuple.
	 *
	 * Must be called holding SEQ_DB_WRITE_LOCK
	 *
	 * @param context
	 * @param protein
	 * @param sequence
	 * @return
	 */
		private static void persistProteinIfNeeded(final RetrieverContext context, final DDatabankProtein protein, final String sequence) {

		assert (context != null) : "persistProteinIfNeeded() context is null";
		assert (protein != null) : "persistProteinIfNeeded() protein is null";
		assert (sequence != null) : "persistProteinIfNeeded() sequence is null";


		final String proteinIdentifier = protein.getIdentifier();
		final List<DatabankProtein> matchingProteins = context.getExistingProteins().get(proteinIdentifier);

		if ((matchingProteins == null) || matchingProteins.isEmpty()) {
			LOG.info("Persist new Protein Identifier [{}] in Databank {}", proteinIdentifier, context.getDatabankInstance());
			persistDatabankProtein(context, protein, sequence);
			context.getCounters().inc("New Proteins persisted");
		} else {
			final DatabankInstance databankInstance = context.getDatabankInstance();
			boolean sequenceMatched = false;

			for (final DatabankProtein matchingProtein : matchingProteins) {

				final String matchingSequence = matchingProtein.getBioSequence().getSequence();

				if (sequence.equals(matchingSequence)) {
					if (sequenceMatched) {
						final String databankName = databankInstance.getDatabank().getName();
						LOG.error( "There are several proteins named [{}] with the same BioSequence for Databank [{}], this should not happen", proteinIdentifier, databankName);
					} else {
						sequenceMatched = true;
						final DatabankInstance matchedDatabankInstance = matchingProtein.getDatabankInstance();

						if (DATABANK_INSTANCE_COMPARATOR.compare(matchedDatabankInstance, databankInstance) < 0) {
							// Update DatabankProtein to newest databankInstance
							// TODO: CBy : je ne comprends pas a quel endroit la matchingProtein est persistée ?
							matchingProtein.setDatabankInstance(databankInstance);
							context.getCounters().inc("Already persisted Proteins was updated");
							updateRepositoryIdentifier(context, matchingProtein, protein);
						} else {
							context.getCounters().inc("Already persisted Proteins (but in a newer databank)");
						}
					}
				}
			}

			if (!sequenceMatched) {
				LOG.info("Persist new Protein [{}] because its Sequence is new", proteinIdentifier);
				persistDatabankProtein(context, protein, sequence);
				context.getCounters().inc("New persisted Proteins (because of a new Sequence)");
			}
		} // End if (matchingProteins is not empty)
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static DatabankProtein persistDatabankProtein(
		final RetrieverContext context,
		final DDatabankProtein dProtein,
		final String sequence) {
		assert (context != null) : "persistDatabankProtein() context is null";
		assert (dProtein != null) : "persistDatabankProtein() dProtein is null";
		assert (sequence != null) : "persistDatabankProtein() sequence is null";

		final DatabankProtein protein = new DatabankProtein();
		protein.setIdentifier(dProtein.getIdentifier());
		protein.setInferred(dProtein.isInferred());
		protein.setDescription(dProtein.getDescription());
		final DatabankInstance seDbInstance = context.getDatabankInstance();
		protein.setDatabankInstance(seDbInstance);

		final BioSequence bioSequence = getOrCreateBioSequence(context, sequence);
		protein.setBioSequence(bioSequence);

		final Repository repository = context.getRepository();
		final String repositoryIdentValue = dProtein.getRepositoryIdentifier();

		if ((repository != null) && (repositoryIdentValue != null)) {
			final RepositoryProtein repositoryIdent = getOrCreateRepositoryIdentifier(context, repositoryIdentValue);
			protein.setRepositoryIdentifier(repositoryIdent);
		}

		persist(context.getSeqEM(), protein);

		return protein;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static BioSequence getOrCreateBioSequence(final RetrieverContext context, final String sequence) {

		assert (context != null) : "getOrCreateBioSequence() context is null";
		assert (sequence != null) : "getOrCreateBioSequence() sequence is null";

		final String hash = HashUtil.calculateSHA256(sequence);
		final Map<String, BioSequence> existingBioSequences = context.getExistingBioSequences();

		BioSequence bioSequence = existingBioSequences.get(hash);

		if (bioSequence == null) {
			bioSequence = new BioSequence();
			bioSequence.setSequence(sequence);
			bioSequence.setHash(hash);

			persist(context.getSeqEM(), bioSequence);

			/* Cache created BioSequence */
			existingBioSequences.put(hash, bioSequence);
		}

		return bioSequence;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static RepositoryProtein getOrCreateRepositoryIdentifier(
		final RetrieverContext context,
		final String value) {

		assert (context != null) : "getOrCreateRepositoryIdentifier() context is null";
		assert (value != null) : "getOrCreateRepositoryIdentifier() value is null";

		final Map<String, RepositoryProtein> existingRepositoryIdents = context.getExistingRepositoryIdents();

		if (existingRepositoryIdents == null) {
			throw new IllegalArgumentException("RetrieverContext.existingRepositoryIdents Map is null");
		}

		RepositoryProtein repositoryProtein = existingRepositoryIdents.get(value);

		if (repositoryProtein == null) {
			repositoryProtein = new RepositoryProtein();
			repositoryProtein.setValue(value);

			final Repository repository = context.getRepository();

			if (repository == null) {
				throw new IllegalArgumentException("RetrieverContext.repository is null");
			}

			repositoryProtein.setRepository(repository);
			persist(context.getSeqEM(), repositoryProtein);

			/* Cache created RepositoryProtein */
			existingRepositoryIdents.put(value, repositoryProtein);
		}

		return repositoryProtein;
	}

	/* Must be called holding SEQ_DB_WRITE_LOCK */
	private static void updateRepositoryIdentifier(
		final RetrieverContext context,
		final DatabankProtein protein,
		final DDatabankProtein dProtein) {
		assert (context != null) : "updateRepositoryIdentifier() context is null";
		assert (protein != null) : "updateRepositoryIdentifier() protein is null";
		assert (dProtein != null) : "updateRepositoryIdentifier() dProtein is null";

		final String repositoryIdentValue = dProtein.getRepositoryIdentifier();

		if (repositoryIdentValue == null) {
			protein.setRepositoryIdentifier(null);
		} else {
			boolean same = false;

			final RepositoryProtein oldRepositoryIdent = protein.getRepositoryIdentifier();
			if (oldRepositoryIdent != null) {
				final String oldRepositoryIdentValue = oldRepositoryIdent.getValue();// Should not be null
				same = repositoryIdentValue.equals(oldRepositoryIdentValue);
			}

			if (!same) {
				final Repository repository = context.getRepository();

				if (repository == null) {
					/* Relation Databank -> Repository removed */
					protein.setRepositoryIdentifier(null);
				} else {
					final String seDbIdentValue = protein.getIdentifier();
					LOG.info("New RepositoryProtein [{}] for DatabankProtein [{}]", repositoryIdentValue, seDbIdentValue);

					/* New RepositoryProtein for this DatabankProtein */
					final RepositoryProtein newRepositoryIdent = getOrCreateRepositoryIdentifier(context, repositoryIdentValue);
					protein.setRepositoryIdentifier(newRepositoryIdent);
				}
			} // End if (oldRepositoryIdent differ from repositoryIdent Value)
		}
	}

	private static void persist(EntityManager em, Object o) {
		if (PERSISTENCE) {
			em.persist(o);
		}
	}
}
