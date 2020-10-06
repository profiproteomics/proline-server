package fr.proline.module.seq.service;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import fr.profi.chemistry.algo.DigestionUtils;
import fr.profi.chemistry.model.EnzymeCleavage;
import fr.profi.util.StringUtils;
import fr.proline.core.orm.msi.*;
import fr.proline.core.orm.msi.repository.ResultSetRepository;
import fr.proline.core.orm.uds.Project;
import fr.proline.core.orm.uds.repository.ProjectRepository;
import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.DBioSequence;
import fr.proline.module.seq.dto.DDatabankInstance;
import fr.proline.module.seq.dto.DDatabankProtein;
import fr.proline.module.seq.util.HashUtil;
import fr.proline.module.seq.util.RegExUtil;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Some;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.TypedQuery;
import java.util.*;
import java.util.Map.Entry;

public class ProjectHandler {

	private static final Logger LOG = LoggerFactory.getLogger(ProjectHandler.class);

	private static final String ALL_SEQ_DB_QUERY = "FROM fr.proline.core.orm.msi.SeqDatabase";

	private static final String VALIDATED_PM_COUNT_FOR_RSMS_QUERY = "SELECT COUNT (DISTINCT pm.accession)"
		+ " FROM fr.proline.core.orm.msi.ProteinMatch pm JOIN pm.proteinSetProteinMatchItems ps"
		+ " WHERE ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER') OR (upper(pm.resultSet.type) = 'QUANTITATION'))"
		+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary.id IN (:rsm_ids) )";

	private static final String VALIDATED_PM_SDM_FOR_RSMS_QUERY = "SELECT DISTINCT pm.accession, pm.description, sdb.id"
		+ " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SeqDatabase sdb, fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap pmsdb"
		+ " JOIN pm.proteinSetProteinMatchItems ps"
		+ " WHERE (pmsdb.id.proteinMatchId = pm.id) AND (pmsdb.id.seqDatabaseId = sdb.id)"
		+ " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER') OR (upper(pm.resultSet.type) = 'QUANTITATION'))"
		+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary.id IN  (:rsm_ids))";

	private static final String VALIDATED_PM_FOR_RSMS_QUERY = "SELECT DISTINCT pm.accession, pm.description, ssdm.seqDatabase.id"
		+ " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SearchSettingsSeqDatabaseMap ssdm"
		+ " JOIN pm.proteinSetProteinMatchItems ps"
		+ " WHERE (pm.resultSet.msiSearch.searchSetting = ssdm.searchSetting)"
		+ " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER') OR (upper(pm.resultSet.type) = 'QUANTITATION'))"
		+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary.id IN  (:rsm_ids))";

	private static final String LIST_RSM_IN_DATASET_ID_QUERY = "SELECT DISTINCT(dt.resultSummaryId) FROM Dataset dt WHERE dt.project.id= :projectId AND dt.type IN ('AGGREGATE','IDENTIFICATION') AND dt.resultSummaryId IS NOT NULL";

	private static final String LIST_QUANT_RSM_ID_QUERY = "SELECT DISTINCT(mqc.quantResultSummaryId) FROM MasterQuantitationChannel mqc, Dataset dt WHERE dt.project.id= :projectId AND mqc.quantDataset.id = dt.id and dt.type IN ('QUANTITATION') AND mqc.quantResultSummaryId IS NOT NULL";

	private static final String LIST_PS_FOR_RSM_QUERY = "SELECT ps FROM ProteinSet ps WHERE ps.resultSummary.id= :rsmId AND ps.isValidated = 'true'";

	private static final int EXPECTED_LINE_LENGTH = 3;

	private static final String GET_PEPID_BY_PSID_PM_QUERY = "SELECT pi.peptide.id, ps.id "
		+ "FROM fr.proline.core.orm.msi.PeptideInstance pi, fr.proline.core.orm.msi.PeptideSetPeptideInstanceItem  pspi, "
		+ " fr.proline.core.orm.msi.ProteinSet ps "
		+ " WHERE  pspi.resultSummary.id = :rsmId "
		+ " AND pspi.peptideInstance= pi "
		+ " AND ps.resultSummary.id = :rsmId AND ps.isValidated=true AND pspi.peptideSet.proteinSet = ps";
	
	private static final String GET_SEQ_MATCH_INFO_FOR_RS_QUERY = "SELECT sm.id.start, sm.id.stop, sm.id.proteinMatchId, sm.id.peptideId "
		+ "FROM SequenceMatch sm "
		+ "WHERE sm.resultSetId = :rsId";

	private final Long projectId;
	private EntityManager msiEM;
	private EntityManager udsEM;

	private int nbrNoSeqProt;
	private int nbrManySeqProt;
	private int nbrBioSeqMissMatch;

	public ProjectHandler(Long projectId) {
		this.projectId = projectId;
		open();
	}

	public Long getProjectId() {
		return projectId;
	}

	public void open() {
		IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);

		if (msiDbConnector == null) {
			LOG.error("Project #{} has NO associated MSI Db", projectId);
		} else {
				this.msiEM = msiDbConnector.createEntityManager();
				this.udsEM = udsDbConnector.createEntityManager();

		}
	}

	public void close() {
		LOG.debug("Closing ProjectHandler EntityManagers of Project #{}", projectId);
		close(msiEM);
		close(udsEM);
	}

	private void close(EntityManager em) {
		if (em!= null) {
			try {
				em.close();
			} catch (Exception exClose) {
				LOG.error("Error closing EntityManager", exClose);
			}
		}
	}

	/**
	 * Find all protein identifier in all databases of a specific list of RSM in MSIdb
	 * specified by it's projectId.
	 *
	 * @param proteinsByDatabank    : ProteinMatches accession (DDatabankProtein) by SearchEntry database (DDatabankInstance)
	 * @param databankBySeqDatabase : databank mapped by their corresponding seDatabaseId
	 * @param rsmIds                : RSM to consider
	 */
	@SuppressWarnings("unchecked")
	public void findProteinIdentifiers(
					final Map<DDatabankInstance, Set<DDatabankProtein>> proteinsByDatabank,
					final Map<Long, DDatabankInstance> databankBySeqDatabase,
					final List<Long> rsmIds) {

		assert (databankBySeqDatabase != null) : "databankBySeqDatabase Map is null";
		assert (proteinsByDatabank != null) : "proteinsByDatabank Map is null";

		final long start = System.currentTimeMillis();
		if (rsmIds.size() > 0) {

			LOG.info("Quering ProteinMatches for {} RSM(s) of Project #{} ", rsmIds.size(), projectId);

			// VDS - TODO : Use outer join query to get all PM even if not directly linked to SeqDb.... Then treat separately !
			// VDS - TODO 2 : Use HQL Query with "Select new MyObj( pm.acc, ...)  " for  VALIDATED_PM_SDM_FOR_RSMS_QUERY  VALIDATED_PM_FOR_RSMS_QUERY

			//-- Get ALL ProteinMatch count associated to RsmIds
			long expectedProteinsCount = -1L;

			final Query countQuery = msiEM.createQuery(VALIDATED_PM_COUNT_FOR_RSMS_QUERY);
			countQuery.setParameter("rsm_ids", rsmIds);
			final Object obj = countQuery.getSingleResult();

			if (obj instanceof Number) {
				expectedProteinsCount = ((Number) obj).longValue();
			}

			LOG.info("{} Databank Instance(s) found in Project #{} ", databankBySeqDatabase.size(), projectId);
			LOG.info("{} Protein Identifiers expected in Project #{} ", expectedProteinsCount, projectId);

			if (expectedProteinsCount > 0L) {

				//-- Get ProteinMatch + Associated SeqDB for untreatedRsm
				int nSEDbIdentifiers = 0;

				final Query query = msiEM.createQuery(VALIDATED_PM_SDM_FOR_RSMS_QUERY).setParameter("rsm_ids", rsmIds);
				//Create 'Fake' DDatabankProtein from info read in MSIdb and map these  DDatabankProtein to msiSeqDbId (same as in databankBySeqDatabase)
				nSEDbIdentifiers = ProjectHandler.createProteinIdentifiersFromQuery(query, databankBySeqDatabase, proteinsByDatabank);

				if (nSEDbIdentifiers >= expectedProteinsCount) {
					//All seDbIdentifiers were found.
					LOG.debug("{} distinct (Protein Identifier, Description, Databank) retrieved via protein_match_Seq_database_map table", nSEDbIdentifiers);

				} else {
					//Still some seDbIdentifiers not found with previous query. Search using searchSettings SeqDBInstances

					final Query pmQuery = msiEM.createQuery(VALIDATED_PM_FOR_RSMS_QUERY).setParameter("rsm_ids", rsmIds);
					nSEDbIdentifiers = ProjectHandler.createProteinIdentifiersFromQuery(query, databankBySeqDatabase, proteinsByDatabank);
					LOG.debug("{} distinct (Protein Identifier, Description, Databank) WITHOUT protein_match_Seq_database_map table", nSEDbIdentifiers);
				}

			} else {
				LOG.warn("There is NO new validated Accession in MSI Project #{}", projectId);
			}

		} // at least one rsm not yet treated and sedb instances not null

		final long end = System.currentTimeMillis();
		final long duration = end - start;
		LOG.info("findProteinIdentifiers() execution for {} RSM(s) : {} ms ", rsmIds.size(), duration);

	}


	public List<Long> findAllRSMIds() {

			final Query udsQuery = udsEM.createQuery(LIST_RSM_IN_DATASET_ID_QUERY);
			udsQuery.setParameter("projectId", projectId);
		  List<Long> rsmIds = udsQuery.getResultList();

			final Query udsQuantQuery = udsEM.createQuery(LIST_QUANT_RSM_ID_QUERY);
			udsQuantQuery.setParameter("projectId", projectId);
			rsmIds.addAll(udsQuantQuery.getResultList());

			return rsmIds;
	}

	/**
	 *
	 * @param forceUpdate : retrieve information even if previously done
	 * @param rsmIdsToTest : specify of subset of RSM of specific project to test : test if should fill data
	 * @return list of RSM IDs to be taken into account
	 */
	public List<Long> filterRSMIdsToUpdate(List<Long> rsmIdsToTest, boolean forceUpdate) {

		List<Long> rsmIdsToFill = new ArrayList<>();

		if (forceUpdate) {
			rsmIdsToFill = rsmIdsToTest;
		} else {
			for (Long rsmId : rsmIdsToTest) {
				// get the properties of the RSM to update
				//FIXME : Use query to get directly properties instead of getting whole RSM.
				ResultSummary rsm = msiEM.find(ResultSummary.class, rsmId);
				if(rsm==null){
					LOG.warn("Unable to get Identification Summary with ID "+rsmId);
					continue;
				}
				JsonObject array = getPropertiesAsJsonObject(rsm.getSerializedProperties());

				// test if the RSM is already calculated
				if (!array.has("is_coverage_updated")) {
					rsmIdsToFill.add(rsmId);
				}
			} // end rsmIds loop
		}
		return rsmIdsToFill;
	}

	private static JsonObject getPropertiesAsJsonObject(String properties) {
		JsonParser parser = new JsonParser();
		JsonObject array = null;
		if ((properties == null) || (properties.isEmpty()))
			properties = "{}";
		try {
			array = parser.parse(properties).getAsJsonObject();
		} catch (Exception e) {
			LOG.error("error accessing properties");
			throw e;
		}
		return array;
	}

	/**
	 * Verify that specified Project is active (no archive was done)
	 *
	 * @return true if project with specified ID is still active
	 */
	public Boolean isProjectActive() {
		Project p = udsEM.find(Project.class, projectId);
		JsonObject propAsJson = getPropertiesAsJsonObject(p.getSerializedProperties());
		// test if the Project is already archived
		return (!propAsJson.has("is_active") || (propAsJson.get("is_active").getAsBoolean()));
	}

	/**
	 * Retrieve Ids of all Project registered in UDS db and which are still
	 * active (no archive was done)
	 *
	 * @return list of IDs of project still active
	 */
	static List<Long> retrieveAllActiveProjectIds() {

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		EntityManager udsEM = udsDbConnector.createEntityManager();
		List<Long> projectIds = ProjectRepository.findAllActiveProjectIds(udsEM);
		return projectIds;
	}

	/**
	 * Retrieve all MSIdb SeqDatabase of a specified MSidb and returns a map of
	 * search engine database instance by msi.SeqDatabase id. Returns null if no
	 * SeqDabase found.
	 * 
	 * @return a map of all SEDatabase Instance of a specified MSIdb or null if
	 *         no SeqDatabase found.
	 */
	public Map<Long, DDatabankInstance> retrieveAllSeqDatabases() {

		Map<Long, DDatabankInstance> result = null;

		final TypedQuery<SeqDatabase> seqDbQuery = msiEM.createQuery(ALL_SEQ_DB_QUERY, SeqDatabase.class);
		final List<SeqDatabase> seqDbs = seqDbQuery.getResultList();

		if ((seqDbs != null) && !seqDbs.isEmpty()) {
			result = new HashMap<>();

			for (final SeqDatabase seqDb : seqDbs) {
				final long seqDbId = seqDb.getId();
				final String name = seqDb.getName();

				if (name == null) {
					LOG.error("SeqDb #{} name is null", seqDbId);

				} else {
					final String trimmedName = name.trim();// Databank name should be trimmed

					if (trimmedName.isEmpty()) {
						LOG.error("SeqDb #{} name is empty", seqDbId);

					} else {
						final String fastaFilePath = seqDb.getFastaFilePath();
						final String version = seqDb.getVersion();

						if (StringUtils.isEmpty(fastaFilePath)) {
							LOG.error("SeqDb #{} fastaFilePath is empty", seqDbId);
						} else {
							final DDatabankInstance databankInstance = new DDatabankInstance(trimmedName, null, fastaFilePath);
							databankInstance.setRelease(version);
							result.put(seqDbId, databankInstance);
						} // End if (fastaFilePath is valid)

					} // End if (trimmedName is valid)

				} // End if (name is not null)

			} // End loop for each seqDb

		} // End if (seqDbs List is not empty)

		return result;
	}

	/**
	 * Fills the proteinsByDatabank map (mapped by database instance)
	 * with a list of protein identifiers. The list of protein identifiers is
	 * retrieved from the specified list of Object arrays.
	 * 
	 * @param query : the SQL query returning  (accession, description and seDatabase.id)
	 * @param databankBySeqDatabase : list of search engine database referenced by their ID
	 * @param proteinsByDatabank : Map (current and to fill) proteinMatches identifiers by the databank to with
	 *                            they are associated
	 * @return the number of protein identifiers added to the map.
	 */
	private static int createProteinIdentifiersFromQuery(
		final Query query,
		final Map<Long, DDatabankInstance> databankBySeqDatabase,
		final Map<DDatabankInstance, Set<DDatabankProtein>> proteinsByDatabank) {

		assert (databankBySeqDatabase != null) : "findProteinIdentifiers() databaseInstances Map is null";
		assert (proteinsByDatabank != null) : "findProteinIdentifiers() proteinIDsByDatabaseInstance Map is null";

		final List<Object[]> lines = query.getResultList();
		int nIdentifiers = 0;

		// Fill seDbIdentifiers map
		if ((lines != null) && !lines.isEmpty()) {
			for (final Object[] line : lines) {
				final int lineLength = line.length;

				if (lineLength >= EXPECTED_LINE_LENGTH) {
					String proteinId = null;
					String description = null;
					Long seqDbId = null;

					if (line[0] instanceof String) {
						proteinId = ((String) line[0]).trim();// identifier should be trimmed
					}
					if (line[1] instanceof String) {
						description = ((String) line[1]).trim();// Description should be trimmed
					}
					if (line[2] instanceof Long) {
						seqDbId = (Long) line[2];
					}

					if (StringUtils.isEmpty(proteinId)) {
						LOG.error("Invalid DatabankProtein value : {}", line[0]);
					} else {
						final DDatabankInstance databaseInstance = databankBySeqDatabase.get(seqDbId);

						if (databaseInstance == null) {
							LOG.error("Unknown SeqDatabase id : {}", line[2]);
						} else {
							Set<DDatabankProtein> proteins = proteinsByDatabank.get(databaseInstance);

							if (proteins == null) {
								proteins = new HashSet<>();
								proteinsByDatabank.put(databaseInstance, proteins);
							}

							final DDatabankProtein protein = new DDatabankProtein(proteinId, description);
							boolean added = proteins.add(protein);
							if (added) nIdentifiers++;
						}

					} // End if (identValue is valid)

				} else {
					LOG.error("Invalid result line length {} (expected : {})", lineLength, EXPECTED_LINE_LENGTH);
				}
			} // End loop for each Result line
		}
		return nIdentifiers;
	}

	/**
	 * Calculate sequence coverage, mass and updates these properties as well as the BioSequence information in msidb.
	 * @param databankBySeqDatabase DDatabankInstance created using msi SeqDabases and referenced by its id (in MSI). Contains all SeqDb of this project
	 * @param rsmIds : RSMs to consider
	 */
	@SuppressWarnings("unchecked")
	public void fillProteinMatchesProperties(final Map<Long, DDatabankInstance> databankBySeqDatabase, final Map<DDatabankInstance, Set<DDatabankProtein>> proteinsByDatabank , final List<Long> rsmIds) {

		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList = new HashMap<>();
		Map<String,  List<DDatabankInstance>> seqDatabankByDbProtMatchIdentifier;


		boolean msiTransactionOK = false;
		try {
			final long startAll = System.currentTimeMillis();
			LOG.info("Quering ProteinMatches for {} RSM(s) of Project #{} ", rsmIds.size(), projectId);

			LOG.info("Filling ProteinMatches properties for {} RSM(s) of Project #{}", rsmIds.size(), projectId);

			if ((databankBySeqDatabase == null) || databankBySeqDatabase.isEmpty()) {
				LOG.warn("There is NO DatabankInstance in MSI Project #{}", projectId);
				msiTransactionOK = true;
			} else {
				LOG.info("Update SeqDatabase release properties for Project #{}. ", projectId);
				updateMSISeqDbRelease(databankBySeqDatabase); //to do only for referenced seqDb from rsms to consider ?

				//Create reverseMap seqDatabankByDbProtMatch
				seqDatabankByDbProtMatchIdentifier = new HashMap<>();
				for(Entry<DDatabankInstance, Set<DDatabankProtein>> e: proteinsByDatabank.entrySet()){
					for(DDatabankProtein dbProt : e.getValue()) {
						List<DDatabankInstance> seqDbs = seqDatabankByDbProtMatchIdentifier.getOrDefault(dbProt.getIdentifier(), new ArrayList<DDatabankInstance>());
						seqDbs.add(e.getKey());
						seqDatabankByDbProtMatchIdentifier.put(dbProt.getIdentifier(), seqDbs);
					}
				}
				// for each RSM in data_set.result_summmary_id
				for (Long rsmId : rsmIds) {
					final long start = System.currentTimeMillis();
					//Start Transaction for each RSM
					msiTransactionOK = false;
					msiEM.getTransaction().begin();

					int psIdcount = 0;
					LOG.debug("Start processing protein sets from RSM Id:" + rsmId);

					final ResultSummary rsm = msiEM.find(ResultSummary.class, rsmId);
					if (rsm == null) {
						LOG.warn("Referenced ReultSummary " + rsmId + " was not found in Project !");
						msiTransactionOK = true;
						msiEM.getTransaction().rollback();
						msiEM.clear();
						continue;
					}

					// Get Sequence Matches informations For current RSM
					Map<Long, List<SequenceMatchInfo>> seqMatchesInfoByProteinMatchId = new HashMap<>();
					// Get Info from SeqMatch
					final Query getSeqMatchInfoQuery = msiEM.createQuery(GET_SEQ_MATCH_INFO_FOR_RS_QUERY);
					getSeqMatchInfoQuery.setParameter("rsId", rsm.getResultSet().getId());
					final List<Object[]> resultSeqMatches = getSeqMatchInfoQuery.getResultList();
					for (Object[] nextEntry : resultSeqMatches) {
						int seqMatchStart = (int) nextEntry[0];
						int seqMatchStop = (int) nextEntry[1];
						Long proteinMatchId = (Long) nextEntry[2];
						Long peptideId = (Long) nextEntry[3];
						if (!seqMatchesInfoByProteinMatchId.containsKey(proteinMatchId)) {
							seqMatchesInfoByProteinMatchId.put(proteinMatchId, new ArrayList<>());
						}
						seqMatchesInfoByProteinMatchId.get(proteinMatchId).add(new SequenceMatchInfo(seqMatchStart, seqMatchStop, peptideId));
					}


					// Get all ProteinSet
					final Query psQuery = msiEM.createQuery(LIST_PS_FOR_RSM_QUERY);
					psQuery.setParameter("rsmId", rsmId);
					final List<ProteinSet> proteinSets = psQuery.getResultList();
					int psIdListSize = proteinSets.size();

					//Get All Peptide Ids identified
					Map<Long, List<Long>> pepIdsByProtSetId = new HashMap<Long, List<Long>>();
					final Query getPepIdQuery = msiEM.createQuery(GET_PEPID_BY_PSID_PM_QUERY);
					getPepIdQuery.setParameter("rsmId", rsmId);
					final List<Object[]> resultPepInsts = getPepIdQuery.getResultList();
					for (Object[] nextEntry : resultPepInsts) {
						Long pepId = (Long) nextEntry[0];
						Long psId = (Long) nextEntry[1];
						if (!pepIdsByProtSetId.containsKey(psId)) {
							pepIdsByProtSetId.put(psId, new ArrayList<>());
						}
						pepIdsByProtSetId.get(psId).add(pepId);
					}


					// For number of observable peptides computation
					fr.profi.chemistry.model.Enzyme enzyme = null;
					MsiSearch msiSearch = rsm.getResultSet().getMsiSearch();
					Set<Enzyme> sameEnzymes = null;
					boolean isSameEnzyme = true;

					if (msiSearch == null) {
						//Look in child and verify same enzyme was used for all child
						List<Long> childSearchesIds = ResultSetRepository.findChildMsiSearchIdsForResultSet(msiEM, rsm.getResultSet().getId());
						for (Long childMsiSearchId : childSearchesIds) {
							MsiSearch childMsiSearch = msiEM.find(MsiSearch.class, childMsiSearchId);
							Set<Enzyme> enzymes = childMsiSearch.getSearchSetting().getEnzymes();
							if (sameEnzymes == null)
								sameEnzymes = enzymes;
							if (!sameEnzymes.equals(enzymes)) {
								isSameEnzyme = false;
								break;
							}
						}
					} else {
						sameEnzymes = msiSearch.getSearchSetting().getEnzymes();
					}

					if (!isSameEnzyme) {
						LOG.warn("Can't get Enzyme for Merged ResultSet as child don't have the same Enzyme ! ");
					} else if (sameEnzymes != null && !sameEnzymes.isEmpty()) {
						//VDS TODO : Why use first 						}
						fr.proline.core.orm.uds.Enzyme ormEnzyme = udsEM.find(fr.proline.core.orm.uds.Enzyme.class, sameEnzymes.iterator().next().getId());
						List<EnzymeCleavage> cleavages = new ArrayList<>();
						for (fr.proline.core.orm.uds.EnzymeCleavage c : ormEnzyme.getCleavages()) {
							cleavages.add(new EnzymeCleavage(c.getId(), c.getSite(), c.getResidues(), (c.getRestrictiveResidues() == null) ? scala.Option.empty() : new Some<String>(c.getRestrictiveResidues())));
						}

						enzyme = new fr.profi.chemistry.model.Enzyme(ormEnzyme.getId(), ormEnzyme.getName(), cleavages.toArray(new EnzymeCleavage[0]), new Some<String>(ormEnzyme.getCleavageRegexp()), false, false, scala.Option.empty());
					}

					//reinit counter
					nbrNoSeqProt = 0;
					nbrManySeqProt = 0;
					nbrBioSeqMissMatch = 0;

					for (ProteinSet protSet : proteinSets) {

						coveredSeqLengthByProtMatchList.clear();
						Map<ProteinMatch, ProteinSetProteinMatchItem> protSetMapByProtMatch = new HashMap<>();
						List<String> allProtMatchesAccession = new ArrayList<>();

						Map<DDatabankInstance, List<String>> proteinsAccByDatabankSubMap  = new HashMap<>();
						//get Coverage length For each ProteinMatch
						for (ProteinSetProteinMatchItem protSet2ProtMatch : protSet.getProteinSetProteinMatchItems()) {
							ProteinMatch currentProtMatch = protSet2ProtMatch.getProteinMatch();
							allProtMatchesAccession.add(currentProtMatch.getAccession());
							List<DDatabankInstance> seqDbInsts = seqDatabankByDbProtMatchIdentifier.get(currentProtMatch.getAccession());
							for(DDatabankInstance seqDbInst : seqDbInsts) {
								List<String> prots = proteinsAccByDatabankSubMap.getOrDefault(seqDbInst, new ArrayList<>());
								prots.add(currentProtMatch.getAccession());
								proteinsAccByDatabankSubMap.put(seqDbInst,prots);
							}
							protSetMapByProtMatch.put(currentProtMatch, protSet2ProtMatch);
							coveredSeqLengthByProtMatchList.put(currentProtMatch, computeSequenceCoverage(seqMatchesInfoByProteinMatchId, currentProtMatch, pepIdsByProtSetId.get(protSet.getId())));
						}

						//--  Get Wrappers for all ProteinMatches: bioSequence and DatabankProtein
						// AND  filters RelatedIdentifier depending to keep only those corresponding to seqDbs
						Map<String, BioSequenceProvider.RelatedIdentifiers> protMatchesObjResult = BioSequenceProvider.findSEDbIdentRelatedData(allProtMatchesAccession, proteinsAccByDatabankSubMap);


						//Update and Save properties
						updateProteinMatchesProperties(coveredSeqLengthByProtMatchList, enzyme, protSetMapByProtMatch, protMatchesObjResult);

						if (psIdcount % 500 == 0) {
							LOG.info("Processed " + psIdcount + " protein sets / " + psIdListSize);
						}
						psIdcount++;
					} // end protein sets

					LOG.info("Processed " + psIdcount + " protein sets / " + psIdListSize);
					LOG.debug("--- Number of proteins with MORE THAN 1 Sequence : {}", nbrManySeqProt);
					LOG.debug("--- Number of proteins with NO Sequence : {}", nbrNoSeqProt);
					LOG.debug("--- Number of proteins with miss matched BioSequence : {}", nbrBioSeqMissMatch);

					//Save RSM Property
					JsonObject array = getPropertiesAsJsonObject(rsm.getSerializedProperties());
					if (!array.has("is_coverage_updated")) {
						LOG.debug(" Saving coverage_updated property for rsm {}.", rsmId);
						array.addProperty("is_coverage_updated", true);
						rsm.setSerializedProperties(array.toString());
						msiEM.merge(rsm);
					}

					msiEM.getTransaction().commit();
					msiTransactionOK = true;
					msiEM.clear();
					final long end = System.currentTimeMillis();
					final long duration = end - start;
					LOG.info("RSM Id #{} successfully processed. Duration : {} ms for {} protein sets ", rsmId, duration, psIdcount);

				} //End go through RSMs

				msiTransactionOK = true;
			} //At least One SEdb

			long duration = System.currentTimeMillis() - startAll;
			LOG.info("Total: fillProteinMatchesProperties() execution : {} ms for project #{} ", duration, projectId);

		} catch (Exception ex) {
			LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			try {
				if (!msiTransactionOK)
					msiEM.getTransaction().rollback();
			} catch (Exception e) {
				e.printStackTrace();
				LOG.error("Error RollingBack MSI Db Project #" + projectId, e);
			}
			throw ex; //throw exception to caller
		}
	}

	private void updateMSISeqDbRelease(Map<Long, DDatabankInstance> databankBySeqDatabase){
		boolean	msiTransactionOK = false;
		try {
			msiEM.getTransaction().begin();

			for (Long seqDbId : databankBySeqDatabase.keySet() ) {
				DDatabankInstance databank = databankBySeqDatabase.get(seqDbId);
				if (databank.getRelease() != null) {
					SeqDatabase msiSeqDb = msiEM.find(SeqDatabase.class, seqDbId);
					msiSeqDb.setVersion(databank.getRelease());
					msiEM.merge(msiSeqDb);
				}
			}

			msiEM.getTransaction().commit();
			msiTransactionOK = true;
		} catch (Exception ex) {
			LOG.error("Error saving SeqDatabase Release", ex);
			try {
				if (!msiTransactionOK  )
					msiEM.getTransaction().rollback();
			} catch (Exception e) {
				e.printStackTrace();
				LOG.error("Error saving SeqDatabase Release" , e);
			}
			throw ex; //throw exception to caller
		}
	}

	private void updateProteinMatchesProperties(
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList,
		fr.profi.chemistry.model.Enzyme enzyme,
		Map<ProteinMatch, ProteinSetProteinMatchItem> protSetMapByProtMatch,
		Map<String, BioSequenceProvider.RelatedIdentifiers> seDbIdentsObjects ) {
		
		int coveredSequenceLength;

		for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatchList.entrySet()) {	
			ProteinMatch protMatch = entry.getKey();
			boolean protMatch2Update = false;
			String protDescription = protMatch.getDescription();
			coveredSequenceLength = entry.getValue();
			BioSequenceProvider.RelatedIdentifiers seDbIdents = seDbIdentsObjects.get(protMatch.getAccession());

			//If many potential DatabankProteins, try using description matching
			int entryIndex = -1;
			if(seDbIdents != null) {
				List<DDatabankProtein> sedbIdentifiers = seDbIdents.getDDatabankProteins();
				if ( !StringUtils.isEmpty(protDescription)) {
					if(sedbIdentifiers.size() >= 1 ) {
						for (int index = 0; index < sedbIdentifiers.size(); index++) {
							DDatabankProtein nextProt = sedbIdentifiers.get(index);
							if (nextProt.getDescription() != null && nextProt.getDescription().equals(protDescription)) {
								entryIndex = index;
								break;
							}
						}
					} else
						entryIndex = 0;
				} else {
					// get missed descriptions for each protein_match. If more than one, use first not null...
					if (sedbIdentifiers.size() >= 1){
						for(int index = 0; index < sedbIdentifiers.size(); index++){
							DDatabankProtein sedbIdent = sedbIdentifiers.get(index);
							//test if sedbIdent description should not be null or empty
							if ((sedbIdent != null) && (sedbIdent.getDescription() != null) && (sedbIdent.getDescription().trim().length() > 0)) {
								protDescription = sedbIdent.getDescription();
								protMatch.setDescription(protDescription);
								entryIndex = index;
								protMatch2Update = true;
								break;
							}
						}
					} else
						entryIndex = 0;
				}
			}
			
			//Use Description to get GeneName :
			if(!StringUtils.isEmpty(protDescription)) {
				String geneName = RegExUtil.getMatchingString(protDescription, ".*GN=([^\\s]+).*");
				if(geneName != null && !geneName.isEmpty()) {
					protMatch.setGeneName(geneName);
					protMatch2Update = true;
				}
			}
			
			if(seDbIdents == null ||  seDbIdents.getDBioSequences() == null ||  seDbIdents.getDBioSequences().isEmpty()) {
					nbrNoSeqProt++;
					if(nbrNoSeqProt <= 10)
						LOG.debug(" ****  FOUND NO Sequence for protein {}. Display only 10 first ", protMatch.getAccession());
				LOG.trace(" ****  FOUND NO Sequence for protein {}", protMatch.getAccession());
			} else {
				List<DBioSequence> protMatchBioSeqs = seDbIdents.getDBioSequences();
				if (protMatchBioSeqs.size() > 1) {
					nbrManySeqProt++;
					if(entryIndex == -1)
						entryIndex = 0;
					if(nbrManySeqProt <= 10)
						LOG.debug(" ****  FOUND MORE THAN 1 Sequence for protein {}. Use sequence at index {} (Display only 10 first) ", protMatch.getAccession(),entryIndex);
					LOG.trace(" ****  FOUND MORE THAN 1 Sequence for protein {}. Use sequence at index {} ", protMatch.getAccession(),entryIndex);
				} else
					entryIndex = 0;
							
				DBioSequence bioSeq = protMatchBioSeqs.get(entryIndex);
				int bioSequenceLenght = bioSeq.getSequence().length();
				// to avoid the indeterminate form : /0
				if ((bioSequenceLenght > 0) && (coveredSequenceLength <= bioSequenceLenght)) {

					//Calculate Coverage and store in MSI
					double coverage = calculateSequenceCoverage(bioSequenceLenght, coveredSequenceLength);
					ProteinSetProteinMatchItem proSetMap = protSetMapByProtMatch.get(protMatch);
					proSetMap.setCoverage(new Float(coverage));
					msiEM.merge(proSetMap);

					//Calculate number of observable peptides and store in MSI
					if(enzyme != null) {
						JsonObject array = getPropertiesAsJsonObject(protMatch.getSerializedProperties());
						if (!array.has("observable_peptide_count")) {
							int observablePeptideCount = DigestionUtils.getObservablePeptidesCount(bioSeq.getSequence(), enzyme);
							LOG.trace(" Saving observable_peptide_count property for proteinMatch {}.", protMatch.getId());
							array.addProperty("observable_peptide_count", observablePeptideCount);
							protMatch.setSerializedProperties(array.toString());
							protMatch2Update = true;
						}
					}
					
					// Save BioSequence
					Long prevBioSeqId = protMatch.getBioSequenceId();
					Long newBioSeqId = bioSeq.getSequenceId();
					if(!newBioSeqId.equals(prevBioSeqId))
						protMatch2Update = true;
					BioSequence msiBioSeq = msiEM.find(BioSequence.class, newBioSeqId);
					int nmass = (int)Math.round(bioSeq.getMass());
					if (msiBioSeq == null) {
						msiBioSeq = new BioSequence();
						msiBioSeq.setAlphabet(Alphabet.AA);
						msiBioSeq.setId(newBioSeqId);
						msiBioSeq.setLength(bioSeq.getSequence().length());
						msiBioSeq.setMass(nmass);
						msiBioSeq.setCrc64(HashUtil.calculateCRC64(bioSeq.getSequence()));
						msiBioSeq.setPi(new Float(bioSeq.getPI()));
						msiBioSeq.setSequence(bioSeq.getSequence());
						msiEM.persist(msiBioSeq);
					} else {
						boolean foundMissMatch = false;
						StringBuffer sb = new StringBuffer(" Following properties don't match with current biosequence with id  ");
						sb.append(newBioSeqId);
						if (msiBioSeq.getLength() != bioSeq.getSequence().length()) {
							foundMissMatch = true;
							sb.append(" sequence length;");
						}

						if (msiBioSeq.getMass() != nmass) {
							foundMissMatch = true;
							sb.append(" sequence mass (");
							sb.append(msiBioSeq.getMass());
							sb.append(" vs ");
							sb.append(nmass);
							sb.append(");");
						}

						if (!msiBioSeq.getSequence().equals(bioSeq.getSequence())) {
							foundMissMatch = true;
							sb.append(" sequence;");
						}
						if (foundMissMatch) {
							nbrBioSeqMissMatch++;
							String msg = sb.toString();
							if(nbrBioSeqMissMatch <= 10)
								LOG.debug(msg+" (Display only first 10)");
							LOG.trace(sb.toString());
						}
					}

					// Save link between ProteinMatch and BioSeq
					protMatch.setBioSequenceId(newBioSeqId);
				} else {
					LOG.warn(" ****  FOUND Sequence is shorter than theorical covered Sequence Length !!! ");
				}
			}
			if(protMatch2Update)
				msiEM.merge(protMatch);
		} // end of proteins list of current protein set
	}

	//TODO: Verifier: pourquoi passer une liste de peptideIds alors qu'ils sont contenus dans les SequenceMatchInfo ?
	private static Integer computeSequenceCoverage(
		Map<Long, List<SequenceMatchInfo>> seqMatchesByProteinMatchId,
		final ProteinMatch protMatch,
		List<Long> peptideIds) {

		// variables definition
		HashSet<Integer> coveredAASet = new HashSet<Integer>();//Set of protein sequence index covered by PeptideMatch	
		List<SequenceMatchInfo> seqMatches = seqMatchesByProteinMatchId.get(protMatch.getId());

		for (SequenceMatchInfo seqMatch : seqMatches) {
			Long pepId = seqMatch.getPeptideId();
			if (peptideIds.contains(pepId)) {
				int start = seqMatch.getStart();
				int stop = seqMatch.getStop();
				// use set to remove duplicate indexes
				coveredAASet.addAll(getIndexesList(start, stop));
			}
		}

		return coveredAASet.size();
	}

	private static double calculateSequenceCoverage(int biosequenceLength, int sequencematchLength) {
		double average = ((double) sequencematchLength / (double) biosequenceLength) * 100;
		return average;
	}

	private static List<Integer> getIndexesList(int start, int stop) {
		List<Integer> sequenceLength = new ArrayList<Integer>();
		for (int i = start; i <= stop; i++) {
			sequenceLength.add(i);
		}
		return (sequenceLength);
	}

}

class SequenceMatchInfo {
	private int m_start;
	private int m_stop;
	private Long m_peptideId;

	public SequenceMatchInfo(int start, int stop, Long pepId) {
		this.m_start = start;
		this.m_stop = stop;
		this.m_peptideId = pepId;
	}

	public int getStart() {
		return m_start;
	}

	public int getStop() {
		return m_stop;
	}

	public Long getPeptideId() {
		return m_peptideId;
	}

}
