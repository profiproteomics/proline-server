package fr.proline.module.seq.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.TypedQuery;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import fr.profi.chemistry.algo.DigestionUtils;
import fr.profi.chemistry.model.EnzymeCleavage;
import fr.profi.util.StringUtils;
import fr.proline.core.orm.msi.BioSequence;
import fr.proline.core.orm.msi.Enzyme;
import fr.proline.core.orm.msi.MsiSearch;
import fr.proline.core.orm.msi.ProteinMatch;
import fr.proline.core.orm.msi.ProteinSet;
import fr.proline.core.orm.msi.ProteinSetProteinMatchItem;
import fr.proline.core.orm.msi.ResultSummary;
import fr.proline.core.orm.msi.SeqDatabase;
import fr.proline.core.orm.msi.repository.ResultSetRepository;
import fr.proline.core.orm.pdi.Alphabet;
import fr.proline.core.orm.uds.Project;
import fr.proline.core.orm.uds.repository.ProjectRepository;
import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.module.seq.util.HashUtil;
import fr.proline.module.seq.util.RegExUtil;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;
import scala.Some;

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

	private static final String LIST_QUANT_RSM_ID_QUERY = "SELECT DISTINCT(mqc.quantResultSummaryId) FROM MasterQuantitationChannel mqc, Dataset dt WHERE dt.project.id= :projectId AND mqc.dataset.id = dt.id and dt.type IN ('QUANTITATION') AND mqc.quantResultSummaryId IS NOT NULL";

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


	/**
	 * Find all Search Engine protein identifier in all Search Engine protein databases of a specific list of RSM in MSIdb specified by it's projectId.
	 * 
	 * @param projectId
	 * @param seDbIdentifiersBySeDbInstance
	 * @param rsmIds : RSM to consider 
	 */
	@SuppressWarnings("unchecked")
	public static void fillSEDbIdentifiersBySEDb(
		final long projectId,
		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiersBySeDbInstance,
		final Map<Long, SEDbInstanceWrapper> seDbInstances,
		final List<Long> rsmIds) {

		if (seDbIdentifiersBySeDbInstance == null) {
			throw new IllegalArgumentException("SeDbIdentifiers Map is null");
		}

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();

		if (msiDbConnector == null) {
			LOG.warn("Project #{} has NO associated MSI Db", projectId);
		} else {
			EntityManager msiEM = null;
			EntityManager udsEM = null;
			final long start = System.currentTimeMillis();
			try {

				msiEM = msiDbConnector.createEntityManager();
				udsEM = udsDbConnector.createEntityManager();

				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);

				} else if (rsmIds.size() > 0) {

					LOG.info(" Quering SEDbIdentifiers for {} RSM(s) for project #{} ", rsmIds.size(), projectId);

					// VDS - TODO : Use outer join query to get all PM even if not directly linked to SeqDb.... Then treat separately ! 
					// VDS - TODO 2 : Use HQL Query with "Select new MyObj( pm.acc, ...)  " for  VALIDATED_PM_SDM_FOR_RSMS_QUERY  VALIDATED_PM_FOR_RSMS_QUERY

					//-- Get ALL ProteinMatch count associated to untreatedRsm
					long nExpectedAccessions = -1L;

					final Query countQuery = msiEM.createQuery(VALIDATED_PM_COUNT_FOR_RSMS_QUERY);
					countQuery.setParameter("rsm_ids", rsmIds);
					final Object obj = countQuery.getSingleResult();

					if (obj instanceof Number) {
						nExpectedAccessions = ((Number) obj).longValue();
					}

					LOG.info("MSI Project #{} found {} SEDbInstances and {} validated Accession", projectId, seDbInstances.size(), nExpectedAccessions);

					if (nExpectedAccessions > 0L) {

						//-- Get ProteinMatch + Associated SeqDB for untreatedRsm
						int nSEDbIdentifiers = 0;

						final Query pmSdmQuery = msiEM.createQuery(VALIDATED_PM_SDM_FOR_RSMS_QUERY);
						pmSdmQuery.setParameter("rsm_ids", rsmIds);
						final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();

						// Fill seDbIdentifiers map
						if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
							nSEDbIdentifiers = fillSEDbIdentifiers(pmSdmLines, seDbInstances, seDbIdentifiersBySeDbInstance);
						}

						if (nSEDbIdentifiers >= nExpectedAccessions) {
							//All seDbIdentifiers were found.
							LOG.debug("{} distinct (validated Accession, Description, SeqDatabase) retrieved via ProteinMatchSeqDatabaseMap",nSEDbIdentifiers);

						} else {
							//Still some seDbIdentifiers not found with previous query. Search using searchSettings SeqDBInstances
							nSEDbIdentifiers = 0;

							final Query pmQuery = msiEM.createQuery(VALIDATED_PM_FOR_RSMS_QUERY);
							pmQuery.setParameter("rsm_ids", rsmIds);
							final List<Object[]> pmLines = pmQuery.getResultList();

							if ((pmLines != null) && !pmLines.isEmpty()) {
								// Fill seDbIdentifiers map
								nSEDbIdentifiers = fillSEDbIdentifiers(pmLines, seDbInstances, seDbIdentifiersBySeDbInstance);
							}
							LOG.debug("{} distinct (validated Accession, Description, SeqDatabase) WITHOUT ProteinMatchSeqDatabaseMap",nSEDbIdentifiers);
						}

					} else {
						LOG.warn("There is NO new validated Accession in MSI Project #{}", projectId);
					}

				} // at least one rsm not yet treated and sedb instances not null

				final long end = System.currentTimeMillis();
				final long duration = end - start;
				LOG.info(" Total fillSEDbIdentifiersBySEDb() execution for {} RSM(s) : {} ms ", rsmIds.size(), duration);
			
			} finally {
				if (msiEM != null) {
					try {
						LOG.debug(" CLOSE MSI Db EntityManager for project #" + projectId);
						msiEM.close();
						udsEM.close();
					} catch (Exception exClose) {
						LOG.error("Error closing MSI Db EntityManager", exClose);
					}
				}
			}
		} // End no MSIDb connector
	}

	@SuppressWarnings("unchecked")
	/**
	 * 
	 * @param projectId : Project to get RSM to fill for
	 * @param forceUpdate : retrieve information even if previously done
	 * @param rsmIdsToTest : specify of subset of RSM of specific project to test : test if should fill data 
	 * @param udsEM
	 * @return
	 */
	public static List<Long> retrieveRSMIdToFill(Long projectId, boolean forceUpdate, List<Long> rsmIdsToTest, EntityManager udsEM, EntityManager msiEM) {

		List<Long> rsmIdsToFill = new ArrayList<>();

		if (rsmIdsToTest == null || rsmIdsToTest.isEmpty()) {
			final Query udsQuery = udsEM.createQuery(LIST_RSM_IN_DATASET_ID_QUERY);
			udsQuery.setParameter("projectId", projectId);
			rsmIdsToTest = udsQuery.getResultList();

			final Query udsQuantQuery = udsEM.createQuery(LIST_QUANT_RSM_ID_QUERY);
			udsQuantQuery.setParameter("projectId", projectId);
			rsmIdsToTest.addAll(udsQuantQuery.getResultList());
		}

		if (forceUpdate)
			rsmIdsToFill = rsmIdsToTest;
		else {
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
		if ((properties == null) || (properties.isEmpty())) properties = "{}";
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
	 * @param udsEM
	 * @param projectId
	 * @return
	 */
	public static Boolean isProjectActive(Long pId, final EntityManager udsEM) {		
		Project p = udsEM.find(Project.class, pId);
		JsonObject propAsJson = getPropertiesAsJsonObject(p.getSerializedProperties());
		// test if the Project is already archived
		return (!propAsJson.has("is_active") || (propAsJson.get("is_active").getAsBoolean()));
	}
	
	/**
	 * Retrieve Ids of all Project registered in UDS db and which are still active (no archive was done)
	 * @param udsEM
	 * @return
	 */
	public static List<Long> retrieveAllActiveProjectIds(final EntityManager udsEM) {		

		List<Long> projectIds = null;
		projectIds = ProjectRepository.findAllActiveProjectIds(udsEM);
		return projectIds;
	}

	/**
	 * Retrieve all MSIdb SeqDatabase of a specified MSidb and returns a map of search engine database instance by msi.SeqDatabase id. Returns null if no
	 * SeqDabase found.
	 * 
	 * @param msiEM
	 * @return a map of all SEDatabase Instance of a specified MSIdb or null if no SeqDatabase found.
	 */
	public static Map<Long, SEDbInstanceWrapper> retrieveAllSeqDatabases(final EntityManager msiEM) {

		Map<Long, SEDbInstanceWrapper> result = null;

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
					final String trimmedName = name.trim();// SEDb name should be trimmed

					if (trimmedName.isEmpty()) {
						LOG.error("SeqDb #{} name is empty", seqDbId);

					} else {
						final String fastaFilePath = seqDb.getFastaFilePath();

						if (StringUtils.isEmpty(fastaFilePath)) {
							LOG.error("SeqDb #{} fastaFilePath is empty", seqDbId);
						} else {
							final SEDbInstanceWrapper seDbInstanceW = new SEDbInstanceWrapper(trimmedName, null,
								fastaFilePath);
							result.put(Long.valueOf(seqDbId), seDbInstanceW);
						} // End if (fastaFilePath is valid)

					} // End if (trimmedName is valid)

				} // End if (name is not null)

			} // End loop for each seqDb

		} // End if (seqDbs List is not empty)

		return result;
	}
	

	/**
	 * Fills the seDbIdentifiers map (mapped by search engine database instance) with a list of protein identifiers. The list of protein identifiers is read
	 * from the specified list of Object arrays.
	 * 
	 * @param lines
	 * @param seDbInstances
	 * @param seDbIdentifiers
	 * @return the number of protein identifiers added to the map.
	 */
	private static int fillSEDbIdentifiers(
		final List<Object[]> lines,
		final Map<Long, SEDbInstanceWrapper> seDbInstances,
		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers) {

		assert (lines != null) : "fillSEDbIdentifiers() lines List is null";
		assert (seDbInstances != null) : "fillSEDbIdentifiers() seDbInstances Map is null";
		assert (seDbIdentifiers != null) : "fillSEDbIdentifiers() seDbIdentifiers Map is null";

		int nIdentifiers = 0;

		for (final Object[] line : lines) {
			final int lineLength = line.length;

			if (lineLength >= EXPECTED_LINE_LENGTH) {
				String value = null;
				String description = null;
				Long seqDbId = null;

				if (line[0] instanceof String) {
					value = ((String) line[0]).trim();// SEDbIdent should be trimmed
				}
				if (line[1] instanceof String) {
					description = ((String) line[1]).trim();// Description should be trimmed
				}
				if (line[2] instanceof Long) {
					seqDbId = (Long) line[2];
				}

				if (StringUtils.isEmpty(value)) {
					LOG.error("Invalid SEDbIdentifier value : {}", line[0]);
				} else {
					final SEDbInstanceWrapper seDbInstance = seDbInstances.get(seqDbId);

					if (seDbInstance == null) {
						LOG.error("Unknown SeqDatabase id : {}", line[2]);
					} else {
						Set<SEDbIdentifierWrapper> seDbIdents = seDbIdentifiers.get(seDbInstance);

						if (seDbIdents == null) {
							seDbIdents = new HashSet<>();
							seDbIdentifiers.put(seDbInstance, seDbIdents);
						}

						final SEDbIdentifierWrapper seDbIdentifierW = new SEDbIdentifierWrapper(value, description);
						seDbIdents.add(seDbIdentifierW);
						++nIdentifiers;// Found a valid SEDbIdentifier
					}

				} // End if (identValue is valid)

			} else {
				LOG.error("Invalid result line length {} (expected : {})", lineLength, EXPECTED_LINE_LENGTH);
			}
		} // End loop for each Result line
		return nIdentifiers;
	}
	
	private static int nbrNoSeqProt;
	private static int nbrManySeqProt;


	/**
	 * Calculate sequence coverage, mass and updates these properties as well as the BioSequence information in msidb.
	 * 
	 * @param projectId
	 * @param rsmIds : RSMs to consider
	 * @param forceUpdate
	 */
	@SuppressWarnings("unchecked")
	public static void fillProteinMatchesProperties(final long projectId, final Map<Long, SEDbInstanceWrapper> seDbInstances, final List<Long> rsmIds) {

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList = new HashMap<ProteinMatch, Integer>();
		if (msiDbConnector == null) {
			LOG.warn("Project #{} has NO associated MSI Db", projectId);
		} else {

			EntityManager msiEM = null;
			EntityManager udsEM = null;
			boolean msiTransactionOK = false;
			try {
				final long startAll = System.currentTimeMillis();
				msiEM = msiDbConnector.createEntityManager();
				udsEM = udsDbConnector.createEntityManager();

				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
					msiTransactionOK = true;
				} else {
					LOG.info(" Filling ProteinMatches properties on project {}. Found a total of {} rsm", projectId, rsmIds.size());

					// for each RSM in data_set.result_summmary_id
					for (Long rsmId : rsmIds) {
						final long start = System.currentTimeMillis();
						//Start Transaction for each RSM
						msiTransactionOK = false;
						msiEM.getTransaction().begin();

						int psIdcount = 0;
						LOG.debug("Going to compute rsmId:" + rsmId);

						final ResultSummary rsm = msiEM.find(ResultSummary.class, rsmId);
						if(rsm == null) {
							LOG.warn("Referenced ReultSummary " + rsmId+" was not found in Project !");
							msiTransactionOK = true;
							msiEM.getTransaction().rollback();
							msiEM.clear();
							continue;
						}
						
						// Get SeqMatches informations For current RSM
						Map<Long, List<SequenceMatchInfo>> seqMatchesInfoByProteinMatchId = new HashMap<>();
						// Get Info from SeqMatch 
						final Query getSeqMatchInfoQuery = msiEM.createQuery(GET_SEQ_MATCH_INFO_FOR_RS_QUERY);
						getSeqMatchInfoQuery.setParameter("rsId", rsm.getResultSet().getId());
						final List<Object[]> resultSeqMatches = getSeqMatchInfoQuery.getResultList();
						for (Object[] nextEntry : resultSeqMatches) {
							int seqMatchStart = (int) nextEntry[0];
							int seqMatchStop= (int) nextEntry[1];
							Long seqMatchProtMatchId= (Long) nextEntry[2];
							Long seqMatchPepId= (Long) nextEntry[3];
							if (!seqMatchesInfoByProteinMatchId.containsKey(seqMatchProtMatchId))
								seqMatchesInfoByProteinMatchId.put(seqMatchProtMatchId, new ArrayList<SequenceMatchInfo>());
							seqMatchesInfoByProteinMatchId.get(seqMatchProtMatchId).add( new SequenceMatchInfo(seqMatchStart, seqMatchStop, seqMatchPepId));
						} 
						
						

						// Get all ProteinSet
						final Query psQuery = msiEM.createQuery(LIST_PS_FOR_RSM_QUERY);
						psQuery.setParameter("rsmId", rsmId);
						final List<ProteinSet> protSets = psQuery.getResultList();
						int psIdListSize = protSets.size();

						//Get All Peptide Ids identified
						Map<Long, List<Long>> pepIdsByProtSetId = new HashMap<Long, List<Long>>();
						final Query getPepIdQuery = msiEM.createQuery(GET_PEPID_BY_PSID_PM_QUERY);
						getPepIdQuery.setParameter("rsmId", rsmId);
						final List<Object[]> resultPepInsts = getPepIdQuery.getResultList();
						for (Object[] nextEntry : resultPepInsts) {
							Long pepId = (Long) nextEntry[0];
							Long psId = (Long) nextEntry[1];
							if (!pepIdsByProtSetId.containsKey(psId))
								pepIdsByProtSetId.put(psId, new ArrayList<Long>());
							pepIdsByProtSetId.get(psId).add(pepId);
						}
						
						
						// For number of observable peptides computation 
						fr.profi.chemistry.model.Enzyme enzyme = null;
						MsiSearch msiSearch = rsm.getResultSet().getMsiSearch();
						Set<Enzyme> sameEnzymes = null;
						boolean isSameEnzyme = true;
						
						if(msiSearch == null){							
							//Look in child and verify same enzyme was used for all child
							List<Long> childSearchesIds = ResultSetRepository.findChildMsiSearchIdsForResultSet(msiEM, rsm.getResultSet().getId());														
							for(Long childMsiSearchId : childSearchesIds){
								MsiSearch childMsiSearch = msiEM.find(MsiSearch.class, childMsiSearchId);
								Set<Enzyme> enzymes = childMsiSearch.getSearchSetting().getEnzymes();	
								if(sameEnzymes == null)
									sameEnzymes = enzymes;
								if(!sameEnzymes.equals(enzymes)){
									isSameEnzyme = false;
									break;
								}									
							}
						} else {
							sameEnzymes = msiSearch.getSearchSetting().getEnzymes();	
						}
						
						if(!isSameEnzyme){
							LOG.warn("Can't get Enzyme for Merged ResultSet as child don't have the same Enzyme ! "); 
						} else {
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
						
						for (ProteinSet protSet : protSets) {

							coveredSeqLengthByProtMatchList.clear();
							Map<ProteinMatch, ProteinSetProteinMatchItem> protSetMapByProtMatch = new HashMap<>();
							List<String> allProtMatchesAccession = new ArrayList<>();

							//get Coverage length For each ProteinMatch
							for (ProteinSetProteinMatchItem protSet2ProtMatch : protSet.getProteinSetProteinMatchItems()) {
								ProteinMatch currentProtMatch = protSet2ProtMatch.getProteinMatch();
								allProtMatchesAccession.add(currentProtMatch.getAccession());
								protSetMapByProtMatch.put(currentProtMatch, protSet2ProtMatch);
								coveredSeqLengthByProtMatchList.put(currentProtMatch,
									getSeqCoverageForProteinMatch(seqMatchesInfoByProteinMatchId, protSet2ProtMatch.getProteinMatch(),
										pepIdsByProtSetId.get(protSet.getId())));
							}

							//--  Get Wrappers for all ProteinMatches: bioSequence and SEDbIdentifier
							Map<String,BioSequenceProvider.SEDbIdentifierRelated> protMatchesObjResult = BioSequenceProvider.findSEDbIdentRelatedData(allProtMatchesAccession);														
							//Update and Save properties
							updateProteinMatchesProperties(coveredSeqLengthByProtMatchList, msiEM, enzyme, protSetMapByProtMatch, protMatchesObjResult);

							if (psIdcount % 500 == 0) {
								LOG.info("Processed " + psIdcount + " protein sets / " + psIdListSize);
							}
							psIdcount++;
						} // end protein sets
						
						LOG.info("Processed " + psIdcount + " protein sets / " + psIdListSize);
						LOG.debug("--- Number of proteins with MORE THAN 1 Sequence : {}", nbrManySeqProt);
					    LOG.debug("--- Number of proteins with NO Sequence : {}", nbrNoSeqProt);

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
						LOG.info("rsmId: {} successfully/already calculated. Duration : {} ms for {} protein sets ", rsmId, duration, psIdcount);

					} //End go through RSMs

					msiTransactionOK = true;
				} //At least One SEdb

				final long endAll = System.currentTimeMillis();
				final long duration = endAll - startAll;
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
			} finally {
				if (msiEM != null) {
					try {
						//			    con.commit();
						msiEM.close();
						udsEM.close();

					} catch (Exception exClose) {
						LOG.error("Error closing MSI Db EntityManager", exClose);
					}
				}
			}
		} //End msiDbConnector !=null
	}

	private static void updateProteinMatchesProperties(
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList,
		EntityManager msiEM,	
		fr.profi.chemistry.model.Enzyme enzyme,
		Map<ProteinMatch, ProteinSetProteinMatchItem> protSetMapByProtMatch,
		Map<String,BioSequenceProvider.SEDbIdentifierRelated> seDbIdentsObjects ) {
		
		int coveredSequenceLength;
		
		// get missed descriptions for each protein_match.
		for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatchList.entrySet()) {	
			ProteinMatch protMatch = entry.getKey();
			boolean protMatch2Update = false;
			String protDescription = protMatch.getDescription();
			coveredSequenceLength = entry.getValue();
			BioSequenceProvider.SEDbIdentifierRelated seDbIdents = seDbIdentsObjects.get(protMatch.getAccession());
			if ( (protDescription == null || protDescription.isEmpty()) && !(seDbIdents ==null)) {
				List<SEDbIdentifierWrapper> sedbIdentifiers = seDbIdents.getSEDbIdentWrappers();
				if ((sedbIdentifiers != null) && (sedbIdentifiers.size() >= 1)) {
					SEDbIdentifierWrapper sedbIdent = sedbIdentifiers.get(0);
					//sedbIdent description should not be null or empty 
					if ((sedbIdent != null) && (sedbIdent.getDescription() != null)) {
						if (sedbIdent.getDescription().trim().length() > 0){
							protDescription = sedbIdent.getDescription();
							protMatch.setDescription(protDescription);	
							protMatch2Update = true;
						}
					}
				}
			}
			
			//Use Description to get GeneName :
			if((protDescription != null) && (!protDescription.isEmpty())) {
				String geneName = RegExUtil.getMatchingString(protDescription, ".*GN=([^\\s]+).*");
				if(geneName != null && !geneName.isEmpty()) {
					protMatch.setGeneName(geneName);
					protMatch2Update = true;
				}		
			}
			
			if(seDbIdents== null ||  seDbIdents.getBioSequenceWrappers() == null ||  seDbIdents.getBioSequenceWrappers().isEmpty()) {
					nbrNoSeqProt++;
					LOG.trace(" ****  FOUND NO Sequence for protein {}", protMatch.getAccession());
			} else {
				List<BioSequenceWrapper> protMatchBioSeqs = seDbIdents.getBioSequenceWrappers();
				if (protMatchBioSeqs.size() > 1) {
					nbrManySeqProt++;
					LOG.trace(" ****  FOUND MORE THAN 1 Sequence for protein {}. Use first one  ", protMatch.getAccession());
				}
							
				BioSequenceWrapper bioSeq = protMatchBioSeqs.get(0);
				int bioSequenceLentgh = bioSeq.getSequence().length();
				// to avoid the indeterminate form : /0
				if ((bioSequenceLentgh > 0) && (coveredSequenceLength < bioSequenceLentgh)) {

					//Calculate Coverage and store in MSI
					double coverage = calculateSequenceCoverage(bioSequenceLentgh, coveredSequenceLength);
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
					BioSequence msiBioSeq = msiEM.find(BioSequence.class, bioSeq.getSequenceId());
					if (msiBioSeq == null) {
						msiBioSeq = new BioSequence();
						msiBioSeq.setAlphabet(Alphabet.AA);
						msiBioSeq.setId(bioSeq.getSequenceId());
						msiBioSeq.setLength(bioSeq.getSequence().length());
						msiBioSeq.setMass(new Double(bioSeq.getMass()).intValue());
						msiBioSeq.setCrc64(HashUtil.calculateCRC64(bioSeq.getSequence()));
						msiBioSeq.setPi(new Float(bioSeq.getPI()));
						msiBioSeq.setSequence(bioSeq.getSequence());
						msiEM.persist(msiBioSeq);
					} else {
						boolean foundMissMatch = false;
						StringBuffer sb = new StringBuffer(" Following properties don't match with current biosequence with id  ");
						sb.append(bioSeq.getSequenceId());
						if (msiBioSeq.getLength() != bioSeq.getSequence().length()) {
							foundMissMatch = true;
							sb.append(" sequence length;");
						}

						if (msiBioSeq.getMass() != new Double(bioSeq.getMass()).intValue()) {
							foundMissMatch = true;
							sb.append(" sequence mass (");
							sb.append(msiBioSeq.getMass());
							sb.append(" vs ");
							sb.append(new Double(bioSeq.getMass()).intValue());
							sb.append(");");
						}

						if (!msiBioSeq.getSequence().equals(bioSeq.getSequence())) {
							foundMissMatch = true;
							sb.append(" sequence;");
						}
						if (foundMissMatch)
							LOG.warn(sb.toString());
					}

					// Save link between ProteinMatch and BioSeq
					protMatch.setBioSequenceId(bioSeq.getSequenceId());
				}
			}
			if(protMatch2Update)
				msiEM.merge(protMatch);
		} // end of proteins list of current protein set
	}

	private static Integer getSeqCoverageForProteinMatch(
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
				coveredAASet.addAll(getSequencesIndexes(start, stop));
			}
		}

		return coveredAASet.size();
	}

	public static double calculateSequenceCoverage(int biosequenceLength, int sequencematchLength) {

		double average = ((double) sequencematchLength / (double) biosequenceLength) * 100;
		return average;
	}

	public static List<Integer> getSequencesIndexes(int start, int stop) {
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
    
    public SequenceMatchInfo(int start,int stop, Long pepId){
    	this.m_start = start;
    	this.m_stop = stop;
    	this.m_peptideId =  pepId;
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
