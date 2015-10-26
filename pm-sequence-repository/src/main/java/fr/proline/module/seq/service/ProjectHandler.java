package fr.proline.module.seq.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Query;
import javax.persistence.TypedQuery;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import fr.profi.util.StringUtils;
import fr.proline.core.orm.msi.BioSequence;
import fr.proline.core.orm.msi.PeptideInstance;
import fr.proline.core.orm.msi.ProteinMatch;
import fr.proline.core.orm.msi.ProteinSet;
import fr.proline.core.orm.msi.ProteinSetProteinMatchItem;
import fr.proline.core.orm.msi.ResultSummary;
import fr.proline.core.orm.msi.SeqDatabase;
import fr.proline.core.orm.msi.SequenceMatch;
import fr.proline.core.orm.msi.SequenceMatchPK;
import fr.proline.core.orm.msi.repository.SequenceMatchRepository;
import fr.proline.core.orm.pdi.Alphabet;
import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.module.seq.util.HashUtil;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;

public class ProjectHandler {

	private static final Logger LOG = LoggerFactory.getLogger(ProjectHandler.class);

	private static final String ALL_SEQ_DB_QUERY = "FROM fr.proline.core.orm.msi.SeqDatabase";

	private static final String VALIDATED_PM_COUNT_FOR_RSMS_QUERY = "SELECT COUNT (DISTINCT pm.accession)"
			+ " FROM fr.proline.core.orm.msi.ProteinMatch pm JOIN pm.proteinSetProteinMatchItems ps"
			+ " WHERE ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
			+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary.id IN (:rsm_ids) )";

	private static final String VALIDATED_PM_SDM_FOR_RSMS_QUERY = "SELECT DISTINCT pm.accession, pm.description, sdb.id"
			+ " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SeqDatabase sdb, fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap pmsdb"
			+ " JOIN pm.proteinSetProteinMatchItems ps"
			+ " WHERE (pmsdb.id.proteinMatchId = pm.id) AND (pmsdb.id.seqDatabaseId = sdb.id)"
			+ " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
			+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary.id IN  (:rsm_ids))";

	private static final String VALIDATED_PM_FOR_RSMS_QUERY = "SELECT DISTINCT pm.accession, pm.description, ssdm.seqDatabase.id"
			+ " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SearchSettingsSeqDatabaseMap ssdm"
			+ " JOIN pm.proteinSetProteinMatchItems ps"
			+ " WHERE (pm.resultSet.msiSearch.searchSetting = ssdm.searchSetting)"
			+ " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
			+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary.id IN  (:rsm_ids))";

	private static final String UPDATE_QUERY_RSM = "UPDATE result_summary  set serialized_properties=:sr where id=:rsmId";

	private static final String LIST_RSM_IN_DATASET_ID_QUERY = "SELECT DISTINCT(dt.resultSummaryId) FROM Dataset dt WHERE dt.project.id= :projectId AND dt.type IN ('AGGREGATE','IDENTIFICATION') AND dt.resultSummaryId IS NOT NULL";

	private static final String LIST_PS_FOR_RSM_QUERY = "SELECT ps FROM ProteinSet ps WHERE ps.resultSummary.id= :rsmId AND ps.isValidated = 'true'";

	private static final int EXPECTED_LINE_LENGTH = 3;

	
	private static final String GET_PEP_INSTANCE_BY_PS_PM_QUERY = "SELECT pi, ps.id "
			+ "FROM fr.proline.core.orm.msi.PeptideInstance pi, fr.proline.core.orm.msi.PeptideSetPeptideInstanceItem  pspi, "
			+ " fr.proline.core.orm.msi.ProteinSet ps "
			+ " WHERE  pspi.resultSummary.id = :rsmId "
			+ " AND pspi.peptideInstance= pi "
			+ " AND ps.resultSummary.id = :rsmId AND ps.isValidated=true AND pspi.peptideSet.proteinSet = ps";

	
	/**
	 * Find all Search Engine protein identifier in all Search Engine protein databases of a specific MSIdb speficied by it's projectId.
	 * 
	 * @param projectId
	 * @param seDbIdentifiersBySeDbInstance
	 * @param forceUpdate
	 */
	@SuppressWarnings("unchecked")
	public static void fillSEDbIdentifiersBySEDb(
		final long projectId,
		final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiersBySeDbInstance,
		boolean forceUpdate) {

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

				final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
				msiEM = emf.createEntityManager();
				final EntityManagerFactory em_uds = udsDbConnector.getEntityManagerFactory();
				udsEM = em_uds.createEntityManager();
				final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);
				final List<Long> untreatedRsmIds = new ArrayList<Long>();

				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);

				} else {
					// check if any rsm has not been validated within this project

					final Query udsQuery = udsEM.createQuery(LIST_RSM_IN_DATASET_ID_QUERY);
					udsQuery.setParameter("projectId", projectId);
					final List<Long> rsmIds = udsQuery.getResultList();
					
					if (forceUpdate)
						untreatedRsmIds.addAll(rsmIds);
					else {
						for (Long rsmId : rsmIds) {

							// get the properties of the RSM to update
							ResultSummary rsm = msiEM.find(ResultSummary.class, rsmId);
							String properties = rsm.getSerializedProperties();
							JsonParser parser = new JsonParser();
							JsonObject array = null;
							try {
								array = parser.parse(properties).getAsJsonObject();
							} catch (Exception e) {
								LOG.debug("error accessing RSM properties for rsm id:" + rsmId + " in project " + projectId + " forcing retrieve ");
								array = parser.parse("{}").getAsJsonObject();
							}

							// test if the RSM is already calculated

							if (!array.has("is_coverage_updated")) {
								untreatedRsmIds.add(rsmId);
							}
						} // end rsmIds loop
					}

					if (untreatedRsmIds.size() > 0) {
						if (forceUpdate) {
							LOG.info(" Quering SEDbIdentifiers for all RSMs for this project ({})", projectId);
						} else {
							LOG.info(" Quering SEDbIdentifiers for {} RSM(s) for this project ({})  ", untreatedRsmIds.size(), projectId);
						}

						// VDS - TODO : Use outer join query to get all PM even if not directly linked to SeqDb.... Then treat separatly ! 
						// VDS - TODO 2 : Use HQL Query with "Select new MyObj( pm.acc, ...)  " for  VALIDATED_PM_SDM_FOR_RSMS_QUERY  VALIDATED_PM_FOR_RSMS_QUERY

						//-- Get ALL ProteinMatch count associated to untreatedRsm
						long nExpectedAccessions = -1L;

						final Query countQuery = msiEM.createQuery(VALIDATED_PM_COUNT_FOR_RSMS_QUERY);
						countQuery.setParameter("rsm_ids", untreatedRsmIds);
						final Object obj = countQuery.getSingleResult();

						if (obj instanceof Number) {
							nExpectedAccessions = ((Number) obj).longValue();
						}

					    LOG.info("MSI Project #{} found {} SEDbInstances and {} validated Accession", projectId, seDbInstances.size(),nExpectedAccessions);

						if (nExpectedAccessions > 0L) {

							//-- Get ProteinMatch + Associated SeqDB for untreatedRsm
							int nSEDbIdentifiers = 0;

							final Query pmSdmQuery = msiEM.createQuery(VALIDATED_PM_SDM_FOR_RSMS_QUERY);
							pmSdmQuery.setParameter("rsm_ids", untreatedRsmIds);
							final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();

							// Fill seDbIdentifiers map
							if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
								nSEDbIdentifiers = fillSEDbIdentifiers(pmSdmLines, seDbInstances, seDbIdentifiersBySeDbInstance);
							}

							if (nSEDbIdentifiers >= nExpectedAccessions) {
								//All seDbIdentifiers were found.
								if (LOG.isDebugEnabled()) {
									LOG.debug("{} distinct (validated Accession, Description, SeqDatabase) retrieved via ProteinMatchSeqDatabaseMap",
										nSEDbIdentifiers);
								}

							} else {
								//Still some seDbIdentifiers not found with previous query. Search using searchSettings SeqDBInstances
								nSEDbIdentifiers = 0;

								final Query pmQuery = msiEM.createQuery(VALIDATED_PM_FOR_RSMS_QUERY);
								pmQuery.setParameter("rsm_ids", untreatedRsmIds);
								final List<Object[]> pmLines = pmQuery.getResultList();

								if ((pmLines != null) && !pmLines.isEmpty()) {
									// Fill seDbIdentifiers map
									nSEDbIdentifiers = fillSEDbIdentifiers(pmLines, seDbInstances, seDbIdentifiersBySeDbInstance);
								}
								if (LOG.isDebugEnabled()) {
									LOG.debug("{} distinct (validated Accession, Description, SeqDatabase) WITHOUT ProteinMatchSeqDatabaseMap",
										nSEDbIdentifiers);
								}
							}

						} else {
							LOG.warn("There is NO new validated Accession in MSI Project #{}", projectId);
						}

					} // at least one rsm not yet treated

				} // sedb instances not null

				final long end = System.currentTimeMillis();
				final long duration = end - start;
				LOG.info(" Total fillSEDbIdentifiersBySEDb() execution for {} RSM(s) : {} ms ", untreatedRsmIds.size(), duration);

			} catch (Exception ex) {
				LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			} finally {
				if (msiEM != null) {
					try {
						LOG.debug(" CLOSE MSI Db EntityManager for project " + projectId);
						msiEM.close();
						udsEM.close();
					} catch (Exception exClose) {
						LOG.error("Error closing MSI Db EntityManager", exClose);
					}
				}
			}
		} // End no MSIDb connector
	}

	/**
	 * Retrieve all MSIdb SeqDatabase of a specified MSidb and returns a map of search engine database instance by msi.SeqDatabase id. Returns null if no
	 * SeqDabase found.
	 * 
	 * @param msiEM
	 * @return a map of all SEDatabase Instance of a specified MSIdb or null if no SeqDatabase found.
	 */
	private static Map<Long, SEDbInstanceWrapper> retrieveAllSeqDatabases(final EntityManager msiEM) {

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

		assert(lines != null) : "fillSEDbIdentifiers() lines List is null";
		assert(seDbInstances != null) : "fillSEDbIdentifiers() seDbInstances Map is null";
		assert(seDbIdentifiers != null) : "fillSEDbIdentifiers() seDbIdentifiers Map is null";

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

	/**
	 * Calculate sequence coverage, mass and updates these properties as well as the BioSequence information in msidb.
	 * 
	 * @param projectId
	 * @param forceUpdate
	 */
	@SuppressWarnings("unchecked")
	public static void fillProteinMatchesProperties(final long projectId, boolean forceUpdate) {

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList = new HashMap<ProteinMatch, Integer>();

		EntityManager msiEM = null;
		EntityManager udsEM = null;
		boolean msiTransactionOK = false;

		try {
			final long startAll = System.currentTimeMillis();
			final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
			msiEM = emf.createEntityManager();
			final EntityManagerFactory em = udsDbConnector.getEntityManagerFactory();
			udsEM = em.createEntityManager();

			// get the list of RSM
			final Query udsQuery = udsEM.createQuery(LIST_RSM_IN_DATASET_ID_QUERY);
			udsQuery.setParameter("projectId", projectId);
			final List<Long> rsmIds = udsQuery.getResultList();

			//TODO : Mutualiser l'appel a cette methode avec fillSEDbIdentifiersBySEDb !!  
			final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);

			if ((seDbInstances == null) || seDbInstances.isEmpty()) {
				LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
				msiTransactionOK = true;
			} else {
				LOG.info(" Filling ProteinMatches properties on project {} for {} rsm", projectId, rsmIds.size());

				msiEM.getTransaction().begin();
				int coveredSequenceLength;

				// for each RSM in data_set.result_summmary_id
				for (Long rsmId : rsmIds) {
					final long start = System.currentTimeMillis();
					
					// get the properties of the RSM to test if update needed
					final ResultSummary rsm = msiEM.find(ResultSummary.class, rsmId);
					JsonParser parser = new JsonParser();//VDS TODO ? Use jackson as for ORM DxxQuant object
					JsonObject array = parser.parse("{}").getAsJsonObject();
					if (!forceUpdate) {
						String properties = rsm.getSerializedProperties();						
						array = null;
						try {
							array = parser.parse(properties).getAsJsonObject();
						} catch (Exception e) {
							if (LOG.isDebugEnabled()) {
								LOG.debug("error accessing project id: " + projectId + " (missing JSON): forcing project work...");
								array = parser.parse("{}").getAsJsonObject();
							}
						}
					}

					int psIdcount = 0;
					
					// test if the RSM is already calculated
					if (!array.has("is_coverage_updated") || forceUpdate) {

						if (LOG.isDebugEnabled()) {
							LOG.debug("Going to compute rsmId:" + rsmId);
						}
						

						// Get ALL SeqMatches For current RSM
						List<SequenceMatch> seqMatches = SequenceMatchRepository.findSequenceMatchForResultSet(msiEM, rsm.getResultSet().getId());
						Map<Long, List<SequenceMatch>> seqMatchesByProteinMatchId = new HashMap<>();
						for (SequenceMatch seqMatch : seqMatches) {
							Long pmId = seqMatch.getId().getProteinMatchId();
							if (!seqMatchesByProteinMatchId.containsKey(pmId))
								seqMatchesByProteinMatchId.put(pmId, new ArrayList<SequenceMatch>());
							seqMatchesByProteinMatchId.get(pmId).add(seqMatch);
						}
												
						// Get all ProteinSet
						final Query psQuery = msiEM.createQuery(LIST_PS_FOR_RSM_QUERY);
						psQuery.setParameter("rsmId", rsmId);
						final List<ProteinSet> protSets = psQuery.getResultList();
						int psIdListSize = protSets.size();						

						//Get All Peptide Ids identified
//						List<Long> peptideIds = new ArrayList<Long>();
						Map<Long, List<Long>> pepIdsByProtSetId = new HashMap<Long, List<Long>>();
						final Query getPepInstQuery = msiEM.createQuery(GET_PEP_INSTANCE_BY_PS_PM_QUERY);
						getPepInstQuery.setParameter("rsmId", rsmId);
						final List<Object[]> resultPepInsts = getPepInstQuery.getResultList();
						for(Object[] nextEntry : resultPepInsts){
							PeptideInstance pi = (PeptideInstance) nextEntry[0];
							Long psId = (Long) nextEntry[1];
							if(!pepIdsByProtSetId.containsKey(psId))
								pepIdsByProtSetId.put(psId, new ArrayList<Long>());
							pepIdsByProtSetId.get(psId).add(pi.getPeptide().getId());
						}
						
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
									getSeqCoverageForProteinMatch(seqMatchesByProteinMatchId, protSet2ProtMatch.getProteinMatch(), pepIdsByProtSetId.get(protSet.getId())));
							}

							// Get bioSequence for all ProteinMatch  				    
							Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider.findBioSequencesBySEDbIdentValues(allProtMatchesAccession);

							// loop into proteins of current protein set.
							for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatchList.entrySet()) {
								ProteinMatch protMatch = entry.getKey();
								coveredSequenceLength = entry.getValue();

								List<BioSequenceWrapper> protMatchBioSeqs = result.get(protMatch.getAccession());
								if ((protMatchBioSeqs == null) || (protMatchBioSeqs.isEmpty())) {
									if (LOG.isDebugEnabled()) {
										LOG.debug(" ****  FOUND NO Sequence for protein {}", protMatch.getAccession());
									}
								} else if (protMatchBioSeqs.size() > 1) {
									if (LOG.isDebugEnabled()) {
										LOG.debug(" ****  FOUND MORE THAN 1 Sequence for protein {}. Use first one  ", protMatch.getAccession());
									}
								}

								if ((protMatchBioSeqs != null) && (protMatchBioSeqs.size() >= 1)) {
									BioSequenceWrapper bioSeq = protMatchBioSeqs.get(0);
									int bioSequenceLentgh = bioSeq.getSequence().length();
									// to avoid the indeterminate form : /0
									if ((bioSequenceLentgh > 0) && (coveredSequenceLength < bioSequenceLentgh)) {

										//Calculate Coverage and store in MSI
										double coverage = calculateSequenceCoverage(bioSequenceLentgh, coveredSequenceLength);
										ProteinSetProteinMatchItem proSetMap = protSetMapByProtMatch.get(protMatch);
										proSetMap.setCoverage(new Float(coverage));
										msiEM.merge(proSetMap);

										// Save BioSequence
										BioSequence msiBioSeq = msiEM.find(BioSequence.class, bioSeq.getSequenceId());
										boolean persist = false;
										if (msiBioSeq == null) {
											msiBioSeq = new BioSequence();
											persist = true;
										}

										msiBioSeq.setAlphabet(Alphabet.AA);
										msiBioSeq.setId(bioSeq.getSequenceId());
										msiBioSeq.setLength(bioSeq.getSequence().length());
										msiBioSeq.setMass(new Double(bioSeq.getMass()).intValue());
										msiBioSeq.setCrc64(HashUtil.calculateCRC64(bioSeq.getSequence()));
										msiBioSeq.setPi(new Float(bioSeq.getPI()));
										msiBioSeq.setSequence(bioSeq.getSequence());
										if (persist)
											msiEM.persist(msiBioSeq);
										else
											msiEM.merge(msiBioSeq);

										// Save link between ProteinMatch and BioSeq
										protMatch.setBioSequenceId(bioSeq.getSequenceId());
										msiEM.merge(protMatch);
									}
								}
							} // end of proteins list of current protein set

							if (psIdcount % 500 == 0) {
								LOG.info("Processed " + psIdcount + " protein sets / " + psIdListSize);
							}
							psIdcount++;
						} // end protein sets

						//Save RSM Property
						if (!array.has("is_coverage_updated")) {
							array.addProperty("is_coverage_updated", true);
							final Query updateQueryprop = msiEM.createNativeQuery(UPDATE_QUERY_RSM);
							updateQueryprop.setParameter("sr", array.toString());
							updateQueryprop.setParameter("rsmId", rsmId);
							updateQueryprop.executeUpdate();
						}

					}

					final long end = System.currentTimeMillis();
					final long duration = end - start;
					LOG.info("rsmId: {} successfully/already calculated. Duration : {} ms for {} protein sets ", rsmId, duration, psIdcount);

				} //End go through RSMs
				
				msiEM.getTransaction().commit();
				msiTransactionOK = true;
			} //At least One SEdb

			final long endAll = System.currentTimeMillis();
			final long duration = endAll - startAll;
			LOG.info("Total: fillProteinMatchesProperties() execution : {} ms for project {} ", duration, projectId);

		} catch (Exception ex) {
			LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			try {
				if (!msiTransactionOK)
					msiEM.getTransaction().rollback();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
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
	}

	private static Integer getSeqCoverageForProteinMatch(Map<Long, List<SequenceMatch>> seqMatchesByProteinMatchId, final ProteinMatch protMatch, List<Long> peptideIds) {

		// variables definition
		HashSet<Integer> coveredAASet = new HashSet<Integer>();//Set of protein sequence index covered by PeptideMatch	
		List<SequenceMatch> seqMatches = seqMatchesByProteinMatchId.get(protMatch.getId());

		for (SequenceMatch seqMatch : seqMatches) {
			Long pepId = seqMatch.getId().getPeptideId();
			if(peptideIds.contains(pepId)){
				SequenceMatchPK seqMatchKey = seqMatch.getId();
				int start = seqMatchKey.getStart();
				int stop = seqMatchKey.getStop();
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
