package fr.proline.module.seq.service;

import java.math.BigInteger;
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

import fr.profi.util.StringUtils;
import fr.proline.core.orm.msi.ProteinMatch;
import fr.proline.core.orm.msi.ProteinSetProteinMatchItem;
import fr.proline.core.orm.msi.ResultSummary;
import fr.proline.core.orm.msi.SeqDatabase;
import fr.proline.core.orm.msi.SequenceMatchPK;
import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

public class ProjectHandler {

	private static final Logger LOG = LoggerFactory.getLogger(ProjectHandler.class);

	private static final String ALL_SEQ_DB_QUERY = "FROM fr.proline.core.orm.msi.SeqDatabase";

	private static final String VALIDATED_PM_COUNT_QUERY = "SELECT COUNT (DISTINCT pm.accession)"
			+ " FROM fr.proline.core.orm.msi.ProteinMatch pm JOIN pm.proteinSetProteinMatchItems ps"
			+ " WHERE ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
			+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary IS NOT NULL))";

	private static final String VALIDATED_PM_SDM_QUERY = "SELECT DISTINCT pm.accession, pm.description, sdb.id"
			+ " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SeqDatabase sdb, fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap pmsdb"
			+ " JOIN pm.proteinSetProteinMatchItems ps"
			+ " WHERE (pmsdb.id.proteinMatchId = pm.id) AND (pmsdb.id.seqDatabaseId = sdb.id)"
			+ " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
			+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary IS NOT NULL)";

	private static final String VALIDATED_PM_QUERY = "SELECT DISTINCT pm.accession, pm.description, ssdm.seqDatabase.id"
			+ " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SearchSettingsSeqDatabaseMap ssdm"
			+ " JOIN pm.proteinSetProteinMatchItems ps"
			+ " WHERE (pm.resultSet.msiSearch.searchSetting = ssdm.searchSetting)"
			+ " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
			+ " AND (ps.proteinSet.isValidated = true) AND (ps.proteinSet.resultSummary IS NOT NULL)";

	private static final String VALIDATED_ACC_RSM_QUERY = "SELECT pm, sm.id "
        	+"FROM ProteinMatch pm, ProteinSet ps, ProteinSetProteinMatchItem pspmm, SequenceMatch sm, PeptideSet pepset, PeptideSetPeptideInstanceItem pepsetinsitem, PeptideInstance pepinst "
        	+"WHERE pm.id = pspmm.id.proteinMatchId AND ps.id = pspmm.id.proteinSetId AND sm.id.proteinMatchId = pm.id AND sm.id.peptideId = pepinst.peptide.id AND pepset.proteinSet.id = ps.id AND "
        	+"pepsetinsitem.peptideSet.id=pepset.id AND pepsetinsitem.peptideInstance.id=pepinst.id "
        	+"AND ps.isValidated = 'true' AND ps.resultSummary.id=?";
	private static final String CALCULATED_RSM = "FROM fr.proline.core.orm.msi.ResultSummary where id=?";
	private static final String UPDATE_QUERY_RSM = "UPDATE result_summary  set serialized_properties=? where id=?";
	private static final String LIST_RSM = "select distinct(dt.resultSummaryId) from Dataset dt where dt.project.id=? and dt.resultSummaryId is not null";
	private static final String UPDATE_QUERY_PSPMI = "UPDATE protein_set_protein_match_item  set coverage=? where protein_match_id=? and result_summary_id=?";

	private static final int EXPECTED_LINE_LENGTH = 3;

	/* In this version : find all SEDbIdentifiers in all SEDbInstances */
	public static void fillSEDbIdentifiersBySEDb(final long projectId,
			final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers) {

		if (seDbIdentifiers == null) {
			throw new IllegalArgumentException("SeDbIdentifiers Map is null");
		}

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);

		if (msiDbConnector == null) {
			LOG.warn("Project #{} has NO associated MSI Db", projectId);
		} else {
			EntityManager msiEM = null;

			try {

				final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
				msiEM = emf.createEntityManager();

				final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);

				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
				} else {
					long nExpectedAccessions = -1L;

					final Query countQuery = msiEM.createQuery(VALIDATED_PM_COUNT_QUERY);

					final Object obj = countQuery.getSingleResult();

					if (obj instanceof Number) {
						nExpectedAccessions = ((Number) obj).longValue();
					}

					if (LOG.isDebugEnabled()) {
						LOG.debug(
								"MSI Project #{} found {} SEDbInstances and {} validated Accession",
								projectId, seDbInstances.size(), nExpectedAccessions);
					}

					if (nExpectedAccessions > 0L) {
						int nSEDbIdentifiers = 0;

						final Query pmSdmQuery = msiEM.createQuery(VALIDATED_PM_SDM_QUERY);

						final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();

						if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
							nSEDbIdentifiers = fillSEDbIdentifiers(pmSdmLines, seDbInstances,
									seDbIdentifiers);
						}
						
						if (nSEDbIdentifiers >= nExpectedAccessions) {
							LOG.debug(
									"{} distinct (validated Accession, Description, SeqDatabase) retrieved via ProteinMatchSeqDatabaseMap",
									nSEDbIdentifiers);
						} else {
							nSEDbIdentifiers = 0;

							final Query pmQuery = msiEM.createQuery(VALIDATED_PM_QUERY);

							final List<Object[]> pmLines = pmQuery.getResultList();

							if ((pmLines != null) && !pmLines.isEmpty()) {
								nSEDbIdentifiers = fillSEDbIdentifiers(pmLines, seDbInstances,
										seDbIdentifiers);
							}
							LOG.info(
									"{} distinct (validated Accession, Description, SeqDatabase) WITHOUT ProteinMatchSeqDatabaseMap",
									nSEDbIdentifiers);
						}
					} else {
						LOG.warn("There is NO validated Accession in MSI Project #{}", projectId);
					}
				} // End if (seDbInstances is not empty)
			} catch (Exception ex) {
				LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			} finally {
				if (msiEM != null) {
					try {
						LOG.debug(" CLOSE MSI Db EntityManager for project "+projectId);
						msiEM.close();
					} catch (Exception exClose) {
						LOG.error("Error closing MSI Db EntityManager", exClose);
					}
				}
			}
		}
	}

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
					final String trimmedName = name.trim(); // SEDb name should be trimmed

					if (trimmedName.isEmpty()) {
						LOG.error("SeqDb #{} name is empty", seqDbId);
						
					} else {
						final String fastaFilePath = seqDb.getFastaFilePath();

						if (StringUtils.isEmpty(fastaFilePath)) {
							LOG.error("SeqDb #{} fastaFilePath is empty", seqDbId);
						} else {
							final SEDbInstanceWrapper seDbInstanceW = new SEDbInstanceWrapper(trimmedName, null, fastaFilePath);
							result.put(Long.valueOf(seqDbId), seDbInstanceW);
						} // End if (fastaFilePath is valid)

					} // End if (trimmedName is valid)

				} // End if (name is not null)

			} // End loop for each seqDb

		} // End if (seqDbs List is not empty)

		return result;
	}

	private static int fillSEDbIdentifiers(final List<Object[]> lines,
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
					value = ((String) line[0]).trim(); // SEDbIdent should be trimmed
				}
				if (line[1] instanceof String) {
					description = ((String) line[1]).trim(); // Description should be trimmed
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

						++nIdentifiers; // Found a valid SEDbIdentifier
					}

				} // End if (identValue is valid)

			} else {
				LOG.error("Invalid result line length {} (expected : {})", lineLength,
						EXPECTED_LINE_LENGTH);
			}
		} // End loop for each Result line
		return nIdentifiers;
	}

	// In this version : find all sequence coverage for each protein match via
	// the projectId and store it in msi db

	public static void fillSequenceMatchesByProteinMatch(final long projectId) {

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatch = new HashMap<ProteinMatch, Integer>();
		Map<ProteinMatch, Integer> allCoveredSeqLengthByProtMatch = new HashMap<ProteinMatch, Integer>();
		int sequencesmatcheslength;
		Long proteinmatchid;
		if (msiDbConnector == null) {
			LOG.warn("Project #{} has NO associated MSI Db", projectId);
		} else {
			EntityManager msiEM = null;
			try {
				final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
				msiEM = emf.createEntityManager();
				EntityManager udsEM = null;
				final EntityManagerFactory em = udsDbConnector.getEntityManagerFactory();
				udsEM = em.createEntityManager();
				final Query udsQuery = udsEM.createQuery(LIST_RSM);
				udsQuery.setParameter(1,projectId);
				final List<Long> rsmIds = udsQuery.getResultList();
				final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);
				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
				} else {
					//for the last RSMs in data_set
					for (Long rsmId : rsmIds) {
						final TypedQuery<ResultSummary> rsms = msiEM.createQuery(CALCULATED_RSM, ResultSummary.class);
						rsms.setParameter(1, rsmId);
						String properties = rsms.getResultList().get(0).getSerializedProperties();
						JsonParser parser = new JsonParser();
			            Gson gson = new Gson();
						JsonObject array = parser.parse(properties).getAsJsonObject();
						//check if RSM is already calculated
						if(!array.has("is_calculated")){
							array.addProperty("is_calculated", true);
							msiEM.getTransaction().begin();
							//update serialized properties of RSM
							final Query updateQueryprop = msiEM.createNativeQuery(UPDATE_QUERY_RSM); 
							updateQueryprop.setParameter(1,array.toString());
							updateQueryprop.setParameter(2,rsmId);
							updateQueryprop.executeUpdate();
							msiEM.getTransaction().commit();
							//compute sequence Coverage
							final Query pmSdmQuery = msiEM.createQuery(VALIDATED_ACC_RSM_QUERY);
							pmSdmQuery.setParameter(1,rsmId);
							final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();
							if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
								coveredSeqLengthByProtMatch = fillProteinMatch(pmSdmLines);
								int biosequencelentgh = 0;
								List<String> accession = new ArrayList<>();
								for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatch.entrySet()) {
									accession.add(entry.getKey().getAccession());
									sequencesmatcheslength = entry.getValue();
									Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider.findBioSequencesBySEDbIdentValues(accession);
								for (Map.Entry<String, List<BioSequenceWrapper>> entry0 : result.entrySet()) {
									List<BioSequenceWrapper> bioSequences = entry0.getValue();
									for (BioSequenceWrapper bsw : bioSequences) {
										biosequencelentgh = bsw.getSequence().length();}
									proteinmatchid = entry.getKey().getId();
									//update protein_set_protein_match_item.coverage
									msiEM.getTransaction().begin();
									final Query updateQuery = msiEM.createNativeQuery(UPDATE_QUERY_PSPMI); 
									updateQuery.setParameter(1,calculateSequenceCoverage(biosequencelentgh, sequencesmatcheslength));
									updateQuery.setParameter(2, proteinmatchid);
									updateQuery.setParameter(3,rsmId);
									updateQuery.executeUpdate();
									msiEM.getTransaction().commit();
								}
								accession.clear();
							}
								}
						}
					}
				}
			} catch (Exception ex) {
				LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			} finally {
				if (msiEM != null) {
					try {
						msiEM.close();
					} catch (Exception exClose) {
						LOG.error("Error closing MSI Db EntityManager", exClose);
					}
				}
			}
		}
	}

	private static Map<ProteinMatch, Integer> fillProteinMatch(final List<Object[]> lines) {
		
		//variables definition
		Map<ProteinMatch, Integer> nbrCoveredAAPerProMatch = new HashMap<ProteinMatch, Integer>();
		HashSet<Integer> coveredAASet = new HashSet<Integer>();
		Map<ProteinMatch, List<Integer>> temp = new HashMap<ProteinMatch, List<Integer>>();
		List<ProteinMatch> proteinMatchDuplicated = new ArrayList<>();
		List<Integer> coveredAAIndexList = new ArrayList<Integer>();
		List<Integer> sequencematchLists = new ArrayList<Integer>();
		ProteinMatch proteinMatch = null;
		SequenceMatchPK sequenceMatch = null;

		for (final Object[] line : lines) {
			int start = 0;
			int stop = 0;

			if (line[0] instanceof ProteinMatch) {
				proteinMatch = (ProteinMatch) line[0];
			}
			if (line[1] instanceof SequenceMatchPK) {
				sequenceMatch = (SequenceMatchPK) line[1];
			}
			start = (int) sequenceMatch.getStart();
			stop = (int) sequenceMatch.getStop();
			coveredAASet.clear();
			coveredAAIndexList.clear();
			if(proteinMatchDuplicated.contains(proteinMatch)){sequencematchLists=temp.get(proteinMatch);}else{sequencematchLists.clear();} //init index list using previously saved indexes for this Prot Match
				sequencematchLists.addAll(getSequencesIndexes(start,stop));		//Add new indexes 
				temp.put(proteinMatch,sequencematchLists);						//save final list for next SeqMatch
				coveredAAIndexList.addAll(temp.get(proteinMatch));				//add all indexes
				coveredAASet.addAll(coveredAAIndexList);						//use set to remove duplicate indexes 
				nbrCoveredAAPerProMatch.put(proteinMatch,coveredAASet.size()); // save size of covered AA for ProteinMatch
			proteinMatchDuplicated.add(proteinMatch);
		}
		return nbrCoveredAAPerProMatch;
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