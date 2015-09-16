package fr.proline.module.seq.service;

import java.math.BigInteger;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Types;
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

import org.hibernate.hql.internal.ast.tree.Statement;
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
import fr.proline.repository.DatabaseConnectorFactory;
import fr.proline.repository.DatabaseUpgrader;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;
import fr.proline.repository.ProlineDatabaseType;

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
			+"AND ps.isValidated = 'true' AND pspmm.id.proteinSetId=:psId AND pspmm.id.proteinMatchId=:pmId AND ps.resultSummary.id=:rsmId ";

	private static final String CALCULATED_RSM = "FROM fr.proline.core.orm.msi.ResultSummary where id=:rsmId";

	private static final String UPDATE_QUERY_RSM = "UPDATE result_summary  set serialized_properties=:sr where id=:rsmId";

	private static final String LIST_RSM = "select distinct(dt.resultSummaryId) from Dataset dt where dt.project.id= :projectId and dt.type='IDENTIFICATION' and dt.resultSummaryId IS NOT NULL";
	private static final String LIST_PS = "select distinct(ps.id) from ProteinSetProteinMatchItem pspm, ProteinSet ps where ps.id = pspm.id.proteinSetId and pspm.resultSummary.id= :rsmId and ps.isValidated = 'true' and pspm.resultSummary.id IS NOT NULL";
	private static final String LIST_PSM = "select distinct(pspm.proteinMatch.id) from ProteinSetProteinMatchItem pspm, ProteinSet ps where ps.id = pspm.id.proteinSetId and pspm.resultSummary.id= :rsmId and pspm.id.proteinSetId= :psId and ps.isValidated = 'true' and pspm.resultSummary.id IS NOT NULL";
	private static final String CREATE_UPDATE_FUNCTION = "CREATE OR REPLACE FUNCTION updatepspmitem(the_id integer, the_value real,ps_id integer,rsm_id integer)  RETURNS integer AS $$ \n BEGIN \n  "
			+" UPDATE protein_set_protein_match_item SET coverage= the_value WHERE protein_match_id =the_id and protein_set_id=ps_id and result_summary_id=rsm_id;\n IF FOUND THEN\n RETURN 0;\n  END IF; RETURN 1; \n "
			+" END; \n $$ LANGUAGE plpgsql;\n";
	private static final String CREATE_INSERT_FUNCTION = "CREATE OR REPLACE FUNCTION upsertmw(nid bigint,nalphabet character,nsequence text ,nlength integer,nmass real, npi real,ncrc64 character,nserialized_properties text) RETURNS VOID AS $$ \n BEGIN \n LOOP \n "
			+" UPDATE bio_sequence SET alphabet=nalphabet,sequence =nsequence ,length =nlength,mass=nmass, pi =npi,crc64=ncrc64,serialized_properties=nserialized_properties where id =nid ;\n IF found THEN  RETURN;\n END IF; BEGIN \n" 
			+" INSERT INTO bio_sequence VALUES (nid,nalphabet,nsequence ,nlength,nmass, npi,ncrc64,nserialized_properties); \n RETURN; \n EXCEPTION WHEN unique_violation THEN \n END; "
			+" END LOOP;\n END; \n $$ LANGUAGE plpgsql;\n";
	private static final String CREATE_UPDATE_PM_FUNCTION = "CREATE OR REPLACE FUNCTION updatepm(pm_id integer,bio_id bigint) RETURNS integer AS $$ \n BEGIN \n   "
			+" UPDATE protein_match SET bio_sequence_id= bio_id WHERE id =pm_id;\n IF FOUND THEN\n RETURN 0;\n  END IF;RETURN 1; \n "
			+" END; \n $$ LANGUAGE plpgsql;\n";

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

	public static void fillSequenceMatchesByProteinMatch(final long projectId, boolean force_update) {

		final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
		final IDatabaseConnector dbConnector = connectorFactory.getMsiDbConnector(projectId);
		DatabaseUpgrader.upgradeDatabase(dbConnector);
		Connection con = null;
		try {
			con = dbConnector.getDataSource().getConnection();
		} catch (SQLException e) {

			e.printStackTrace();
		}
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatch = new HashMap<ProteinMatch, Integer>();
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList = new HashMap<ProteinMatch, Integer>();

		int sequencesmatcheslength;
		Long proteinmatchid = null;


		if (msiDbConnector == null) {
			LOG.warn("Project #{} has NO associated MSI Db", projectId);
		} else {
			EntityManager msiEM = null;
			EntityManager udsEM = null;
			try {

				final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
				msiEM = emf.createEntityManager();
				final EntityManagerFactory em = udsDbConnector.getEntityManagerFactory();	
				udsEM = em.createEntityManager();
				//create the function to update  
				java.sql.Statement stmt = con.createStatement();
				stmt.execute(CREATE_UPDATE_FUNCTION);
				stmt.execute(CREATE_INSERT_FUNCTION);
				stmt.execute(CREATE_UPDATE_PM_FUNCTION);
				//get the list of RSM
				final Query udsQuery = udsEM.createQuery(LIST_RSM);
				udsQuery.setParameter("projectId",projectId);
				final List<Long> rsmIds = udsQuery.getResultList();
				final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);
				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
				} else {
					//for each RSM in data_set.result_summmary_id

					for (Long rsmId : rsmIds) {
						int maxQueryCount = 1000,queryCountupdate =0,biosequencelentgh=0;// the max number of query to update
						Long biosequenceId=null;
						Double biosequencePi=null,biosequenceMass=null;
						String bioSequence = null;

						//get the properties of the RSM to update
						final TypedQuery<ResultSummary> rsms = msiEM.createQuery(CALCULATED_RSM, ResultSummary.class);
						rsms.setParameter("rsmId", rsmId);
						String properties = rsms.getResultList().get(0).getSerializedProperties();
						JsonParser parser = new JsonParser();
						Gson gson = new Gson();
						JsonObject array = parser.parse(properties).getAsJsonObject();

						//test if the RSM is already calculated
						if(!array.has("is_coverage_updated") || force_update){ 
							LOG.info("going to compute rsmId:" + rsmId);
							final Query psQuery = msiEM.createQuery(LIST_PS);
							psQuery.setParameter("rsmId",rsmId);
							final List<Long> psIds = psQuery.getResultList();
							int psIdListSize = psIds.size();
							int psIdcount=0;
							LOG.info("start processing of "+psIdListSize+" proteinsets.");
							for(Long psId : psIds)
							{
								coveredSeqLengthByProtMatchList.clear();
								final Query pmQuery = msiEM.createQuery(LIST_PSM);
								pmQuery.setParameter("rsmId", rsmId);
								pmQuery.setParameter("psId",psId);
								final List<Long> pmIds = pmQuery.getResultList();
								for(Long pmId : pmIds){
									coveredSeqLengthByProtMatch.clear();
									final Query pmAccQuery = msiEM.createQuery(VALIDATED_ACC_RSM_QUERY);
									pmAccQuery.setParameter("psId",psId);
									pmAccQuery.setParameter("pmId",pmId);
									pmAccQuery.setParameter("rsmId",rsmId);
									final List<Object[]> pmSdmLines = pmAccQuery.getResultList();
									if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
										//coveredSeqLengthByProtMatch :get for protein_match_id -the number of AA 
										coveredSeqLengthByProtMatch = fillProteinMatch(pmSdmLines);
										for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatch.entrySet()) {
											coveredSeqLengthByProtMatchList.put(entry.getKey(), entry.getValue());
										}
									}
								}
								List<String> accessionList = new ArrayList<>();
								String updateQuery="select ",insertQuery="select ",updatePmQuery="select ";

								//search for each accession its bio_sequence
								int queryLength =0;

								for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatchList.entrySet()) {
									queryCountupdate++;queryLength++;
									accessionList.add(entry.getKey().getAccession());
									sequencesmatcheslength = entry.getValue();
									Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider.findBioSequencesBySEDbIdentValues(accessionList);
									for (Map.Entry<String, List<BioSequenceWrapper>> entry0 : result.entrySet()) {
										List<BioSequenceWrapper> bioSequences = entry0.getValue();
										for (BioSequenceWrapper bsw : bioSequences) {
											biosequencelentgh = bsw.getSequence().length();
											biosequenceId=bsw.getSequenceId();
											bioSequence=bsw.getSequence();
											biosequencePi=bsw.getPI();
											biosequenceMass=bsw.getMass();
										}	
										proteinmatchid = entry.getKey().getId();


									}
									//to avoid the indeterminate form : /0
									if((biosequencelentgh>0) && (sequencesmatcheslength<biosequencelentgh))
									{
										int querySize = coveredSeqLengthByProtMatchList.keySet().size();
										//call the function loop_merge to execute 1000 update in one command : loop_merge(proteinmatch_id,coverage_value,Rsm_id) 
										if(queryCountupdate==querySize || queryCountupdate==maxQueryCount || queryLength==querySize)
										{
											updateQuery+=" updatepspmitem("+proteinmatchid+","+calculateSequenceCoverage(biosequencelentgh, sequencesmatcheslength)+","+psId+","+rsmId+"); ";
											insertQuery+=" upsertmw("+biosequenceId+",'aa','"+bioSequence+"',"+biosequencelentgh+","+biosequenceMass+","+biosequencePi+",'',''); ";
											updatePmQuery+=" updatepm("+proteinmatchid+","+biosequenceId+"); ";
											stmt = con.createStatement();
											stmt.execute(updateQuery);
											stmt.execute(insertQuery);
											stmt.execute(updatePmQuery);
											if(psIdcount% 500 ==0) 
											{
												LOG.info("Process "+psIdcount+" protein sets / " + psIdListSize);

											}
											updateQuery="select ";
											insertQuery="select ";
											updatePmQuery="select ";
											queryCountupdate=0;
										}
										else{
											updateQuery+=" updatepspmitem("+proteinmatchid+","+calculateSequenceCoverage(biosequencelentgh, sequencesmatcheslength)+","+psId+","+rsmId+") , ";
											insertQuery+=" upsertmw("+biosequenceId+",'aa','"+bioSequence+"',"+biosequencelentgh+","+biosequenceMass+","+biosequencePi+",'','') , ";
											updatePmQuery+=" updatepm("+proteinmatchid+","+biosequenceId+") , ";
										}


									}
									accessionList.clear();
								}
								if("select ".equals(updateQuery)||"select ".equals(insertQuery) ||"select ".equals(updatePmQuery)) { // it means we finished before the maxQuuery count
									// do nothing
								} else {
									stmt = con.createStatement();
									int stUpdateLength = updateQuery.length(),stInsertLength=insertQuery.length(),stUpdatePmLength=updatePmQuery.length();
									if(updateQuery.substring(stUpdateLength-2).equals(", ")) {
										updateQuery = updateQuery.substring(0, stUpdateLength-3);
									}
									if(insertQuery.substring(stInsertLength-2).equals(", ")) {
										insertQuery = insertQuery.substring(0, stInsertLength-3);
									}
									if(updatePmQuery.substring(stUpdatePmLength-2).equals(", ")) {
										updatePmQuery = updatePmQuery.substring(0, stUpdatePmLength-3);
									}
								}
								updateQuery="";insertQuery="";updatePmQuery="";
								psIdcount++;
							}
							if(!array.has("is_coverage_updated")) {
								array.addProperty("is_coverage_updated", true);
								msiEM.getTransaction().begin();
								final Query updateQueryprop = msiEM.createNativeQuery(UPDATE_QUERY_RSM); 
								updateQueryprop.setParameter("sr",array.toString());
								updateQueryprop.setParameter("rsmId",rsmId);
								updateQueryprop.executeUpdate();
								msiEM.getTransaction().commit();
							}
							LOG.info("rsm " + rsmId + " successfully calculated");
						} else {
							LOG.info("rsmId: "+rsmId+" already calculated");
						}
					    con.setAutoCommit(false);
						con.commit();
					}
				}
			} catch (Exception ex) {
				LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			} finally {
				if (msiEM != null) {
					try {
						con.close();
						msiEM.close();
						udsEM.close();
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
			if(nbrCoveredAAPerProMatch.containsKey(proteinMatch)){
				sequencematchLists=temp.get(proteinMatch);
			}
			else{
				sequencematchLists.clear();
			} //init index list using previously saved indexes for this Prot Match
			sequencematchLists.addAll(getSequencesIndexes(start,stop));		//Add new indexes 
			temp.put(proteinMatch,sequencematchLists);						//save final list for next SeqMatch
			coveredAAIndexList.addAll(temp.get(proteinMatch));				//add all indexes
			coveredAASet.addAll(coveredAAIndexList);						//use set to remove duplicate indexes 
			nbrCoveredAAPerProMatch.put(proteinMatch,coveredAASet.size()); // save size of covered AA for ProteinMatch
			//proteinMatchDuplicated.add(proteinMatch);
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
