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

//	private static final String VALIDATED_ACC_RSM_QUERY = "SELECT pm, sm.id "
//			+ "FROM ProteinMatch pm, ProteinSet ps, ProteinSetProteinMatchItem pspmm, SequenceMatch sm, PeptideSet pepset, PeptideSetPeptideInstanceItem pepsetinsitem, PeptideInstance pepinst "
//			+ "WHERE pm.id = pspmm.id.proteinMatchId AND ps.id = pspmm.id.proteinSetId AND sm.id.proteinMatchId = pm.id AND sm.id.peptideId = pepinst.peptide.id AND pepset.proteinSet.id = ps.id AND "
//			+ "pepsetinsitem.peptideSet.id=pepset.id AND pepsetinsitem.peptideInstance.id=pepinst.id "
//			+ "AND ps.isValidated = 'true' AND pspmm.id.proteinSetId=:psId AND pspmm.id.proteinMatchId=:pmId AND ps.resultSummary.id=:rsmId ";


	private static final String UPDATE_QUERY_RSM = "UPDATE result_summary  set serialized_properties=:sr where id=:rsmId";


	private static final String LIST_RSM_IN_DATASET_ID_QUERY = "SELECT DISTINCT(dt.resultSummaryId) FROM Dataset dt WHERE dt.project.id= :projectId AND dt.type IN ('AGGREGATE','IDENTIFICATION') AND dt.resultSummaryId IS NOT NULL";

	private static final String LIST_PS_FOR_RSM_QUERY = "SELECT ps FROM ProteinSet ps WHERE ps.resultSummary.id= :rsmId AND ps.isValidated = 'true'";
	
//	private static final String LIST_PM_FOR_PS_QUERY = "SELECT distinct(pspm.proteinMatch.id) FROM ProteinSetProteinMatchItem pspm, ProteinSet ps WHERE ps.id = pspm.id.proteinSetId AND pspm.resultSummary.id= :rsmId and pspm.id.proteinSetId= :psId and ps.isValidated = 'true'";
//	
//	private static final String CREATE_UPDATE_FUNCTION = "CREATE OR REPLACE FUNCTION updatepspmitem(the_id integer, the_value real,ps_id integer,rsm_id integer)  RETURNS integer AS $$ \n BEGIN \n  "
//			+ " UPDATE protein_set_protein_match_item SET coverage= the_value WHERE protein_match_id =the_id and protein_set_id=ps_id and result_summary_id=rsm_id;\n IF FOUND THEN\n RETURN 0;\n  END IF; RETURN 1; \n "
//			+ " END; \n $$ LANGUAGE plpgsql;\n";
//	private static final String CREATE_INSERT_FUNCTION = "CREATE OR REPLACE FUNCTION upsertmw(nid bigint,nalphabet character,nsequence text ,nlength integer,nmass real, npi real,ncrc64 character,nserialized_properties text) RETURNS VOID AS $$ \n BEGIN \n LOOP \n "
//			+ " UPDATE bio_sequence SET alphabet=nalphabet,sequence =nsequence ,length =nlength,mass=nmass, pi =npi,crc64=ncrc64,serialized_properties=nserialized_properties where id =nid ;\n IF found THEN  RETURN;\n END IF; BEGIN \n"
//			+ " INSERT INTO bio_sequence VALUES (nid,nalphabet,nsequence ,nlength,nmass, npi,ncrc64,nserialized_properties); \n RETURN; \n EXCEPTION WHEN unique_violation THEN \n END; "
//			+ " END LOOP;\n END; \n $$ LANGUAGE plpgsql;\n";
//	private static final String CREATE_UPDATE_PM_FUNCTION = "CREATE OR REPLACE FUNCTION updatepm(pm_id integer,bio_id bigint) RETURNS integer AS $$ \n BEGIN \n   "
//			+ " UPDATE protein_match SET bio_sequence_id= bio_id WHERE id =pm_id;\n IF FOUND THEN\n RETURN 0;\n  END IF;RETURN 1; \n "
//			+ " END; \n $$ LANGUAGE plpgsql;\n";

	private static final int EXPECTED_LINE_LENGTH = 3;

	/**
	 * Find all Search Engine protein identifier in all Search Engine protein databases of a specific MSIdb speficied by it's projectId.
	 * 
	 * @param projectId
	 * @param seDbIdentifiersBySeDbInstance
	 * @param forceUpdate
	 */
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
			try {

				final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
				msiEM = emf.createEntityManager();
				final EntityManagerFactory em_uds = udsDbConnector.getEntityManagerFactory();
				udsEM = em_uds.createEntityManager();
				final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);

				if ((seDbInstances == null) || seDbInstances.isEmpty()) {
					LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
					
				} else {
					// check if any rsm has not been validated within this project

				    final Query udsQuery = udsEM.createQuery(LIST_RSM_IN_DATASET_ID_QUERY);
				    udsQuery.setParameter("projectId", projectId);
				    final List<Long> rsmIds = udsQuery.getResultList();
				    final List<Long> untreatedRsmIds = new ArrayList<Long>();
				    if(forceUpdate)
					untreatedRsmIds.addAll(rsmIds);
				    else{
                			for (Long rsmId : rsmIds) {
                
                			    // get the properties of the RSM to update
                			    ResultSummary rsm = msiEM.find(ResultSummary.class, rsmId);
                			    String properties = rsm.getSerializedProperties();
                			    JsonParser parser = new JsonParser();
                			    JsonObject array = null;
                			    try {
                				array = parser.parse(properties).getAsJsonObject();
                			    } catch (Exception e) {
                				LOG.warn("error accessing RSM properties for rsm id:" + rsmId + " in project " + projectId + " forcing retrieve ");
                				array = parser.parse("{}").getAsJsonObject();
                			    }
                
                			    // test if the RSM is already calculated
                
                			    if (!array.has("is_coverage_updated")) {
                				untreatedRsmIds.add(rsmId);
                			    }
                			} // end rsmIds loop
                		    }

				    if (untreatedRsmIds.size() > 0 || forceUpdate) {
					if (LOG.isDebugEnabled()) {
					    if (forceUpdate) {
						LOG.debug(" Update all RSMs for this project ({})",projectId);
					    } else {
						LOG.debug(" Update {} RSM(s) for this project ({})  ", untreatedRsmIds.size(), projectId);
					    }					
					}
					

					// VDS - TODO : Use outer join query to get all PM even if not directly linked to SeqDb.... Then treat separatly ! 
					// VDS - TODO 2 : Use HQM Query with "Select new MyObj( pm.acc, ...)  " for  VALIDATED_PM_SDM_FOR_RSMS_QUERY  VALIDATED_PM_FOR_RSMS_QUERY
					//-- Get ALL ProteinMatch count associated to untreatedRsm
					long nExpectedAccessions = -1L;

					final Query countQuery = msiEM.createQuery(VALIDATED_PM_COUNT_FOR_RSMS_QUERY);
					countQuery.setParameter("rsm_ids", untreatedRsmIds);
					final Object obj = countQuery.getSingleResult();
					
					if (obj instanceof Number) {
					    nExpectedAccessions = ((Number) obj).longValue();
					}

					if (LOG.isDebugEnabled()) {
					    LOG.debug("MSI Project #{} found {} SEDbInstances and {} validated Accession", projectId, seDbInstances.size(), nExpectedAccessions);
					}

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
						    LOG.debug("{} distinct (validated Accession, Description, SeqDatabase) retrieved via ProteinMatchSeqDatabaseMap",nSEDbIdentifiers);
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
						
						LOG.info("{} distinct (validated Accession, Description, SeqDatabase) WITHOUT ProteinMatchSeqDatabaseMap", nSEDbIdentifiers);
					    }

					} else {
					    LOG.warn("There is NO new validated Accession in MSI Project #{}", projectId);
					}

				    } // at least one rsm not yet treated

				} // sedb instances not null
				
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
	 * Fills the seDbIdentifiers map (mapped by search engine database instance) with a list of protein identifiers. The list of protein identifiers is read from the
	 * specified list of Object arrays.
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
					value = ((String) line[0]).trim(); // SEDbIdent should be trimmed
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
	 * Find all sequence coverage for each protein match via the projectId and store it in msidb.
	 * 
	 * @param projectId
	 * @param forceUpdate
	 */
	public static void fillSequenceMatchesByProteinMatch(final long projectId, boolean forceUpdate) {

	    	final IDataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
		final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
		final IDatabaseConnector udsDbConnector = connectorFactory.getUdsDbConnector();
//		Connection con = null;
//		try {
//		    con = msiDbConnector.getDataSource().getConnection();
//		} catch (SQLException e) {
//		    e.printStackTrace();
//		}
		
//		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatch = new HashMap<ProteinMatch, Integer>();
		Map<ProteinMatch, Integer> coveredSeqLengthByProtMatchList = new HashMap<ProteinMatch, Integer>();

		int sequencesmatcheslength;

		EntityManager msiEM = null;
		EntityManager udsEM = null;
		boolean msiTransactionOK = false;
		
		try {
		    
		    final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
		    msiEM = emf.createEntityManager();
		    final EntityManagerFactory em = udsDbConnector.getEntityManagerFactory();
		    udsEM = em.createEntityManager();
			// create the function to update
//			java.sql.Statement stmt = con.createStatement();
//			con.setAutoCommit(false);
//			stmt.execute(CREATE_UPDATE_FUNCTION);
//			stmt.execute(CREATE_INSERT_FUNCTION);
//			stmt.execute(CREATE_UPDATE_PM_FUNCTION);
			// get the list of RSM
		    final Query udsQuery = udsEM.createQuery(LIST_RSM_IN_DATASET_ID_QUERY);
		    udsQuery.setParameter("projectId", projectId);
		    final List<Long> rsmIds = udsQuery.getResultList();
		    
		    //TODO : Mutualiser l'appel a cette methode avec fillSEDbIdentifiersBySEDb !!  
		    final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);
		    
		    if ((seDbInstances == null) || seDbInstances.isEmpty()) {
			LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
		    } else {
				LOG.info("start transaction on project {} for {} rsm", projectId, rsmIds.size());
			msiEM.getTransaction().begin();
			// for each RSM in data_set.result_summmary_id
			for (Long rsmId : rsmIds) {
//			    int peptideMatchBatchSize = 1000;
//			    int nbPeptideMatchTreatedInCurrentRSM = 0;

//			    Long biosequenceId = null;
//			    Double biosequencePi = null, biosequenceMass = null;
//			    String bioSequence = null;

			    // get the properties of the RSM to update
			    final ResultSummary rsm = msiEM.find(ResultSummary.class,rsmId);
			    String properties = rsm.getSerializedProperties();
			    JsonParser parser = new JsonParser(); //VDS TODO ? Use jackson as for ORM DxxQuant object 
			    JsonObject array = null;
			    try {
				array = parser.parse(properties).getAsJsonObject();
			    } catch (Exception e) {
				LOG.warn("error accessing project id: " + projectId + " (missing JSON): forcing project work...");
				array = parser.parse("{}").getAsJsonObject();
			    }
			    
			    // Get SeqMatche For RSM
			    List<SequenceMatch> seqMatches = SequenceMatchRepository.findSequenceMatchForResultSet(msiEM, rsm.getResultSet().getId());
			    Map<Long, List<SequenceMatch>> seqMatchesByProteinMatchId = new HashMap<>();
			    for(SequenceMatch seqMatch : seqMatches){
				Long pmId =  seqMatch.getId().getProteinMatchId();
				if(!seqMatchesByProteinMatchId.containsKey(pmId))
				    seqMatchesByProteinMatchId.put(pmId, new ArrayList<SequenceMatch>());
				seqMatchesByProteinMatchId.get(pmId).add(seqMatch);
			    }


			    // test if the RSM is already calculated
			    if (!array.has("is_coverage_updated") || forceUpdate) {
				LOG.info("going to compute rsmId:" + rsmId);
				final Query psQuery = msiEM.createQuery(LIST_PS_FOR_RSM_QUERY);
				psQuery.setParameter("rsmId", rsmId);
				final List<ProteinSet> protSets = psQuery.getResultList();
				int psIdListSize = protSets.size();
				int psIdcount = 0;

				//String updateQuery = "select ", insertQuery = "select ", updatePmQuery = "select ";

				LOG.info("start processing of " + psIdListSize + " proteinsets.");
				
				for (ProteinSet protSet : protSets)// loop through proteinsets.
				{
				    coveredSeqLengthByProtMatchList.clear();
				    Map<ProteinMatch, ProteinSetProteinMatchItem> protSetMapByProtMatch = new HashMap<>();
				    List<String> allProtMatchesAccession = new ArrayList<>();
				    
				    for (ProteinSetProteinMatchItem protSet2ProtMatch : protSet.getProteinSetProteinMatchItems()) {
				    	ProteinMatch currentProtMatch = protSet2ProtMatch.getProteinMatch();
				    	allProtMatchesAccession.add(currentProtMatch.getAccession());
				    	protSetMapByProtMatch.put(currentProtMatch, protSet2ProtMatch);
				    	coveredSeqLengthByProtMatchList.put(currentProtMatch, getSeqCoverageForProteinMatch(seqMatchesByProteinMatchId,protSet2ProtMatch.getProteinMatch()));
				    } // end go through match 
				    
				    // Get bioSequence for all accessions 				    
				    Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider.findBioSequencesBySEDbIdentValues(allProtMatchesAccession);
				    			    
				    // loop into proteins of current protein set.
				    for (Entry<ProteinMatch, Integer> entry : coveredSeqLengthByProtMatchList.entrySet()) {
					ProteinMatch protMatch = entry.getKey();
					sequencesmatcheslength = entry.getValue();

					List<BioSequenceWrapper> protMatchBioSeqs =  result.get(protMatch.getAccession());
					if((protMatchBioSeqs == null) || (protMatchBioSeqs.isEmpty()) ){
					    LOG.debug(" ****  FOUND NO Sequence for protein {}",protMatch.getAccession());
					} else if(protMatchBioSeqs.size() > 1){
					    LOG.debug(" ****  FOUND MORE THAN 1 Sequence for protein {}. Use first one  ",protMatch.getAccession());
					}
					
					if((protMatchBioSeqs != null) && (protMatchBioSeqs.size() >= 1)) {
					    BioSequenceWrapper bioSeq =  protMatchBioSeqs.get(0);
					    int biosequencelentgh = bioSeq.getSequence().length();
					    // to avoid the indeterminate form : /0
					    if ((biosequencelentgh > 0) && (sequencesmatcheslength < biosequencelentgh)) {

						// call the function loop_merge to execute
						// for example 1000 update in one command :
						// loop_merge(proteinmatch_id,coverage_value,Rsm_id)
						// if(nbTreatedPMInBatch==querySize ||
						// nbTreatedPMInBatch==peptideMatchBatchSize
						// ||
						// nbPeptideMatchTreatedInCurrentRSM==querySize)
						double coverage = calculateSequenceCoverage(biosequencelentgh, sequencesmatcheslength);
						ProteinSetProteinMatchItem proSetMap = protSetMapByProtMatch.get(protMatch);
						proSetMap.setCoverage(new Float(coverage));
						
						msiEM.merge(proSetMap);

						BioSequence msiBioSeq = msiEM.find(BioSequence.class,bioSeq.getSequenceId());
						boolean persist = false;
						if(msiBioSeq == null){
						    msiBioSeq = new BioSequence();
						    persist = true;
						}
						
						msiBioSeq.setAlphabet(Alphabet.AA);
						msiBioSeq.setId(bioSeq.getSequenceId());
						msiBioSeq.setLength(bioSeq.getSequence().length());
						msiBioSeq.setMass(new Double(bioSeq.getMass()).intValue());
						msiBioSeq.setCrc64("");
						msiBioSeq.setPi(new Float(bioSeq.getPI()));
						msiBioSeq.setSequence(bioSeq.getSequence());
						if(persist)
						    msiEM.persist(msiBioSeq);
						else
						    msiEM.merge(msiBioSeq);

						protMatch.setBioSequenceId(bioSeq.getSequenceId());
						msiEM.merge(protMatch);

//						if (nbPeptideMatchTreatedInCurrentRSM >= peptideMatchBatchSize) {// then flush
//						    updateQuery += " updatepspmitem(" + proteinmatchid + ","
//							    + calculateSequenceCoverage(biosequencelentgh, sequencesmatcheslength)
//							    + "," + psId + "," + rsmId + "); ";
//						    insertQuery += " upsertmw(" + biosequenceId + ",'aa','" + bioSequence + "',"
//							    + biosequencelentgh + "," + biosequenceMass + "," + biosequencePi
//							    + ",'',''); ";
//						    updatePmQuery += " updatepm(" + proteinmatchid + "," + biosequenceId + "); ";
//						    stmt = con.createStatement();
//						    
//						    stmt.execute(updateQuery);
//						    stmt.execute(insertQuery);
//						    stmt.execute(updatePmQuery);
//    
//						    updateQuery = "select ";
//						    insertQuery = "select ";
//						    updatePmQuery = "select ";
//						    nbTreatedPMInBatch = 0;
//						    nbPeptideMatchTreatedInCurrentRSM = 0;
//						} else {// prepare
//						    updateQuery += " updatepspmitem(" + proteinmatchid + ","
//							    + calculateSequenceCoverage(biosequencelentgh, sequencesmatcheslength)
//							    + "," + psId + "," + rsmId + ") , ";
//						    insertQuery += " upsertmw(" + biosequenceId + ",'aa','" + bioSequence + "',"
//							    + biosequencelentgh + "," + biosequenceMass + "," + biosequencePi
//							    + ",'','') , ";
//						    updatePmQuery += " updatepm(" + proteinmatchid + "," + biosequenceId + ") , ";
//						}

					    }	
					    
					} 
				    } // end of proteins list of current protein set
				    
				    if (psIdcount % 500 == 0) {
				    	LOG.info("Processed " + psIdcount + " protein sets / " + psIdListSize);
				    }
				    psIdcount++;
				} // end protein sets

//				if ("select ".equals(updateQuery) || "select ".equals(insertQuery) || "select ".equals(updatePmQuery)) {
//							// it means we finished before the maxQuery count
//						    // do nothing
//						} else {
//							stmt = con.createStatement();
//
//							int stUpdateLength = updateQuery.length(), stInsertLength = insertQuery.length(),
//									stUpdatePmLength = updatePmQuery.length();
//							if (updateQuery.substring(stUpdateLength - 2).equals(", ")) {
//								updateQuery = updateQuery.substring(0, stUpdateLength - 3);
//							}
//							if (insertQuery.substring(stInsertLength - 2).equals(", ")) {
//								insertQuery = insertQuery.substring(0, stInsertLength - 3);
//							}
//							if (updatePmQuery.substring(stUpdatePmLength - 2).equals(", ")) {
//								updatePmQuery = updatePmQuery.substring(0, stUpdatePmLength - 3);
//							}
//							stmt.execute(updateQuery);
//							stmt.execute(insertQuery);
//							stmt.execute(updatePmQuery);
//						}
//						updateQuery = "";
//						insertQuery = "";
//						updatePmQuery = "";
				
				if (!array.has("is_coverage_updated")) {
				    array.addProperty("is_coverage_updated", true);
				    final Query updateQueryprop = msiEM.createNativeQuery(UPDATE_QUERY_RSM);
				    updateQueryprop.setParameter("sr", array.toString());
				    updateQueryprop.setParameter("rsmId", rsmId);
				    updateQueryprop.executeUpdate();				    
				}
					
				LOG.info("rsm " + rsmId + " successfully calculated");
			    } else {
				LOG.info("rsmId: " + rsmId + " already calculated");
			    }

//					con.commit();
			}//End go through RSMs
		    } //At least One SEdb
		    msiEM.getTransaction().commit();
		    msiTransactionOK = true;
		} catch (Exception ex) {
			LOG.error("Error accessing MSI Db Project #" + projectId, ex);
			try {
			    if(!msiTransactionOK)
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

	private static Integer getSeqCoverageForProteinMatch(Map<Long, List<SequenceMatch>> seqMatchesByProteinMatchId, final ProteinMatch protMatch) {

		// variables definition
		HashSet<Integer> coveredAASet = new HashSet<Integer>(); //Set of protein sequence index covered by PeptideMatch	
		List<SequenceMatch> seqMatches = seqMatchesByProteinMatchId.get(protMatch.getId());

		for (SequenceMatch seqMatch : seqMatches) {
			
		    SequenceMatchPK seqMatchKey = seqMatch.getId();
		    int start = seqMatchKey.getStart();
		    int stop =  seqMatchKey.getStop();

		    // use set to remove duplicate indexes
		    coveredAASet.addAll(getSequencesIndexes(start, stop));
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
