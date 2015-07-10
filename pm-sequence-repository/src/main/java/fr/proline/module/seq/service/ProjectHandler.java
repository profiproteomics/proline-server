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

import com.google.gson.Gson;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.StringUtils;
import fr.proline.core.orm.msi.ProteinMatch;
import fr.proline.core.orm.msi.ResultSummary;
import fr.proline.core.orm.msi.SeqDatabase;
import fr.proline.core.orm.msi.SequenceMatchPK;
import fr.proline.core.orm.util.DataStoreConnectorFactory;
import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.module.seq.orm.*;
import fr.proline.repository.IDatabaseConnector;

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
   
    private static final String ALL_PM_SQM = "select id FROM fr.proline.core.orm.msi.SequenceMatch";
    private static final String ALL_RSM_PROP = "select rs FROM fr.proline.core.orm.msi.ResultSummary rs where rs.id=?";
    //
    private static final String VALIDATED_ACCRSM_QUERY1 = "SELECT pm,sm.id,ps.resultSummary"
    	+" FROM ProteinMatch pm, ProteinSet ps,  ProteinSetProteinMatchItem pspmm, SequenceMatch sm,PeptideMatch pepm "
    	+"WHERE pm.id = pspmm.id.proteinMatchId AND ps.id = pspmm.id.proteinSetId AND sm.id.proteinMatchId = pm.id AND sm.bestPeptideMatchId=pepm.id"
    	+" AND ps.isValidated = 'true' AND ps.resultSummary.id=? AND pepm.rank<=? AND pepm.score>=?";
    
    private static final String LIST_RSMS = "SELECT pm,sm.id,ps.resultSummary FROM ProteinMatch pm, ProteinSet ps,"
    +"  ProteinSetProteinMatchItem pspmm, SequenceMatch sm WHERE pm.id = pspmm.id.proteinMatchId AND ps.id = pspmm.id.proteinSetId"
    +" AND sm.id.proteinMatchId = pm.id AND ps.isValidated = 'true'";
  
    private static final String UPDATE_QUERY = "UPDATE protein_match  set coverage=? where id=?";
    private static final String UPDATE_QUERY_PSPMI = "UPDATE protein_set_protein_match_item  set coverage=? where protein_match_id=?";
    
    private static final int EXPECTED_LINE_LENGTH_PM_SQM = 3;
    private static final int EXPECTED_LINE_LENGTH = 3;
    public static  Gson gson = new Gson();
    
    /* In this version : find all SEDbIdentifiers in all SEDbInstances */
    public static void fillSEDbIdentifiersBySEDb(final long projectId,
	    final Map<SEDbInstanceWrapper, Set<SEDbIdentifierWrapper>> seDbIdentifiers) {
    	
	if (seDbIdentifiers == null) {
	    throw new IllegalArgumentException("SeDbIdentifiers Map is null");
	}

	final DataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
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
			LOG.debug("MSI Project #{} found {} SEDbInstances and {} validated Accession",
				projectId, seDbInstances.size(), nExpectedAccessions);
		    }

		    if (nExpectedAccessions > 0L) {
			int nSEDbIdentifiers = 0;

			final Query pmSdmQuery = msiEM.createQuery(VALIDATED_PM_SDM_QUERY);
		
			final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();
			
			if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
			    nSEDbIdentifiers = fillSEDbIdentifiers(pmSdmLines, seDbInstances, seDbIdentifiers);
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
			    final SEDbInstanceWrapper seDbInstanceW = new SEDbInstanceWrapper(trimmedName,
				    null, fastaFilePath);
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

			final SEDbIdentifierWrapper seDbIdentifierW = new SEDbIdentifierWrapper(value,
				description);

			seDbIdents.add(seDbIdentifierW);

			++nIdentifiers; // Found a valid SEDbIdentifier
		    }

		} // End if (identValue is valid)

	    } else {
		LOG.error("Invalid result line length {} (expected : {})", lineLength, EXPECTED_LINE_LENGTH);
	    }
	} // End loop for each Result line
	return nIdentifiers;
    }
    // In this version : find all sequence coverage for each protein match via the projectId and store it in msi db
    
  
	public static void fillsequenceMatchesByProteinMatch(final long projectId) {
    	final DataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
    	final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
    	Map<ProteinMatch, Integer> accessionSqmatch = new HashMap<ProteinMatch, Integer>();
    	Map<Long, Map<String,String>> rsmproprities = new HashMap<Long, Map<String,String>>();
    	Map<String,String> props=new HashMap<String,String>();
    	int sequencesmatcheslength;
    	int rank;
    	Float score;
    	int peplength;
    	HashSet<Long> rsmlist = new HashSet<Long>();
    	Long proteinmatchid;
    	List<BioSequenceWrapper> bioSequenceWrapperList;
    	if (msiDbConnector == null) {
    		LOG.warn("Project #{} has NO associated MSI Db", projectId);
    	}else {
    		EntityManager msiEM = null;
    		try {
    			final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
    			msiEM = emf.createEntityManager();
    			final Map<Long, SEDbInstanceWrapper> seDbInstances = retrieveAllSeqDatabases(msiEM);
    			if ((seDbInstances == null) || seDbInstances.isEmpty()) {
    				LOG.warn("There is NO SEDbInstance in MSI Project #{}", projectId);
    			} else {
    				
    				final Query listresultsummaries=msiEM.createQuery(LIST_RSMS);
    				final List<Object[]> listresul = listresultsummaries.getResultList();
    				rsmlist.addAll(getrsms(listresul));
    				for(Long RSM:rsmlist)
    				{   
    					LOG.debug("calculating for RSM: " + RSM);
    					final Query listrsmprop = msiEM.createQuery(ALL_RSM_PROP);
        				listrsmprop.setParameter(1, RSM);
        				final List<ResultSummary> rsmpropLines = listrsmprop.getResultList();
        				rsmproprities=fillvalidationproprieties(rsmpropLines);
        				props=rsmproprities.get(RSM);
        				rank=Integer.parseInt(props.get("RANK"));
        				score=Float.parseFloat(props.get("SCORE"));
        				peplength=Integer.parseInt(props.get("PEP_SEQ_LENGTH"));
        				final Query pmSdmQuery = msiEM.createQuery(VALIDATED_ACCRSM_QUERY1);	
        				pmSdmQuery.setParameter(1,RSM);
        				pmSdmQuery.setParameter(2,rank);
        				pmSdmQuery.setParameter(3,score);
        				
        				final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();
        				if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
        					fillproteinmatch(pmSdmLines,peplength);
        					accessionSqmatch=fillproteinmatch(pmSdmLines,peplength);   					
        					ArrayList<String> values = new ArrayList<>();
        					int biosequencelentgh = 0;
        					List<String> Accession = new ArrayList<>();
        					for (Entry<ProteinMatch, Integer> entry : accessionSqmatch.entrySet())
        					{   Accession.add(entry.getKey().getAccession());
        						sequencesmatcheslength=entry.getValue();
        						Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider
        							.findBioSequencesBySEDbIdentValues(Accession);
        						for (Map.Entry<String, List<BioSequenceWrapper>> entry0 : result.entrySet()) {
        							String seDbIdentValue = entry0.getKey();
        							List<BioSequenceWrapper> bioSequences = entry0.getValue();
        							for ( BioSequenceWrapper bsw : bioSequences) {
        								
        								biosequencelentgh=bsw.getSequence().length();
        							}
        							proteinmatchid=entry.getKey().getId();
         							msiEM.getTransaction().begin();
        							final Query updateQuery = msiEM.createNativeQuery(UPDATE_QUERY);
        							updateQuery.setParameter(1,calculsequenceCoverage(biosequencelentgh,sequencesmatcheslength));
        							updateQuery.setParameter(2, proteinmatchid);
        							updateQuery.executeUpdate();
        							msiEM.getTransaction().commit();
        							msiEM.getTransaction().begin();
        							final Query updateQuerypspmi = msiEM.createNativeQuery(UPDATE_QUERY_PSPMI);
        							updateQuerypspmi.setParameter(1,calculsequenceCoverage(biosequencelentgh,sequencesmatcheslength));
        							updateQuerypspmi.setParameter(2, proteinmatchid);
        							updateQuerypspmi.executeUpdate();
        							msiEM.getTransaction().commit();
        					   }
        					Accession.clear();
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
    private static Map<ProteinMatch,Integer> fillproteinmatch(final List<Object[]> lines,int peplength) {
       	Map<ProteinMatch, Integer> accessionSqmatch = new HashMap<ProteinMatch, Integer>();
       	HashSet<Integer> Seqsorted = new HashSet<Integer>();
    	Map<ProteinMatch, List<Integer>> temp = new HashMap<ProteinMatch, List<Integer>>();
    	List<ProteinMatch> ProteinMatchduplicated = new ArrayList<>();
    	List<Integer> SeqLists = new ArrayList<Integer>();
    	List<Integer> Seqindex = new ArrayList<Integer>();
    	List<Long> Listrsms = new ArrayList<Long>();
    	Map<Long,Map<ProteinMatch,Integer>> listRsms =new HashMap<Long,Map<ProteinMatch,Integer>>();
    	ProteinMatch proteinmatch= null;
    	SequenceMatchPK SequenceMatch = null; 
    	ResultSummary rsm = null ;
    	for (final Object[] line : lines) {
    		int start =0;
			int stop=0;
			
    				if (line[0] instanceof ProteinMatch) {
    					proteinmatch = (ProteinMatch) line[0];
    				}
    				if (line[1] instanceof SequenceMatchPK) {
    					SequenceMatch = (SequenceMatchPK) line[1];
    				}
    				if (line[2] instanceof ResultSummary) {
    					rsm = (ResultSummary) line[2];
    				}
    				Listrsms.add(rsm.getId());
    				start=(int)SequenceMatch.getStart();
    				stop=(int)SequenceMatch.getStop();
    				if(ProteinMatchduplicated.contains(proteinmatch)){
						Seqindex.clear();Seqsorted.clear();
						SeqLists=temp.get(proteinmatch);
						if(getSequencesIndexes(start,stop).size()>=peplength)
						{
							SeqLists.addAll(getSequencesIndexes(start,stop));
						}
						temp.put(proteinmatch,SeqLists);
						Seqindex.addAll(temp.get(proteinmatch));
						Seqsorted.addAll(Seqindex);
						accessionSqmatch.put(proteinmatch,Seqsorted.size());
					}else{
						SeqLists.clear();Seqsorted.clear();Seqindex.clear();
						if(getSequencesIndexes(start,stop).size()>=peplength)
						{
							SeqLists.addAll(getSequencesIndexes(start,stop));
						}
						temp.put(proteinmatch,SeqLists);
						Seqindex.addAll(temp.get(proteinmatch));
						Seqsorted.addAll(Seqindex);
						accessionSqmatch.put(proteinmatch,Seqsorted.size());
					 }
    				ProteinMatchduplicated.add(proteinmatch);
    			}
    	return accessionSqmatch;
    } 

public static Set<String> findDuplicates(List<String> listContainingDuplicates) {
		final Set<String> setToReturn = new HashSet<String>();
		final Set<String> set1 = new HashSet<String>();
		for (String yourInt : listContainingDuplicates) {
			if (!set1.add(yourInt)) {
				setToReturn.add(yourInt);
			}
		}
		return setToReturn;
	}

public static double calculsequenceCoverage(int biosequencelentgth,int sequencematchlenttgh){
	
	 double average=((double)sequencematchlenttgh/(double)biosequencelentgth)*100;
	 return average;
}

public static List<Integer> getSequencesIndexes(int start,int stop){
	List<Integer> seqList = new ArrayList();
	for(int i=start;i<=stop;i++){
		seqList.add(i);
	}
	return (seqList);
}

private static List<Long> getrsms(final List<Object[]> lines) {
	List<Long> Listrsms = new ArrayList<Long>();
	ResultSummary rsm=null;
	for (final Object[] line : lines) {
	if (line[2] instanceof ResultSummary) {
		rsm = (ResultSummary) line[2];
	}
	Listrsms.add(rsm.getId());
	}
	return Listrsms;
}
private static Map<Long,Map<String,String>>fillvalidationproprieties(final List<ResultSummary> lines) {

	Map<Long,Map<String,String>> RsmProp=new HashMap<Long, Map<String,String>>();
	Map<String,String> pepfilters=new HashMap<String, String>();
	Map<String,String> profilters=new HashMap<String, String>();
	for (final ResultSummary line : lines) {
		try {
				String validationdpropstr=line.getSerializedProperties();
				Proprieties validationdproprities = gson.fromJson(validationdpropstr,Proprieties.class);
				if(validationdproprities.validation_properties.params.peptide_filters.size()>0)
				{
					for(Peptidefilters pepfilter : validationdproprities.validation_properties.params.peptide_filters){
						pepfilters.put(pepfilter.getParameter(), pepfilter.properties.getThreshold_value());
					}
				}
				if(!pepfilters.containsKey("SCORE"))
				{pepfilters.put("SCORE","0.0");}
				if(!pepfilters.containsKey("RANK"))
				{pepfilters.put("RANK","1");}
				if(!pepfilters.containsKey("SCORE"))
				{pepfilters.put("SCORE","0.0");}
				if(!pepfilters.containsKey("PEP_SEQ_LENGTH"))
				{pepfilters.put("PEP_SEQ_LENGTH","0");}
				
				RsmProp.put(line.getId(),pepfilters);
				
		  } catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	return RsmProp;
}

}
