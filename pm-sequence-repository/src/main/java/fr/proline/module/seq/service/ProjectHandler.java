package fr.proline.module.seq.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

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
import fr.proline.core.orm.msi.SeqDatabase;
import fr.proline.core.orm.msi.SequenceMatch;
import fr.proline.core.orm.msi.SequenceMatchPK;
import fr.proline.core.orm.util.DataStoreConnectorFactory;
import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.module.seq.dto.SequenceMatchWrapper;
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
 
    private static final String VALIDATED_ACCRSM_QUERY = "SELECT pm.id, pm.accession, pmsqm.id"
    	    + " FROM fr.proline.core.orm.msi.ProteinMatch pm, fr.proline.core.orm.msi.SeqDatabase sdb, fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap pmsdb,"
    		+"fr.proline.core.orm.msi.SequenceMatch pmsqm"
    	    + " JOIN pm.proteinSetProteinMatchItems ps"
    	    + " WHERE (pmsdb.id.proteinMatchId = pm.id) AND (pmsdb.id.seqDatabaseId = sdb.id) AND (pmsqm.id.proteinMatchId=pm.id)"
    	    + " AND ((upper(pm.resultSet.type) = 'SEARCH') OR (upper(pm.resultSet.type) = 'USER'))"
    	    + " AND (ps.proteinSet.isValidated = true) AND(ps.proteinSet.resultSummary IS NOT NULL)";
    
    // In this version : find all sequence coverage for each protein match via the projectId
    public static void fillsequenceMatchesByProteinMatch(final long projectId) {
    	final DataStoreConnectorFactory connectorFactory = DatabaseAccess.getDataStoreConnectorFactory();
    	final IDatabaseConnector msiDbConnector = connectorFactory.getMsiDbConnector(projectId);
    	Map<String, Integer> accessionSqmatch = new HashMap<String, Integer>();
    	List<BioSequenceWrapper> bioSequenceWrapperList;
    	if (msiDbConnector == null) {
    		LOG.warn("Project #{} has NO associated MSI Db", projectId);
    	} else {
    		EntityManager msiEM = null;
    		try {
    			final EntityManagerFactory emf = msiDbConnector.getEntityManagerFactory();
    			msiEM = emf.createEntityManager();
    				final Query pmSdmQuery = msiEM.createQuery(VALIDATED_ACCRSM_QUERY);
    				final List<Object[]> pmSdmLines = pmSdmQuery.getResultList();
    				if ((pmSdmLines != null) && !pmSdmLines.isEmpty()) {
    					fillproteinmatch(pmSdmLines);
    					accessionSqmatch=fillproteinmatch(pmSdmLines);   					
    					ArrayList<String> values = new ArrayList<>();
    					int biosequencelentgh = 0;
    					for (Entry<String, Integer> entry : accessionSqmatch.entrySet())
    					{   
    						values.add(entry.getKey());
    						Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider.findBioSequencesBySEDbIdentValues(values);
    						if(result.get(entry.getKey())!=null){
    							bioSequenceWrapperList = result.get(entry.getKey());
    							biosequencelentgh=bioSequenceWrapperList.get(0).getSequence().length();
    							LOG.debug("the sequence coverage : "+calculsequenceCoverage(biosequencelentgh,entry.getValue()));
    							result.clear(); 
    						}else
    						{
    							LOG.warn("biosequence does not exist for this proteinmatch");
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

  
    private static final int EXPECTED_LINE_LENGTH = 3;
    
    private static final int EXPECTED_LINE_LENGTH_PM_SQM = 3;
    
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
		 LOG.warn("seqDbId", seqDbId);
		final String name = seqDb.getName();
		 LOG.warn("seqDbId", name);
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
    
    //***aymen
    private static Map<String, Integer> fillproteinmatch(final List<Object[]> lines) {
    	assert (lines != null) : "fillproteinmatch() lines can not be null";
    	Map<String, Integer> accessionSqmatch = new HashMap<String, Integer>();
    	for (final Object[] line : lines) {
    		final int lineLength = line.length;
    		if(lineLength>=EXPECTED_LINE_LENGTH_PM_SQM)
    		{
    			SequenceMatchPK id = null;
    			Long Id = null;
    			String accessvalue=null;
    			int start =0;
    			int stop=0;
    			Long peptidID=null;
    			if (line[0] instanceof Long) {
    				Id = (Long) line[0];
    			}
    			if (line[1] instanceof String) {
    				accessvalue = ((String) line[1]).trim(); 
    			}
    			if (line[2] instanceof SequenceMatchPK) {
    				id = (SequenceMatchPK) line[2];
    			}
    			start=(int)id.getStart();
    			stop=(int)id.getStop();
    			peptidID=(Long)id.getPeptideId();
    			if(Id==null)
    			{
    				LOG.error("ProteinMAtch id can not be null:{}",Id);
    			}else{
    				if(accessvalue==null){
    					LOG.error("accession is null,:{}",accessvalue);
    				}else{
    					if((id.getStart()<0)||(id.getStop()<0)){
    						LOG.error("sequence Match is null");
    					}else{
    						accessionSqmatch.put(accessvalue, stop-start);
    					}
    				}
    			}
    		}else
    		{
    			LOG.error("Invalid result line length {} (expected : {})", lineLength, EXPECTED_LINE_LENGTH_PM_SQM);
    		}    
    	} 
    	if(accessionSqmatch.isEmpty()){
    		LOG.error("Invalid proteinmatch Map:{}",accessionSqmatch);
    	}
    	return (Map<String, Integer>) accessionSqmatch;
    }
    
public static Set<Long> findDuplicates(List<Long> listContainingDuplicates) {
    	 
		final Set<Long> setToReturn = new HashSet<Long>();
		final Set<Long> set1 = new HashSet<Long>();
 
		for (Long yourInt : listContainingDuplicates) {
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
}
