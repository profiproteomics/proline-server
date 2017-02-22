package fr.proline.module.seq;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.RepositoryIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
import fr.proline.module.seq.orm.Alphabet;
import fr.proline.module.seq.orm.BioSequence;
import fr.proline.module.seq.orm.Repository;
import fr.proline.module.seq.orm.RepositoryIdentifier;
import fr.proline.module.seq.orm.SEDb;
import fr.proline.module.seq.orm.SEDbIdentifier;
import fr.proline.module.seq.orm.SEDbInstance;
import fr.proline.module.seq.orm.repository.SEDbIdentifierRepository;
import fr.proline.repository.IDatabaseConnector;


public final class BioSequenceProvider {

	private static final Logger LOG = LoggerFactory.getLogger(BioSequenceProvider.class);

	/* Private constructor (Utility class) */
	private BioSequenceProvider() {
	}
	
	/**
	 * Create a Map from proteins specified by its value (name/acc..) to a associated SEDbIdentifierRelated
	 * The SEDbIdentifierRelated contains the List of BioSequenceWrapper for each SEDbIdentier found and the List of SEDbIdentifierWrapper
	 *  (if one value corresponds to multiple SEDbIdentifier) 
	 * 
	 * @param values
	 * @return
	 */
	public static Map<String, SEDbIdentifierRelated> findSEDbIdentRelatedData(final Collection<String> values) {
		Map<String, SEDbIdentifierRelated> result = null;

		/* Client / Provider side */
		final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(false);

		EntityManager seqEM = seqDb.createEntityManager();

		try {
			result = findSEDbIdentRelated(seqEM, values);
		} finally {

			if (seqEM != null) {
				try {
					seqEM.close();
				} catch (Exception exClose) {
					LOG.error("Error closing SEQ Db EntityManager", exClose);
				}
			}
		}
		return result;
	}
	

	private static Map<String, SEDbIdentifierRelated> findSEDbIdentRelated(final EntityManager seqEM, final Collection<String> values) {
		
		final Map<String, SEDbIdentifierRelated> result= new HashMap<>();
		
		final List<SEDbIdentifier> seDbIdentifiers = SEDbIdentifierRepository.findSEDbIdentByValues(seqEM, values);				
		if ((seDbIdentifiers != null) && !seDbIdentifiers.isEmpty()) {

			for (final SEDbIdentifier seDbIdent : seDbIdentifiers) {
				
				final String key = seDbIdent.getValue();// Should not be null
				if ((key!= null) && (!key.isEmpty())) {
					final String Description = seDbIdent.getDescription();//could be null
					
					SEDbIdentifierRelated seIdentObjects =result.get(key);
					if(seIdentObjects== null) {
						seIdentObjects = new SEDbIdentifierRelated();
						result.put(key, seIdentObjects);
					}
					
					//-- Get BioSequenceWrapper
					List<BioSequenceWrapper> bioSequences = seIdentObjects.getBioSequenceWrappers();										

					final BioSequenceWrapper bioSequenceW = buildBioSequenceWrapper(seDbIdent);
					bioSequences.add(bioSequenceW);
					
					//-- Get SEDbIdentifierWrapper
					List<SEDbIdentifierWrapper> sedDbid =  seIdentObjects.getSEDbIdentWrappers();					
					SEDbIdentifierWrapper SEDbIdentWrap=new SEDbIdentifierWrapper(key, Description);
					sedDbid.add(SEDbIdentWrap);
				}
			} //End go through seDbIdentifiers	
		}
		
		return result;
	}

	private static BioSequenceWrapper buildBioSequenceWrapper(final SEDbIdentifier seDbIdent) {
		assert (seDbIdent != null) : "buildBioSequenceWrapper() seDbIdent is null";

		final BioSequence bioSequence = seDbIdent.getBioSequence();// Should not be null
		final long sequenceId = bioSequence.getId();
		final String sequence = bioSequence.getSequence();// Should not be null

		final SEDbInstance seDbInstance = seDbIdent.getSEDbInstance();// Should not be null

		final String seDbRelease = seDbInstance.getRelease();// Should not be null
		final String sourcePath = seDbInstance.getSourcePath();// Should not be null

		final SEDb seDb = seDbInstance.getSEDb();// Should not be null
		final String seDbName = seDb.getName();// Should not be null
		final Alphabet alphabet = seDb.getAlphabet();// Should not be null

		final SEDbInstanceWrapper seDbInstanceW = new SEDbInstanceWrapper(seDbName, alphabet, sourcePath);

		RepositoryIdentifierWrapper repositoryIdentValue = null;

		final RepositoryIdentifier repositoryIdent = seDbIdent.getRepositoryIdentifier();

		if (repositoryIdent != null) {// Can be null
			repositoryIdentValue = buildRepositoryIdentifierWrapper(repositoryIdent);
		}

		return new BioSequenceWrapper(sequenceId, sequence, seDbInstanceW, seDbRelease, repositoryIdentValue);
	}

	private static RepositoryIdentifierWrapper buildRepositoryIdentifierWrapper(
		final RepositoryIdentifier repositoryIdent) {
		assert (repositoryIdent != null) : "buildRepositoryIdentifierWrapper() repositoryIdent is null";

		final Repository repository = repositoryIdent.getRepository();// Should not be null
		final String repositoryName = repository.getName();// Should not be null
		final String repositoryURL = repository.getURL();// Can be null

		final String repositoryIdentValue = repositoryIdent.getValue();// Should not be null

		return new RepositoryIdentifierWrapper(repositoryName, repositoryURL, repositoryIdentValue);
	}

	public static class SEDbIdentifierRelated {
		List<BioSequenceWrapper> bioSequences;		
		List<SEDbIdentifierWrapper> sedDbid;
		
		
		public SEDbIdentifierRelated(){
			bioSequences = new ArrayList<BioSequenceWrapper>();
			sedDbid =  new ArrayList<SEDbIdentifierWrapper>();
		}
		public List<BioSequenceWrapper> getBioSequenceWrappers() {
			return bioSequences;
		}
		public void setBioSequenceWrappers(List<BioSequenceWrapper> bioSequences) {
			this.bioSequences = bioSequences;
		}
		public List<SEDbIdentifierWrapper> getSEDbIdentWrappers() {
			return sedDbid;
		}
		public void setSEDbIdentWrappers(List<SEDbIdentifierWrapper> sedDbid) {
			this.sedDbid = sedDbid;
		}
		
		
	}
}
