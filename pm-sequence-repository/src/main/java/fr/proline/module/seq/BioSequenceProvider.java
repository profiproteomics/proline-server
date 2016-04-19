package fr.proline.module.seq;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.persistence.EntityManagerFactory;
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

	public static Map<String, List<BioSequenceWrapper>> findBioSequencesBySEDbIdentValues(final Collection<String> values) {
		Map<String, List<BioSequenceWrapper>> result = null;

		/* Client / Provider side */
		final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(false);

		EntityManager seqEM = seqDb.createEntityManager();

		try {
			result = findBioSequencesBySEDbIdentValues(seqEM, values);
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

	public static Map<String, List<SEDbIdentifierWrapper>> findSEDbyIdentValues(final Collection<String> values) {
		Map<String, List<SEDbIdentifierWrapper>> result = null;

		/* Client / Provider side */
		final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(false);

		EntityManager seqEM = seqDb.createEntityManager();

		try {
			result = findSEDbIdentValues(seqEM, values);
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

	public static Map<String, List<BioSequenceWrapper>> findBioSequencesBySEDbIdentValues(final EntityManager seqEM, final Collection<String> values) {

		final Map<String, List<BioSequenceWrapper>> result = new HashMap<>();
		final List<SEDbIdentifier> seDbIdentifiers = SEDbIdentifierRepository.findSEDbIdentByValues(seqEM, values);
		if ((seDbIdentifiers != null) && !seDbIdentifiers.isEmpty()) {

			for (final SEDbIdentifier seDbIdent : seDbIdentifiers) {
				final String key = seDbIdent.getValue();// Should not be null

				List<BioSequenceWrapper> bioSequences = result.get(key);

				if (bioSequences == null) {
					bioSequences = new ArrayList<>();

					result.put(key, bioSequences);
				}

				final BioSequenceWrapper bioSequenceW = buildBioSequenceWrapper(seDbIdent);
				bioSequences.add(bioSequenceW);
			}

		}

		return result;
	}
	
	public static Map<String, List<SEDbIdentifierWrapper>> findSEDbIdentValues(final EntityManager seqEM, final Collection<String> values) {

		final Map<String, List<SEDbIdentifierWrapper>> result = new HashMap<>();
		
		final List<SEDbIdentifier> seDbIdentifiers = SEDbIdentifierRepository.findSEDbIdentByValues(seqEM, values);
		List<SEDbIdentifierWrapper> sedDbid = new ArrayList<>();
		if ((seDbIdentifiers != null) && !seDbIdentifiers.isEmpty()) {

			for (final SEDbIdentifier seDbIdent : seDbIdentifiers) {
				final String key = seDbIdent.getValue();// Should not be null
				final String Description = seDbIdent.getDescription();//could be null
				
				if ((key!= null) && (!key.isEmpty())) {
					 try{
						 SEDbIdentifierWrapper SEDbIdentWrap=new SEDbIdentifierWrapper(key, Description);
						 sedDbid.add(SEDbIdentWrap);}
					 catch(Exception e){
				     LOG.debug("error ",e);
					 }
				}
				result.put(key, sedDbid);
			}

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

}
