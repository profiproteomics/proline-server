package fr.proline.module.seq;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.RepositoryIdentifierWrapper;
import fr.proline.module.seq.dto.SEDbInstanceWrapper;
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

    public static Map<String, List<BioSequenceWrapper>> findBioSequencesBySEDbIdentValues(
	    final Collection<String> values) {
	Map<String, List<BioSequenceWrapper>> result = null;

	/* Client / Provider side */
	final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(false);
	final EntityManagerFactory emf = seqDb.getEntityManagerFactory();

	EntityManager seqEM = emf.createEntityManager();

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

    public static Map<String, List<BioSequenceWrapper>> findBioSequencesBySEDbIdentValues(
	    final EntityManager seqEM, final Collection<String> values) {
	final Map<String, List<BioSequenceWrapper>> result = new HashMap<>();

	final List<SEDbIdentifier> seDbIdentifiers = SEDbIdentifierRepository.findSEDbIdentByValues(seqEM,
		values);
	if ((seDbIdentifiers != null) && !seDbIdentifiers.isEmpty()) {

	    for (final SEDbIdentifier seDbIdent : seDbIdentifiers) {
		final String key = seDbIdent.getValue(); // Should not be null

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

    private static BioSequenceWrapper buildBioSequenceWrapper(final SEDbIdentifier seDbIdent) {
	assert (seDbIdent != null) : "buildBioSequenceWrapper() seDbIdent is null";

	final BioSequence bioSequence = seDbIdent.getBioSequence(); // Should not be null
	final String sequence = bioSequence.getSequence(); // Should not be null

	final SEDbInstance seDbInstance = seDbIdent.getSEDbInstance(); // Should not be null

	final String seDbRelease = seDbInstance.getRelease(); // Should not be null
	final String sourcePath = seDbInstance.getSourcePath(); // Should not be null

	final SEDb seDb = seDbInstance.getSEDb(); // Should not be null
	final String seDbName = seDb.getName(); // Should not be null

	final SEDbInstanceWrapper seDbInstanceW = new SEDbInstanceWrapper(seDbName, sourcePath);

	RepositoryIdentifierWrapper repositoryIdentValue = null;

	final RepositoryIdentifier repositoryIdent = seDbIdent.getRepositoryIdentifier();

	if (repositoryIdent != null) { // Can be null
	    repositoryIdentValue = buildRepositoryIdentifierWrapper(repositoryIdent);
	}

	return new BioSequenceWrapper(sequence, seDbInstanceW, seDbRelease, repositoryIdentValue);
    }

    private static RepositoryIdentifierWrapper buildRepositoryIdentifierWrapper(
	    final RepositoryIdentifier repositoryIdent) {
	assert (repositoryIdent != null) : "buildRepositoryIdentifierWrapper() repositoryIdent is null";

	final Repository repository = repositoryIdent.getRepository(); // Should not be null
	final String repositoryName = repository.getName(); // Should not be null
	final String repositoryURL = repository.getURL(); // Can be null

	final String repositoryIdentValue = repositoryIdent.getValue(); // Should not be null

	return new RepositoryIdentifierWrapper(repositoryName, repositoryURL, repositoryIdentValue);
    }

}
