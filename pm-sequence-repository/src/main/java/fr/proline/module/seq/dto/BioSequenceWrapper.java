package fr.proline.module.seq.dto;

import java.io.Serializable;

import fr.proline.util.StringUtils;

public class BioSequenceWrapper implements Serializable {

    private static final long serialVersionUID = 1L;

    private final String m_sequence;

    private final SEDbInstanceWrapper m_seDbInstance;

    private final String m_seDbRelease;

    private final RepositoryIdentifierWrapper m_repositoryIdentifier;

    public BioSequenceWrapper(final String sequence, final SEDbInstanceWrapper seDbInstance,
	    final String seDbRelease, final RepositoryIdentifierWrapper repositoryIdentifier) {

	if (StringUtils.isEmpty(sequence)) {
	    throw new IllegalArgumentException("Invalid sequence");
	}

	m_sequence = sequence;

	if (seDbInstance == null) {
	    throw new IllegalArgumentException("SEDbInstance wrapper is null");
	}

	m_seDbInstance = seDbInstance;

	if (StringUtils.isEmpty(seDbRelease)) {
	    throw new IllegalArgumentException("Invalid seDbRelease");
	}

	m_seDbRelease = seDbRelease;

	m_repositoryIdentifier = repositoryIdentifier;
    }

    public String getSequence() {
	return m_sequence;
    }

    public SEDbInstanceWrapper getSEDbInstance() {
	return m_seDbInstance;
    }

    public String getSEDbRelease() {
	return m_seDbRelease;
    }

    /**
     * Can be <code>null</code>.
     * 
     * @return
     */
    public RepositoryIdentifierWrapper getRepositoryIdentifier() {
	return m_repositoryIdentifier;
    }

}
