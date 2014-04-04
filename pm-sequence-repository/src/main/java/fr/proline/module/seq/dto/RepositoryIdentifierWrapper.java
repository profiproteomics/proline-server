package fr.proline.module.seq.dto;

import java.io.Serializable;

import fr.proline.util.StringUtils;

public class RepositoryIdentifierWrapper implements Serializable {

    private static final long serialVersionUID = 1L;

    private final String m_repositoryName;

    private final String m_repositoryURL;

    private final String m_repositoryIdentifierValue;

    public RepositoryIdentifierWrapper(final String repositoryName, final String repositoryURL,
	    final String repositoryIdentValue) {

	if (StringUtils.isEmpty(repositoryName)) {
	    throw new IllegalArgumentException("Invalid repositoryName");
	}

	m_repositoryName = repositoryName;

	if (StringUtils.isEmpty(repositoryURL)) {
	    m_repositoryURL = null; // Normalize to null
	} else {
	    m_repositoryURL = repositoryURL;
	}

	if (StringUtils.isEmpty(repositoryIdentValue)) {
	    throw new IllegalArgumentException("Invalid repositoryIdentValue");
	}

	m_repositoryIdentifierValue = repositoryIdentValue;

    }

    public String getRepositoryName() {
	return m_repositoryName;
    }

    /**
     * Can be <code>null</code> .
     * 
     * @return
     */
    public String getRepositoryURL() {
	return m_repositoryURL;
    }

    public String getRepositoryIdentifierValue() {
	return m_repositoryIdentifierValue;
    }

}
