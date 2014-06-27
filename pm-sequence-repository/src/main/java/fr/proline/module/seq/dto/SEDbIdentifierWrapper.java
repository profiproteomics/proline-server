package fr.proline.module.seq.dto;

import java.util.Objects;

import fr.profi.util.StringUtils;

public class SEDbIdentifierWrapper {

    private final String m_value;

    private final String m_description;

    /* All mutable fields are @GuardedBy("this") */
    private boolean m_inferred;

    private String m_repositoryIdentifier;

    /* Constructors */
    public SEDbIdentifierWrapper(final String value, final String description) {

	if (StringUtils.isEmpty(value)) {
	    throw new IllegalArgumentException("Invalid value");
	}

	m_value = value;

	if (StringUtils.isEmpty(description)) {
	    m_description = null; // Normalize to null
	} else {
	    m_description = description;
	}

    }

    public String getValue() {
	return m_value;
    }

    /**
     * 
     * @return can be <code>null</code> .
     */
    public String getDescription() {
	return m_description;
    }

    public synchronized void setRepositoryIdentifier(final String repositoryIdent) {

	if (m_repositoryIdentifier == null) {

	    if (!StringUtils.isEmpty(repositoryIdent)) { // Normalize to null
		m_repositoryIdentifier = repositoryIdent;
	    }

	} else {

	    if (!m_repositoryIdentifier.equals(repositoryIdent)) { // Latched
		throw new IllegalStateException(
			"SEDbIdentifierWrapper ALREADY contains a repositoryIdentifier");
	    }

	}

    }

    /**
     * Can be <code>null</code> .
     * 
     * @return
     */
    public synchronized String getRepositoryIdentifier() {
	return m_repositoryIdentifier;
    }

    public synchronized void setInferred(final boolean inferred) {

	if (m_inferred) {

	    if (!inferred) { // Latched
		throw new IllegalStateException("SEDbIdentifierWrapper is ALREADY inferred");
	    }

	} else {
	    m_inferred = inferred;
	}

    }

    public synchronized boolean isInferred() {
	return m_inferred;
    }

    @Override
    public int hashCode() {
	return getValue().hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
	boolean result = false;

	if (this == obj) {
	    result = true;
	} else if (obj instanceof SEDbIdentifierWrapper) {
	    final SEDbIdentifierWrapper otherIdent = (SEDbIdentifierWrapper) obj;

	    result = (getValue().equals(otherIdent.getValue()) && Objects.equals(getDescription(),
		    otherIdent.getDescription()));
	}

	return result;
    }

}
