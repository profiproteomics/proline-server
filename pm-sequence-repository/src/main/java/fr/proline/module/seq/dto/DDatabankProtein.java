package fr.proline.module.seq.dto;

import java.util.Objects;

import fr.profi.util.StringUtils;

public class DDatabankProtein {

	private final String m_identifier;
	private final String m_description;
	private boolean m_inferred;
	private String m_repositoryIdentifier;

	/* Constructors */
	public DDatabankProtein(final String identifier, final String description) {

		assert !StringUtils.isEmpty(identifier) : "Invalid identifier";

		m_identifier = identifier;

		if (StringUtils.isEmpty(description)) {
			m_description = null; // Normalize to null
		} else {
			m_description = description;
		}

	}

	public String getIdentifier() {
		return m_identifier;
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
						"DDatabankProtein ALREADY contains a repositoryIdentifier");
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
				throw new IllegalStateException("DDatabankProtein is ALREADY inferred");
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
		return getIdentifier().hashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		boolean result = false;

		if (this == obj) {
			result = true;
		} else if (obj instanceof DDatabankProtein) {
			final DDatabankProtein otherIdent = (DDatabankProtein) obj;

			result = (getIdentifier().equals(otherIdent.getIdentifier()) && Objects.equals(getDescription(),
				otherIdent.getDescription()));
		}

		return result;
	}

}
