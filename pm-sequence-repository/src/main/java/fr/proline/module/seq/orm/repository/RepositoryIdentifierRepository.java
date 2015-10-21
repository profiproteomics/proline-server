package fr.proline.module.seq.orm.repository;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import fr.proline.core.orm.util.JPARepositoryUtils;
import fr.proline.module.seq.orm.RepositoryIdentifier;
import fr.proline.repository.util.JPAUtils;
import fr.profi.util.StringUtils;

public final class RepositoryIdentifierRepository {

	/* Private constructor (Utility class) */
	private RepositoryIdentifierRepository() {
	}

	public static List<RepositoryIdentifier> findRepositoryIdentByRepoNameAndValues(
		final EntityManager seqEM,
		final String repositoryName,
		final Collection<String> values) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(repositoryName)) {
			throw new IllegalArgumentException("Invalid repositoryName");
		}

		final TypedQuery<RepositoryIdentifier> query = seqEM.createNamedQuery("findRepositoryIdentByRepoNameAndValues", RepositoryIdentifier.class);
		query.setParameter("repositoryName", repositoryName);

		return JPARepositoryUtils.executeInQueryAsBatch(query, "values", values);
	}

}
