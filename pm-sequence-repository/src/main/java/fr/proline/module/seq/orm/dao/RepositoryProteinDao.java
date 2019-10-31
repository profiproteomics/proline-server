package fr.proline.module.seq.orm.dao;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import fr.proline.core.orm.util.JPARepositoryUtils;
import fr.proline.module.seq.orm.RepositoryProtein;
import fr.proline.repository.util.JPAUtils;
import fr.profi.util.StringUtils;

public final class RepositoryProteinDao {

	public static List<RepositoryProtein> findRepositoryIdentByRepoNameAndValues(
		final EntityManager seqEM,
		final String repositoryName,
		final Collection<String> values) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(repositoryName)) {
			throw new IllegalArgumentException("Invalid repositoryName");
		}

		final TypedQuery<RepositoryProtein> query = seqEM.createNamedQuery("findRepositoryIdentByRepoNameAndValues", RepositoryProtein.class);
		query.setParameter("repositoryName", repositoryName);

		return JPARepositoryUtils.executeInQueryAsBatch(query, "values", values);
	}

}
