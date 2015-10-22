package fr.proline.module.seq.orm.repository;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.NonUniqueResultException;
import javax.persistence.TypedQuery;

import fr.proline.module.seq.orm.Repository;
import fr.proline.repository.util.JPAUtils;
import fr.profi.util.StringUtils;

public final class RepositoryRepository {

	/* Private constructor (Utility class) */
	private RepositoryRepository() {
	}

	public static Repository findRepositoryByName(final EntityManager seqEM, final String name) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(name)) {
			throw new IllegalArgumentException("Invalid name");
		}

		Repository result = null;

		final TypedQuery<Repository> query = seqEM.createNamedQuery("findRepositoryByName", Repository.class);
		query.setParameter("name", name);

		final List<Repository> seDbs = query.getResultList();

		if ((seDbs != null) && !seDbs.isEmpty()) {

			if (seDbs.size() == 1) {
				result = seDbs.get(0);
			} else {
				throw new NonUniqueResultException("There are more than one Repository for given name");
			}

		}

		return result;
	}

}
