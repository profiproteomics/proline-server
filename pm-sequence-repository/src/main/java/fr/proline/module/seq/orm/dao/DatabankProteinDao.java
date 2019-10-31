package fr.proline.module.seq.orm.dao;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import fr.proline.core.orm.util.JPARepositoryUtils;
import fr.proline.module.seq.orm.DatabankInstance;
import fr.proline.module.seq.orm.DatabankProtein;
import fr.proline.repository.util.JPAUtils;
import fr.profi.util.StringUtils;

public final class DatabankProteinDao {

	public static List<DatabankProtein> findProteins(final EntityManager seqEM, final Collection<String> proteinIdentifiers) {
		JPAUtils.checkEntityManager(seqEM);
		return JPARepositoryUtils.executeInQueryAsBatch(seqEM.createNamedQuery("findSEDbIdentByValues", DatabankProtein.class), "values", proteinIdentifiers);
	}

	public static List<DatabankProtein> findProteinsInDatabank(
		final EntityManager seqEM,
		final DatabankInstance databank,
		final Collection<String> proteinIdentifiers) {

		JPAUtils.checkEntityManager(seqEM);

		if (databank == null) {
			throw new IllegalArgumentException("SeDbInstance is null");
		}

		final TypedQuery<DatabankProtein> query = seqEM.createNamedQuery("findSEDbIdentBySEDbInstanceAndValues", DatabankProtein.class);
		query.setParameter("seDbInstance", databank);

		return JPARepositoryUtils.executeInQueryAsBatch(query, "values", proteinIdentifiers);
	}

	public static List<DatabankProtein> findProteinsInDatabankName(
		final EntityManager seqEM,
		final String seDbName,
		final Collection<String> proteinIdentifiers) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(seDbName)) {
			throw new IllegalArgumentException("Invalid seDbName");
		}

		final TypedQuery<DatabankProtein> query = seqEM.createNamedQuery("findSEDbIdentBySEDbNameAndValues", DatabankProtein.class);
		query.setParameter("seDbName", seDbName);

		return JPARepositoryUtils.executeInQueryAsBatch(query, "values", proteinIdentifiers);
	}

}
