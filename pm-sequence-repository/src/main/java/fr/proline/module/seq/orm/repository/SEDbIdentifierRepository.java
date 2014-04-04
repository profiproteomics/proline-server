package fr.proline.module.seq.orm.repository;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import fr.proline.core.orm.util.JPARepositoryUtils;
import fr.proline.module.seq.orm.SEDbIdentifier;
import fr.proline.module.seq.orm.SEDbInstance;
import fr.proline.repository.util.JPAUtils;
import fr.proline.util.StringUtils;

public final class SEDbIdentifierRepository {

    /* Private constructor (utility class) */
    private SEDbIdentifierRepository() {
    }

    public static List<SEDbIdentifier> findSEDbIdentByValues(final EntityManager seqEM,
	    final Collection<String> values) {

	JPAUtils.checkEntityManager(seqEM);

	return JPARepositoryUtils.executeInQueryAsBatch(
		seqEM.createNamedQuery("findSEDbIdentByValues", SEDbIdentifier.class), "values", values);
    }

    public static List<SEDbIdentifier> findSEDbIdentBySEDbInstanceAndValues(final EntityManager seqEM,
	    final SEDbInstance seDbInstance, final Collection<String> values) {

	JPAUtils.checkEntityManager(seqEM);

	if (seDbInstance == null) {
	    throw new IllegalArgumentException("SeDbInstance is null");
	}

	final TypedQuery<SEDbIdentifier> query = seqEM.createNamedQuery(
		"findSEDbIdentBySEDbInstanceAndValues", SEDbIdentifier.class);
	query.setParameter("seDbInstance", seDbInstance);

	return JPARepositoryUtils.executeInQueryAsBatch(query, "values", values);
    }

    public static List<SEDbIdentifier> findSEDbIdentBySEDbNameAndValues(final EntityManager seqEM,
	    final String seDbName, final Collection<String> values) {

	JPAUtils.checkEntityManager(seqEM);

	if (StringUtils.isEmpty(seDbName)) {
	    throw new IllegalArgumentException("Invalid seDbName");
	}

	final TypedQuery<SEDbIdentifier> query = seqEM.createNamedQuery("findSEDbIdentBySEDbNameAndValues",
		SEDbIdentifier.class);
	query.setParameter("seDbName", seDbName);

	return JPARepositoryUtils.executeInQueryAsBatch(query, "values", values);
    }

}
