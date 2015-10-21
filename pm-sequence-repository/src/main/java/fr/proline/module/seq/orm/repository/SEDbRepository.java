package fr.proline.module.seq.orm.repository;

import java.util.Arrays;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.NonUniqueResultException;
import javax.persistence.TypedQuery;

import fr.proline.module.seq.orm.ParsingRule;
import fr.proline.module.seq.orm.SEDb;
import fr.proline.module.seq.orm.SEDbInstance;
import fr.proline.module.seq.orm.SEDbInstanceComparator;
import fr.proline.repository.util.JPAUtils;
import fr.profi.util.StringUtils;

public final class SEDbRepository {

    /* Private constructor (Utility class) */
    private SEDbRepository() {
    }

    public static SEDb findSEDbByName(final EntityManager seqEM, final String name) {

	JPAUtils.checkEntityManager(seqEM);

	if (StringUtils.isEmpty(name)) {
	    throw new IllegalArgumentException("Invalid name");
	}

	SEDb result = null;

	final TypedQuery<SEDb> query = seqEM.createNamedQuery("findSEDbByName", SEDb.class);
	query.setParameter("name", name);

	final List<SEDb> seDbs = query.getResultList();

	if ((seDbs != null) && !seDbs.isEmpty()) {

	    if (seDbs.size() == 1) {
		result = seDbs.get(0);
	    } else {
		throw new NonUniqueResultException("There are more than one SEDb for given name");
	    }

	}

	return result;
    }

    public static ParsingRule findParsingRuleByName(final EntityManager seqEM, final String name) {

	JPAUtils.checkEntityManager(seqEM);

	if (StringUtils.isEmpty(name)) {
	    throw new IllegalArgumentException("Invalid name");
	}

	ParsingRule result = null;

	final TypedQuery<ParsingRule> query = seqEM.createNamedQuery("findParsingRuleByName",ParsingRule.class);
	query.setParameter("name", name);

	final List<ParsingRule> rules = query.getResultList();

	if ((rules != null) && !rules.isEmpty()) {

	    if (rules.size() == 1) {
		result = rules.get(0);
	    } else {
		throw new NonUniqueResultException("There are more than one ParsingRule for given name");
	    }

	}

	return result;
    }

    public static List<SEDbInstance> findSEDbInstanceBySEDbName(final EntityManager seqEM,
	    final String seDbName) {

	JPAUtils.checkEntityManager(seqEM);

	if (StringUtils.isEmpty(seDbName)) {
	    throw new IllegalArgumentException("Invalid seDbName");
	}

	List<SEDbInstance> result = null;

	final TypedQuery<SEDbInstance> query = seqEM.createNamedQuery("findSEDbInstanceBySEDbName",
		SEDbInstance.class);
	query.setParameter("seDbName", seDbName);

	List<SEDbInstance> seDbInstances = query.getResultList();

	if ((seDbInstances != null) && !seDbInstances.isEmpty()) {
	    /*
	     * First create a protection copy in an Array (Do not sort List provided by JPA implementation).
	     * 
	     * Collections.sort() calls List.toArray() then sorts the array and copy-back array elems to the
	     * list !
	     */
	    final SEDbInstance[] sortedInstances = seDbInstances.toArray(new SEDbInstance[seDbInstances
		    .size()]);
	    final SEDbInstanceComparator comparator = new SEDbInstanceComparator();

	    Arrays.sort(sortedInstances, comparator);

	    result = Arrays.asList(sortedInstances);
	}

	return result;
    }

    public static List<SEDbInstance> findSEDbInstanceByNameAndSourcePath(final EntityManager seqEM,
	    final String seDbName, final String sourcePath) {

	JPAUtils.checkEntityManager(seqEM);

	if (StringUtils.isEmpty(seDbName)) {
	    throw new IllegalArgumentException("Invalid seDbName");
	}

	if (StringUtils.isEmpty(sourcePath)) {
	    throw new IllegalArgumentException("Invalid sourcePath");
	}

	List<SEDbInstance> result = null;

	final TypedQuery<SEDbInstance> query = seqEM.createNamedQuery("findSEDbInstanceByNameAndSourcePath",
		SEDbInstance.class);
	query.setParameter("seDbName", seDbName);
	query.setParameter("sourcePath", sourcePath);

	List<SEDbInstance> seDbInstances = query.getResultList();

	if ((seDbInstances != null) && !seDbInstances.isEmpty()) {
	    /* Same sort as findSEDbInstanceBySEDbName */
	    final SEDbInstance[] sortedInstances = seDbInstances.toArray(new SEDbInstance[seDbInstances
		    .size()]);
	    final SEDbInstanceComparator comparator = new SEDbInstanceComparator();

	    Arrays.sort(sortedInstances, comparator);

	    result = Arrays.asList(sortedInstances);
	}

	return result;
    }

    public static SEDbInstance findSEDbInstanceByNameAndRelease(final EntityManager seqEM,
	    final String seDbName, final String release) {

	JPAUtils.checkEntityManager(seqEM);

	if (StringUtils.isEmpty(seDbName)) {
	    throw new IllegalArgumentException("Invalid seDbName");
	}

	if (StringUtils.isEmpty(release)) {
	    throw new IllegalArgumentException("Invalid release");
	}

	SEDbInstance result = null;

	final TypedQuery<SEDbInstance> query = seqEM.createNamedQuery("findSEDbInstanceByNameAndRelease",
		SEDbInstance.class);
	query.setParameter("seDbName", seDbName);
	query.setParameter("release", release);

	final List<SEDbInstance> seDbInstances = query.getResultList();

	if ((seDbInstances != null) && !seDbInstances.isEmpty()) {

	    if (seDbInstances.size() == 1) {
		result = seDbInstances.get(0);
	    } else {
		throw new NonUniqueResultException("There are more than one ParsingRule for given name");
	    }

	}

	return result;
    }

}
