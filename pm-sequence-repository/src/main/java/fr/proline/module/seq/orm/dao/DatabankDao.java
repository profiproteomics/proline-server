package fr.proline.module.seq.orm.dao;

import java.util.Arrays;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.NonUniqueResultException;
import javax.persistence.TypedQuery;

import fr.proline.module.seq.orm.Databank;
import fr.proline.module.seq.orm.DatabankInstance;
import fr.proline.module.seq.orm.ParsingRule;
import fr.proline.module.seq.util.DatabankInstanceComparator;
import fr.proline.repository.util.JPAUtils;
import fr.profi.util.StringUtils;

public final class DatabankDao {


	public static Databank findSEDbByName(final EntityManager seqEM, final String name) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(name)) {
			throw new IllegalArgumentException("Invalid name");
		}

		Databank result = null;

		final TypedQuery<Databank> query = seqEM.createNamedQuery("findSEDbByName", Databank.class);
		query.setParameter("name", name);

		final List<Databank> seDbs = query.getResultList();

		if ((seDbs != null) && !seDbs.isEmpty()) {

			if (seDbs.size() == 1) {
				result = seDbs.get(0);
			} else {
				throw new NonUniqueResultException("There are more than one Databank for given name");
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

		final TypedQuery<ParsingRule> query = seqEM.createNamedQuery("findParsingRuleByName", ParsingRule.class);
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

	public static List<DatabankInstance> findSEDbInstanceBySEDbName(
		final EntityManager seqEM,
		final String seDbName) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(seDbName)) {
			throw new IllegalArgumentException("Invalid seDbName");
		}

		List<DatabankInstance> result = null;

		final TypedQuery<DatabankInstance> query = seqEM.createNamedQuery("findSEDbInstanceBySEDbName",
			DatabankInstance.class);
		query.setParameter("seDbName", seDbName);

		List<DatabankInstance> seDbInstances = query.getResultList();

		if ((seDbInstances != null) && !seDbInstances.isEmpty()) {
			/*
			 * First create a protection copy in an Array (Do not sort List provided by JPA implementation).
			 * 
			 * Collections.sort() calls List.toArray() then sorts the array and copy-back array elems to the
			 * list !
			 */
			final DatabankInstance[] sortedInstances = seDbInstances.toArray(new DatabankInstance[seDbInstances
					.size()]);
			final DatabankInstanceComparator comparator = new DatabankInstanceComparator();

			Arrays.sort(sortedInstances, comparator);

			result = Arrays.asList(sortedInstances);
		}

		return result;
	}

	public static List<DatabankInstance> findSEDbInstanceByNameAndSourcePath(
		final EntityManager seqEM,
		final String seDbName,
		final String sourcePath) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(seDbName)) {
			throw new IllegalArgumentException("Invalid seDbName");
		}

		if (StringUtils.isEmpty(sourcePath)) {
			throw new IllegalArgumentException("Invalid sourcePath");
		}

		List<DatabankInstance> result = null;

		final TypedQuery<DatabankInstance> query = seqEM.createNamedQuery("findSEDbInstanceByNameAndSourcePath",
			DatabankInstance.class);
		query.setParameter("seDbName", seDbName);
		query.setParameter("sourcePath", sourcePath);

		List<DatabankInstance> seDbInstances = query.getResultList();

		if ((seDbInstances != null) && !seDbInstances.isEmpty()) {
			/* Same sort as findSEDbInstanceBySEDbName */
			final DatabankInstance[] sortedInstances = seDbInstances.toArray(new DatabankInstance[seDbInstances.size()]);
			final DatabankInstanceComparator comparator = new DatabankInstanceComparator();

			Arrays.sort(sortedInstances, comparator);
			result = Arrays.asList(sortedInstances);
		}

		return result;
	}

	public static DatabankInstance findSEDbInstanceByNameAndRelease(
		final EntityManager seqEM,
		final String seDbName,
		final String release) {

		JPAUtils.checkEntityManager(seqEM);

		if (StringUtils.isEmpty(seDbName)) {
			throw new IllegalArgumentException("Invalid seDbName");
		}

		if (StringUtils.isEmpty(release)) {
			throw new IllegalArgumentException("Invalid release");
		}

		DatabankInstance result = null;

		final TypedQuery<DatabankInstance> query = seqEM.createNamedQuery("findSEDbInstanceByNameAndRelease",
			DatabankInstance.class);
		query.setParameter("seDbName", seDbName);
		query.setParameter("release", release);

		final List<DatabankInstance> seDbInstances = query.getResultList();

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
