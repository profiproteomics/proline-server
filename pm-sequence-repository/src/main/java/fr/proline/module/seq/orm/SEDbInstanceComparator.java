package fr.proline.module.seq.orm;

import java.sql.Timestamp;
import java.util.Comparator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Compare two SEDbInstance based on lexicographic order of <em>release</em> field.
 * <p>
 * WARN : Not consistent with <code>equals</code> and <code>hashCode</code>
 * 
 * @author LMN
 * 
 */
public class SEDbInstanceComparator implements Comparator<SEDbInstance> {

	private static final Logger LOG = LoggerFactory.getLogger(SEDbInstanceComparator.class);

	public int compare(final SEDbInstance instance1, final SEDbInstance instance2) {

		if (instance1 == null) {
			throw new IllegalArgumentException("Instance1 is null");
		}

		if (instance2 == null) {
			throw new IllegalArgumentException("Instance2 is null");
		}

		final String release1 = instance1.getRelease();// Should not be null
		final String release2 = instance2.getRelease();// Should not be null

		final int releaseOrder = release1.compareTo(release2);

		final Timestamp time1 = instance1.getSourceLastModifiedTime();// Should not be null
		final Timestamp time2 = instance2.getSourceLastModifiedTime();// Should not be null

		final int timeOrder = time1.compareTo(time2);

		if (signum(releaseOrder) != signum(timeOrder)) {
			LOG.warn("Inconsistent SEDbInstance order 1.[{}] 2.[{}]  1.{} 2.{}", release1, release2, time1,
				time2);
		}

		return releaseOrder;
	}

	private static int signum(final int value) {
		int result = 0;

		if (value > 0) {
			result = 1;
		} else if (value < 0) {
			result = -1;
		}

		return result;
	}

}
