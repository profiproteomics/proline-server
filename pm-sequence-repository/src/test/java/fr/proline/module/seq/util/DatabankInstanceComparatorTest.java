package fr.proline.module.seq.util;

import static org.junit.Assert.*;

import java.sql.Timestamp;

import fr.proline.module.seq.orm.DatabankInstance;
import fr.proline.module.seq.util.DatabankInstanceComparator;
import org.junit.Test;

public class DatabankInstanceComparatorTest {

	@Test
	public void testCompare() {
		final long now = System.currentTimeMillis();

		DatabankInstance instance1 = new DatabankInstance();
		instance1.setRelease("A");
		instance1.setSourceLastModifiedTime(new Timestamp(now));

		DatabankInstance instance2 = new DatabankInstance();
		instance2.setRelease("B");
		instance2.setSourceLastModifiedTime(new Timestamp(now + 1));

		final DatabankInstanceComparator comparator = new DatabankInstanceComparator();

		assertTrue("1 < 2", comparator.compare(instance1, instance2) < 0);
		assertTrue("2 > 1", comparator.compare(instance2, instance1) > 0);

		instance2.setSourceLastModifiedTime(new Timestamp(now));

		assertTrue("1 < 2 with inconsistent date", comparator.compare(instance1, instance2) < 0);

	}

}
