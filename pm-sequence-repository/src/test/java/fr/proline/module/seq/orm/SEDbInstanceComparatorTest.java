package fr.proline.module.seq.orm;

import static org.junit.Assert.*;

import java.sql.Timestamp;

import org.junit.Test;

import fr.proline.module.seq.orm.SEDbInstance;
import fr.proline.module.seq.orm.SEDbInstanceComparator;

public class SEDbInstanceComparatorTest {

	@Test
	public void testCompare() {
		final long now = System.currentTimeMillis();

		SEDbInstance instance1 = new SEDbInstance();
		instance1.setRelease("A");
		instance1.setSourceLastModifiedTime(new Timestamp(now));

		SEDbInstance instance2 = new SEDbInstance();
		instance2.setRelease("B");
		instance2.setSourceLastModifiedTime(new Timestamp(now + 1));

		final SEDbInstanceComparator comparator = new SEDbInstanceComparator();

		assertTrue("1 < 2", comparator.compare(instance1, instance2) < 0);
		assertTrue("2 > 1", comparator.compare(instance2, instance1) > 0);

		instance2.setSourceLastModifiedTime(new Timestamp(now));

		assertTrue("1 < 2 with incosistent date", comparator.compare(instance1, instance2) < 0);

	}

}
