package fr.proline.module.seq.util;

import org.junit.Assert;
import org.junit.Test;

public class HashTest {

	@Test
	public void testHash() {
		doTest("ADDRADLAKYICENQDSISSKLKECCE");
		doTest("VNFEDNDNRSVLKGGPFSDSYRLFQFHFHWGSTNEHGSEH");
	}

	private static void doTest(final String sequence) {
		final String hash = HashUtil.calculateSHA256(sequence);
		Assert.assertNotNull(hash);
		Assert.assertEquals(64, hash.length());
		System.out.println(hash);
	}

}
